#scratchpad
library(dplyr)
library(ggplot2)
#devtools::install_github("RamiKrispin/coronavirus")
#library(coronavirus)
#data(coronavirus)
#load("c:/users/Arthur/downloads/coronavirus.rda")

# get johns hopkins data into a tibble called cv_mod
source("load_JH_data.r")

# pop <- readr::read_csv("data/demographic-2019-UNFPA.csv") %>% 
#   rename(region = `Countries and areas`) %>% 
#   rename(population = 'Total population in millions, 2019') %>% 
#   select(region,population) %>% 
#   mutate(population= population * 1e6)

# choose type
type_select ="confirmed"
# case_threshold = 10
# yaxis_type = "log"

countries_of_interest = c("Australia","Belgium","Denmark","France","Germany", "China ex Hubei",
                          "Hong Kong","Iran","Italy","Korea, South","United Kingdom","Singapore","Japan", "US")

countries_of_interest_small = c("France", "China ex Hubei","Iran","Italy","Spain",
                                "Korea, South","United Kingdom","Japan", "US","Germany")

countries_of_interest_recoveries <- cv_mod %>% 
  filter(type == "Recovered",cases >200 ) %>% 
  select(region) %>% 
  unique() %>% 
  pull()

# ----------------------------------------
# just for testing
countries <- countries_of_interest
case_threshold = 100
type_select <- "confirmed"
cum_cases <- cv_mod %>%
  filter(region %in% countries) %>% 
  filter(type == type_select) %>% 
  group_by(region, date) %>% 
  summarise(cases = sum(cases)) %>% 
  #    mutate(cum_cases = cumsum(cases)) %>%
  # cases from JH data are already cumulative
  mutate(cum_cases = cases) %>%
  filter(cum_cases > case_threshold) %>%
  group_by(region) %>%
  mutate(days_since = as.numeric(date-min(date))) %>%
  # left_join(pop) %>%
  # mutate(cum_cases_per100k = cum_cases/population*100000)
  {.}
# ----------------------------------------
doubling_amt <- function(initial=1,time=1,doubling_time=3){
  return (initial *2 ^(time/doubling_time))
}
# ----------------------------------------

plot_cum_trend <- function(regions = "US",
                           type_select = "confirmed",
                           case_threshold = 10,
                           scale_type = "log", #could be linear
                           max_days = Inf){
  # Plotting the data
  #make cumulative cases
  cum_cases <- cv_mod %>%
    filter(region %in% regions) %>% 
    filter(type == type_select) %>% 
    group_by(region, date) %>% 
    summarise(cases = sum(cases)) %>% 
    #    mutate(cum_cases = cumsum(cases)) %>%
    # cases from JH data are already cumulative
    mutate(cum_cases = cases) %>%
    filter(cum_cases > case_threshold) %>%
    group_by(region) %>%
    mutate(days_since = as.numeric(date-min(date))) %>%
    # left_join(pop) %>%
    # mutate(cum_cases_per100k = cum_cases/population*100000)
    {.}

    cum_cases_wide <- cum_cases %>% 
    tidyr::pivot_wider(id_cols="days_since",names_from = "region",values_from = "cum_cases")
  
  # constrain the chart a bit
  cum_cases_wide <- cum_cases_wide %>% filter(days_since < max_days + 1)
  
  plot_object <- cum_cases_wide %>% 
    plotly::plot_ly() %>% 
    plotly::layout(yaxis = list(type = scale_type))
  # make US stand out with markers and lines
  if ("US" %in% countries){
    plot_object <- plot_object %>%  
      plotly::add_trace(x = ~days_since,y = ~US,type = "scatter",mode="lines+markers",name="US")
    
  } 
    
  regions[which(regions != "US")] %>% 
    purrr::walk( function(region) {
      print(region)
      plot_object <<- plot_object %>%
        plotly::add_trace(x = ~days_since,
                          y =  cum_cases_wide[[region]], 
                          type = "scatter", 
                          mode = "lines",
                          name = region)
    })
  # add a growth rate constant line
  plot_object <- plot_object %>% 
    plotly::add_trace(x = ~days_since,y = doubling_amt(case_threshold,1:(max(cum_cases_wide$days_since)+1)),
                      type = "scatter",mode="lines",line = list(dash="dash",color="black"),name="Doubling Time = 3 Days")
  
  # add a growth rate constant line
  plot_object <- plot_object %>% 
    plotly::add_trace(x = ~days_since,
                      y = doubling_amt(case_threshold,1:(max(cum_cases_wide$days_since)+1),doubling_time = 2),
                      type = "scatter",
                      mode="lines",
                      line = list(dash="dash",color="red"),
                      name="Doubling Time = 2 Days")

    plot_object %>% plotly::layout(title = "",
                                 legend = list(x = 0.9, y = 0.7),
                                 yaxis = list(title = paste0("Cumulative ",type_select," Cases")),
                                 xaxis = list(title = paste0("Days Since ",case_threshold," ",type_select)),
                                 # paper_bgcolor = "black",
                                 # plot_bgcolor = "black",
                                 # font = list(color = 'white'),
                                 hovermode = "compare",
                                 margin =  list(
                                   # l = 60,
                                   # r = 40,
                                   b = 10,
                                   t = 10,
                                   pad = 2
                                 ))
  
}

plot_cum_trend(countries_of_interest_small,"deaths",10,max_days = 30)
plot_cum_trend(countries_of_interest,"confirmed",100, max_days = 30)
# plot_cum_trend(countries_of_interest_recoveries,"Recovered",10)


# ----------------------------------------------------
# daily cases
daily_cases <- cv_mod %>% group_by(date,region,type) %>% mutate(daily_cases = cases - lag(cases))
