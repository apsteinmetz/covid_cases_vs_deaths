#scratchpad
library(dplyr)
library(ggplot2)
devtools::install_github("RamiKrispin/coronavirus")
library(coronavirus)

load("c:/users/Arthur/downloads/coronavirus.rda")
pop <- readr::read_csv("data/demographic-2019-UNFPA.csv") %>% 
  rename(region = `Countries and areas`) %>% 
  rename(population = 'Total population in millions, 2019') %>% 
  select(region,population) %>% 
  mutate(population= population * 1e6)

cv_mod <- coronavirus %>%   
  dplyr::mutate(region = dplyr::if_else(Country.Region == "United Arab Emirates", "UAE", Country.Region)) %>%
  dplyr::mutate(region = dplyr::if_else(Country.Region == "China" & Province.State != "Hubei", "China ex Hubei", region)) %>%
  dplyr::mutate(region = dplyr::if_else(Country.Region == "China" & Province.State == "Hubei", "Hubei, China", region)) %>%
  dplyr::mutate(region = dplyr::if_else(Province.State == "Hong Kong", "Hong Kong", region)) %>%
  #  dplyr::mutate(region = dplyr::if_else(region == "Republic of Korea", "Korea (South)", region)) %>%
  dplyr::mutate(region = dplyr::if_else(region == "North Macedonia", "N.Macedonia", region)) %>%
  dplyr::mutate(region = dplyr::if_else(region == "Iran (Islamic Republic of", "Iran", region)) %>%
  dplyr::mutate(region = trimws(region)) %>%
  tidyr::as_tibble() %>%
  {.}


#make cumulative cases
cum_cases <- cv_mod %>%
  filter(type == "confirmed") %>% 
  select(-Province.State) %>% 
  group_by(region, date) %>% 
  summarise(cases = sum(cases)) %>% 
  mutate(cum_confirmed = cumsum(cases)) %>% 
  filter(cum_confirmed > 99) %>% 
  group_by(region) %>% 
  mutate(days_since_100 = as.numeric(date-min(date))) %>% 
  left_join(pop) %>% 
  mutate(cum_confirmed_per100k = cum_confirmed/population*100000)
  {.}


countries_of_interest = c("Australia","Belgium","Denmark","France","Germany", "China ex Hubei","Hubei, China",
                          "Hong Kong","Iran","Italy","Korea, South","United Kingdom","Singapore","Japan", "US")

countries_of_interest = c("Australia","Belgium","Denmark","France","Germany", "China ex Hubei",
                          "Hong Kong","Iran","Italy","Korea, South","United Kingdom","Singapore","Japan", "US")
# cum_cases %>% 
#   # filter smaller, non-asian and china
#   filter(region %in% countries_of_interest) %>% 
#   ggplot(aes(days_since_100,cum_confirmed,color=region)) + 
#   geom_line() + 
#   scale_y_log10() + 
#   NULL


# Plotting the data
cum_cases_wide <- cum_cases %>% 
  # filter smaller, non-asian and china
  filter(region %in% countries_of_interest) %>% 
  tidyr::pivot_wider(id_cols="days_since_100",names_from = "region",values_from = "cum_confirmed")

# constrain the chart a bit
cum_cases_wide <- cum_cases_wide %>% filter(days_since_100 < 31)

plot_object <- cum_cases_wide %>% 
  plotly::plot_ly(x = ~days_since_100,y = ~US,type = "scatter",mode="lines+markers",name="US") %>% 
#  plotly::layout(yaxis = list(type = "log")) %>% 
  {.}

countries_of_interest[which(countries_of_interest != "US")] %>% 
  purrr::walk( function(region) {
    print(region)
    plot_object <<- plot_object %>%
      plotly::add_trace(x = ~days_since_100,
                        y =  cum_cases_wide[[region]], 
                        # type = "scatter", 
                        mode = "lines",
                        name = region)
  })

  plot_object %>% plotly::layout(title = "",
                 legend = list(x = 0.9, y = 0.7),
                 yaxis = list(title = "Cumulative Confirmed Cases"),
                 xaxis = list(title = "Days Since 100 Cases"),
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

  # one day jumps
  
temp <-  cv_mod %>% group_by(region,type) %>% mutate(d_o_d_change = cases-lag(cases))
  
