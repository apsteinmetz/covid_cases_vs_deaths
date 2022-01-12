# show scaled deaths and cases
# correlate deaths and cases by state
library(tidyverse)
library(COVID19)
library(lubridate)


# devtools::install_github("covid19R/covid19nytimes")
us_states_raw <- COVID19::covid19(country="US",level=2,verbose=FALSE)



cutoff_start <- as.Date("2020-06-15") 
cutoff_end <- min(max(us_states_raw$date),Sys.Date())-1
cutoff_old_max <- as.Date("2021-11-01")

# # Remove tiny territories
territories <- c("Guam", "Northern Mariana Islands","American Samoa")

us_states <- us_states_raw %>% 
  as_tibble() %>% 
  filter(date >= cutoff_start) %>% 
  filter(date <= cutoff_end) %>% 
  rename(location = administrative_area_level_2) %>% 
  filter(!(location %in% territories)) %>% 
  rename(deaths_total = deaths) %>% 
  rename(cases_total = confirmed) %>% 
  rename(state = location) %>%
  select(date, state, cases_total, deaths_total) %>%
  mutate(state = as_factor(state)) %>%
  arrange(state, date) %>%
  group_by(state) %>%
  # smooth the data with prior 7 days
  mutate(cases_7day = (cases_total - lag(cases_total, 7)) / 7) %>%
  mutate(deaths_7day = (deaths_total - lag(deaths_total, 7)) / 7)
# ----------------------------------------------
# find max point prior to current surge

get_old_max <- function(dataset,type = c("cases","deaths")){
  dataset <- dataset %>% filter(date < cutoff_old_max)
  series <- switch(type,
                   cases = dataset$cases_7day,
                   deaths = dataset$deaths_7day)

  old_max <- max(series,na.rm = T)
  return(old_max)
}

# create lags
lag_period = 21

# get peaks based on early waves
state_peaks <- us_states %>% 
  filter(date < cutoff_old_max) %>% 
  group_by(state) %>% 
  summarise(old_max_cases=max(cases_7day,na.rm = T),
            old_max_deaths=max(deaths_7day,na.rm = T))

# pad future dates equal to lag and add peaks
us_states_lag <- us_states %>% 
  complete(date = seq.Date(min(date), 
                           max(date)+lag_period, by="day")) %>% 
  mutate(lag_cases_7day=lag(cases_7day,lag_period)) %>% 
  mutate(deaths_7day =  ifelse(deaths_7day==0,NA,deaths_7day)) %>% 
  left_join(state_peaks,by="state")

# scale and make long
us_states_long <- us_states_lag %>% 
  group_by(state) %>% 
  mutate(scaled_cases=cases_7day/old_max_cases) %>% 
  mutate(scaled_deaths=deaths_7day/old_max_deaths) %>% 
  mutate(lag_scaled_cases=lag_cases_7day/old_max_cases) %>% 
  select(date,state, lag_scaled_cases,scaled_deaths) %>% 
  pivot_longer(cols = contains("scaled"),
               names_to="type")


#plot scaled and lagged count for selected stated
states = c("New York")
gg1 <- us_states_long %>%
  filter(state %in% states) %>%
  filter(type =="scaled_deaths") %>%
  ggplot(aes(date, value, color = type)) +
  geom_line(size = 1.2) +
  scale_color_manual(values=c("black"),
                     labels = c("7-Day Avg. of Deaths on Date")) + 
  facet_grid(cols=vars(state),scales = "free") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "What Does This Mean?",
    y = "Percent of Last Years Peak",
    x = "",
    caption = "Source: covid19datahub.io, Arthur Steinmetz"
  ) +
  theme_light() +
  theme(legend.title = element_blank(), legend.position = "top")

states = c("New York")
gg2 <- us_states_long %>%
  filter(state %in% states) %>%
  ggplot(aes(date, value, color = type)) +
  geom_line(size = 1.2) +
  scale_color_manual(values=c("tomato2","black"),
                     labels = c(glue::glue("Cases {lag_period} Days After Date"),
                              "7-Day Avg. of Deaths on Date")) + 
  facet_grid(cols=vars(state),scales = "free") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "It Means This",
    subtitle = glue::glue("Deaths Follow Cases.  Will The Relationship Hold?"),
    y = "Percent of Last Years Peak",
    x = "",
    caption = "Source: covid19datahub.io, Arthur Steinmetz"
  ) +
  theme_light() +
  theme(legend.title = element_blank(), legend.position = "top")

cowplot::plot_grid(gg1,gg2,ncol = 1)

# Now show National level results ----------------------
# aggregate state to national
us_lag <- us_states_lag %>%
  group_by(date) %>%
  summarize(across(
    .cols = where(is.numeric),
    .fns = function(x) sum(x, na.rm = T),
    .names = "{.col}"
  ))  %>% 
  mutate(deaths_7day=ifelse(deaths_7day==0,NA,deaths_7day))

#scale US to % of peak
old_max_deaths <- get_old_max(us_lag,"deaths")
old_max_cases <- get_old_max(us_lag,"cases")
us_long <- us_lag %>% 
  mutate(scaled_cases=cases_7day/old_max_cases) %>% 
  mutate(scaled_deaths=deaths_7day/old_max_deaths) %>% 
  mutate(lag_scaled_cases=lag_cases_7day/old_max_cases) %>% 
  select(date,lag_scaled_cases,scaled_deaths) %>% 
  pivot_longer(cols = contains("scaled"),
               names_to="type")

#plot scaled and lagged national count
us_long %>% 
  ggplot(aes(date,value,color=type)) + geom_line(size=1.2) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values=c("tomato2","black"),
                     labels = c(glue::glue("Cases Pushed Forward {lag_period} Days"),
                                "7-Day Avg. of Deaths on Date")) + 
  labs(title="Covid Cases and Deaths in U.S.",
       subtitle = glue::glue("Cases Led by {lag_period} Days"),
       y="Percent of Last Years Peak",
       x = "Date",
       caption = "Source:covid19datahub.io,Arthur Steinmetz")+
  theme_light() + 
  theme(legend.title = element_blank(),legend.position = "top")
  
