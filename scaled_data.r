# show scaled deaths and cases
# correlate deaths and cases by state
library(tidyverse)
library(COVID19)
library(lubridate)


# devtools::install_github("covid19R/covid19nytimes")
us_states_raw <- COVID19::covid19(country="US",level=2,verbose=FALSE)



cutoff_start <- as.Date("2020-06-15") 
cutoff_end <- min(max(us_states_raw$date),Sys.Date()-1)

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
  mutate(deaths_7day = (deaths_total - lag(deaths_total, 7)) / 7)%>%
  # smooth the data with a centered mean of 7 days
  mutate(cases_1day = (cases_total - lag(cases_total, 1))) %>%
  mutate(deaths_1day = (deaths_total - lag(deaths_total, 1)))
# ----------------------------------------------
# aggregate state to national
us <- us_states %>%
  group_by(date) %>%
  summarize(across(
    .cols = where(is.numeric),
    .fns = function(x) sum(x, na.rm = T),
    .names = "{.col}"
  ))

# find max point prior to current surge
old_max_cases <- us %>% 
  filter(date<as.Date("2021-11-01")) %>% 
  summarise(max_cases_7day=max(cases_7day)) %>% pull()
  
old_max_deaths <- us %>% 
  filter(date<as.Date("2021-11-01")) %>% 
  summarise(max_deaths_7day=max(deaths_7day)) %>% pull()


# create lags
lag_period = 21

# pad future dates equal to lag
us <- us %>% 
  complete(date = seq.Date(min(date), 
                           max(date)+lag_period, by="day")) %>% 
  mutate(lag_cases_7day=lag(cases_7day,lag_period))

#scale to % of peak
us <- us %>% 
  mutate(scaled_cases=cases_7day/old_max_cases) %>% 
  mutate(scaled_deaths=deaths_7day/old_max_deaths) %>% 
  mutate(lag_scaled_cases=lag_cases_7day/old_max_cases)

#pivot to long
us_long <- us %>% 
  select(date,lag_scaled_cases,scaled_deaths) %>% 
  pivot_longer(cols = contains("scaled"),
               names_to="type")

us_long %>% 
  ggplot(aes(date,value,color=type)) + geom_line() +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = c("red","darkgray")) +
  labs(title="Covid Cases and Deaths in U.S.",
       subtitle = "Cases lagged by 3 Weeks",
       y="Percent of Last Years Peak",
       caption = "Source:covid19datahub.io,Arthur Steinmetz")+
  theme_light() + 
  theme(legend.title = element_blank(),legend.position = "top")
  