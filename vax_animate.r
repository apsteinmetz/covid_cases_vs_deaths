# vaccines vs covid
# using Johns Hopkins data
library(tidyverse)
library(coronavirus)
library(covid19nytimes)
library(RSocrata)
library(ggpubr)
library(lubridate)
library(gganimate)



# source https://github.com/nytimes/covid-19-data.git
us_states_long <- covid19nytimes::refresh_covid19nytimes_states()
covid19_df <- refresh_coronavirus_jhu()


# get population by state
# ------------------------------------
# https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-total.html#par_textimage
state_pop <- read_csv("data/nst-est2019-alldata.csv") %>% 
  transmute(state=NAME,pop=POPESTIMATE2019) %>% 
  right_join(data.frame(state=state.name,state.abb=state.abb))


# get vaccination info
# ------------------------------------
# https://covid.cdc.gov/covid-data-tracker/#vaccinations

# by state
# takes a while so save data
# raw_vax <- as_tibble(read.socrata("https://data.cdc.gov/resource/unsk-b7fc.json"))
# write_csv(raw_vax,file="data/raw_vax.csv")
raw_vax <- read_csv("data/raw_vax.csv")

# Process data
# ---------------------------------------------------
# Days lag to use for changes in values over time
lag2 = 7

print("Using pct of 12+ population")
vax_pct <- raw_vax %>% 
  mutate(across(4:ncol(raw_vax),as.double)) %>% 
  rename(state.abb = location,pct_full_vax = series_complete_12pluspop) %>% 
  group_by(state.abb) %>% 
  arrange(state.abb,date) %>% 
  mutate(pct_full_vax_prior = lag(pct_full_vax,lag2)) %>% 
  mutate(admin_per_100k_prior = lag(admin_per_100k,lag2)) %>% 
  mutate(date = as.Date(date)) %>% 
  # filter(date == max(date)) %>% 
  right_join(state_pop) %>% 
  select(date,state,state.abb,pct_full_vax,pct_full_vax_prior,admin_per_100k,admin_per_100k_prior) %>% 
  # select(date,state.abb,pct_full_vax,pct_full_vax_prior)
  {.}
# print(max(vax_pct$date))


# to get change over time use lag2 day ago
us_states <- us_states_long %>%
  # discard dates before cases were tracked.
  filter(date > as.Date("2020-03-01")) %>%
  pivot_wider(names_from = "data_type", values_from = "value") %>%
  rename(state = location) %>%
  select(date, state, cases_total, deaths_total) %>%
  mutate(state = as_factor(state)) %>%
  arrange(state, date) %>%
  group_by(state) %>%
  # smooth the data with prior 7 days
  mutate(cases_7day = (cases_total - lag(cases_total, 7)) / 7) %>%
  mutate(deaths_7day = (deaths_total - lag(deaths_total, 7)) / 7)%>% 
  mutate(cases_7day_prior = (lag(cases_total, lag2)-lag(cases_total, lag2+7)) / 7) %>%
  mutate(deaths_7day_prior = (lag(deaths_total, lag2)-lag(deaths_total, lag2+7)) / 7)%>% 
  mutate(deaths_last90 = deaths_total-lag(deaths_total,90)) %>% 
  # filter(date == max(date)) %>% 
  right_join(state_pop) %>% 
  mutate(deaths_per_million=deaths_7day/pop*1000000,cases_per_million=cases_7day/pop*1000000) %>% 
  mutate(deaths_per_million_prior=deaths_7day_prior/pop*1000000,cases_per_million_prior=cases_7day_prior/pop*1000000) %>% 
#
    mutate(deaths_per_million_prior=deaths_7day_prior/pop*1000000,cases_per_million_prior=cases_7day_prior/pop*1000000) %>% 
  mutate(deaths_per_million_prior=deaths_7day_prior/pop*1000000,cases_per_million_prior=cases_7day_prior/pop*1000000) %>% 
#
    mutate(deaths_last90_per_million=deaths_last90/pop*1000000) %>% 
  mutate(cases_arrow_color = ifelse(cases_per_million-cases_per_million_prior>0,"red",rgb(0.2,0.7,0.1,0.5))) %>% 
  mutate(deaths_arrow_color = ifelse(deaths_per_million-deaths_per_million_prior>0,"red",rgb(0.2,0.7,0.1,0.5))) %>% 
  {.}

# oops bad data for Idaho so take it out for now
# us_states <- us_states %>% filter(state != "Idaho")

vax_effect <- right_join(vax_pct,us_states) %>%
  mutate(pct_unvaxed=100-pct_full_vax) %>% 
  mutate(pct_unvaxed_prior=100-pct_full_vax_prior) %>%
  filter(date > as.Date("2021-05-16")) %>% 
  {.}

vax_effect_wk <- vax_effect %>% 
  mutate(week = floor_date(date,unit="week")) %>% 
  filter(date == week) %>% 
  filter(!is.na(pct_full_vax)) %>% 
  select(week,everything(),-date)

# Plot data
# ---------------------------------------------------
p <- vax_effect_wk %>% 
  ggplot(aes(pct_unvaxed,cases_per_million,color=state,size=cases_total)) + 
  #   geom_point() + 
  # geom_smooth(method = "gam",se = FALSE) + 
  geom_text(aes(label=state.abb)) +
  scale_x_continuous(limits = c(20,80)) +
  scale_y_continuous(limits = c(0,1250)) +
  theme(legend.position = "none") +
  labs(title = "Fewer Vaxed, More Cases",
       x = "Percent of Age 12+ Population  Not Fully Vaxed",
       subtitle = paste("New Case in Week Ending",max(us_states$date)),
       caption = "Sources: Johns Hopkins, CDC. Census Bureau")

p
anim <- p +
  transition_states(week,
                    transition_length = 2,
                    state_length = 1) +
  shadow_wake(wake_length = .2,wrap = FALSE,alpha = 0.5)

  
anim
  
vax_effect %>% ggplot(aes(pct_unvaxed,deaths_per_million)) + 
  geom_smooth(method = "gam",se = FALSE) + 
  geom_text(aes(label=state.abb)) +
  labs(title = "More Vaxed, Less Dying",
       subtitle = paste("New Deaths in Two Weeks Ending",max(us_states$date)),
       x = "Percent of Age 12+ Population Not Fully Vaxed",
       y = "Deaths per Million Residents",
       caption = "Sources: Johns Hopkins, CDC, Census Bureau")

