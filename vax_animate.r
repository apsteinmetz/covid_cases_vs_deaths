# animate changes in vax rates and cases/deaths
# vaccines vs covid
# using Johns Hopkins data
library(tidyverse)
library(coronavirus)
library(covid19nytimes)
library(RSocrata)
library(gganimate)
library(lubridate)

# get population by state
# ------------------------------------
# https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-total.html#par_textimage
state_pop <- read_csv("data/nst-est2019-alldata.csv") %>% 
  transmute(state=NAME,pop=POPESTIMATE2019) %>% 
  right_join(data.frame(state=state.name,state.abb=state.abb))

"https://www.census.gov/data/tables/time-series/demo/popest/2010s-counties-total.html#par_textimage"
county_pop <- read_csv("data/co-est2019-alldata.csv") %>%
  transmute(state=STNAME,county = CTYNAME,pop=POPESTIMATE2019) %>% 
  mutate(county = str_remove(county," County| Parish")) %>% 
  {.}

# get vaccination info
# ------------------------------------
# https://covid.cdc.gov/covid-data-tracker/#vaccinations

REFRESH_VAX_DATA <- FALSE

us_states_long <- covid19nytimes::refresh_covid19nytimes_states()
covid19_df <- refresh_coronavirus_jhu()
raw_vax <- as_tibble(read.socrata("https://data.cdc.gov/resource/unsk-b7fc.json"))

# Days lag to use for changes in values over time
lag2 = 7

# ---------------------------------------------------
# Process data
vax_pct <- raw_vax %>% 
  mutate(across(4:ncol(raw_vax),as.double)) %>% 
  rename(state.abb = location,pct_full_vax = series_complete_12pluspop) %>% 
  group_by(state.abb) %>% 
  arrange(state.abb,date) %>% 
  mutate(pct_full_vax_prior = lag(pct_full_vax,lag2)) %>% 
  mutate(admin_per_100k_prior = lag(admin_per_100k,lag2)) %>% 
  mutate(date = as.Date(date)) %>% 
  right_join(state_pop) %>% 
  select(date,state,state.abb,pct_full_vax,pct_full_vax_prior,admin_per_100k,admin_per_100k_prior)


# to get change over time use lag2 days ago
us_states <- us_states_long %>%
  # discard dates before vaxing was reported.
  filter(date > as.Date("2021-05-16")) %>%
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
  right_join(state_pop) %>% 
  mutate(deaths_per_million=deaths_7day/pop*1000000,
         cases_per_million=cases_7day/pop*1000000) %>% 
  mutate(deaths_per_million_prior=deaths_7day_prior/pop*1000000,
         cases_per_million_prior=cases_7day_prior/pop*1000000) %>% 
  mutate(deaths_total_per_million=deaths_total/pop*1000000,
         cases_total_per_million=cases_total/pop*1000000) %>% 
  mutate(deaths_last90_per_million=deaths_last90/pop*1000000) %>% 
  mutate(cases_arrow_color = ifelse(cases_per_million-cases_per_million_prior>0,"red",rgb(0.2,0.7,0.1,0.5))) %>% 
  mutate(deaths_arrow_color = ifelse(deaths_per_million-deaths_per_million_prior>0,"red",rgb(0.2,0.7,0.1,0.5)))

# oops bad data for Idaho so take it out for now
us_states <- us_states %>% filter(state != "Idaho")

vax_effect <- right_join(vax_pct,us_states) %>%
  mutate(pct_unvaxed=100-pct_full_vax) %>% 
  mutate(pct_unvaxed_prior=100-pct_full_vax_prior) %>% 
  {.}

# reduce to weekly data
vax_effect_wk <- vax_effect %>% 
  mutate(week = ceiling_date(date,unit="weeks", change_on_boundary = FALSE)) %>% 
  select(date,week,everything()) %>% 
  filter(date == week) %>% 
  select(-date)

# Plot data by State
# ---------------------------------------------------
p <- vax_effect_wk %>% ggplot(aes(pct_full_vax,cases_per_million,label=state.abb)) + 
  #geom_point(aes(color=state,size=cases_total_per_million)) + 
  geom_text(aes(color=state,size=cases_total_per_million,label=state.abb)) +
  theme(legend.position = "none") + 
  scale_y_continuous(limits = c(0,1250)) + 
  labs(title = "The Race between Delta and Vaccine",
       x = "Percent of Age 12+ Population Fully Vaxed",
       y = "Average New Daily Cases Per Million",
       subtitle = paste("New Cases in Week Ending {closest_state}"),
       caption = "Sources: Johns Hopkins, CDC. Census Bureau")

p
anim <- p + transition_states(week)
anim  

p <- vax_effect_wk %>% ggplot(aes(pct_full_vax,deaths_per_million,label=state.abb)) + 
  #geom_point(aes(color=state,size=cases_total_per_million)) + 
  geom_text(aes(color=state,size=deaths_total_per_million,label=state.abb)) +
  theme(legend.position = "none") + 
  # scale_y_continuous(limits = c(0,1250)) + 
  labs(title = "The Race between Delta and Vaccine",
       x = "Percent of Age 12+ Population  Not Fully Vaxed",
       y = "Average New Daily Deaths Per Million",
       subtitle = paste("New Deaths in Week Ending {closest_state}"),
       caption = "Sources: Johns Hopkins, CDC. Census Bureau")

p
anim <- p + transition_states(week,wrap = FALSE)
anim  
