# vaccines vs covid
# using Johns Hopkins data
library(tidyverse)
library(coronavirus)
library(covid19nytimes)
library(RSocrata)

# source https://github.com/nytimes/covid-19-data.git
us_states_long <- covid19nytimes::refresh_covid19nytimes_states()
covid19_df <- refresh_coronavirus_jhu()


# get 2020 prez vote totals
# ----------------------------------
PopVote2020 <- read_csv("data/PopVote2020.csv")

# get population by state
# ------------------------------------
# https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-total.html#par_textimage
state_pop <- read_csv("~/R Projects/covid_cases_vs_deaths/data/nst-est2019-alldata.csv") %>% 
   transmute(state=NAME,pop=POPESTIMATE2019) %>% 
   right_join(data.frame(state=state.name,state.abb=state.abb)) %>% 
   left_join(PopVote2020) %>% 
   mutate(repub_percent_2020=as.numeric(str_remove(rep_percent,"%"))) %>% 
   select(state,pop,state.abb,repub_percent_2020)

# get vaccination info
# ------------------------------------
# https://covid.cdc.gov/covid-data-tracker/#vaccinations

raw_vax <- as_tibble(read.socrata("https://data.cdc.gov/resource/unsk-b7fc.json"))

# raw_vax <- read_csv("https://data.cdc.gov/api/views/unsk-b7fc/rows.csv?accessType=DOWNLOAD")


print("Using pct of 12+ population")
vax_pct <- raw_vax %>% 
   mutate(across(4:ncol(raw_vax),as.double)) %>% 
   # mutate(Date = as.Date(Date,format="%m/%d/%Y")) %>% 
   filter(date == max(date)) %>% 
   rename(state.abb = location,pct_full_vax = series_complete_12pluspop) %>% 
   right_join(state_pop) %>% 
   select(date,state,state.abb,pct_full_vax) %>% 
   {.}
print(max(vax_pct$date))

# ------------------------------------

# to get change over time use lag2 day ago
lag2 = 1 * 7
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
   mutate(cases_7day_old = (lag(cases_total, lag2)-lag(cases_total, lag2+7)) / 7) %>%
   mutate(deaths_7day_old = (lag(deaths_total, lag2)-lag(deaths_total, lag2+7)) / 7)%>% 
   filter(date == max(date))



vax_effect <- right_join(vax_pct,us_states) %>%
   right_join(state_pop) %>% 
   select(state,state.abb,pct_full_vax,deaths_7day,cases_7day,pop,repub_percent_2020) %>% 
   mutate(pct_unvaxed=100-pct_full_vax) %>% 
   mutate(deaths_per_million=deaths_7day/pop*1000000,cases_per_million=cases_7day/pop*1000000) %>% 
   {.}

vax_effect %>% ggplot(aes(pct_unvaxed,deaths_per_million)) + 
   #   geom_point() + 
   geom_smooth(method = "gam",se = FALSE) + 
   geom_text(aes(label=state.abb)) +
   labs(title = "Fewer Vaxed, More Dying",
        subtitle = paste("New Deaths in Week Ending",max(us_states$date)),
        x = "Percent of Age 12+ Population Unvaccinated",
        caption = "Sources: Johns Hopkins, CDC, Census Bureau")

vax_effect %>% ggplot(aes(pct_unvaxed,cases_per_million)) + 
   #   geom_point() + 
   geom_smooth(method = "gam",se = FALSE) + 
   geom_text(aes(label=state.abb)) +
   labs(title = "Fewer Vaxed, More Cases",
        x = "Percent of Age 12+ Population Unvaccinated",
        subtitle = paste("New Case in Week Ending",max(us_states$date)),
        caption = "Sources: Johns Hopkins, CDC. Census Bureau")

vax_effect %>% ggplot(aes(repub_percent_2020,cases_per_million)) + 
   #   geom_point() + 
   geom_smooth(method = "gam",se = FALSE) + 
   geom_text(aes(label=state.abb)) +
   labs(title = "Fewer Vaxed, More Republican",
        x = "Republican Share of Vote in 2020",
        subtitle = paste("New Case in Week Ending",max(us_states$date)),
        caption = "Sources: Johns Hopkins, CDC. Census Bureau, Cook Political Report")

