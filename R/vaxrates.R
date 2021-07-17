# vaccines vs covid
# using Johns Hopkins data
library(tidyverse)
library(coronavirus)
library(covid19nytimes)

# source https://github.com/nytimes/covid-19-data.git
us_states_long <- covid19nytimes::refresh_covid19nytimes_states()
covid19_df <- refresh_coronavirus_jhu()

# https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-total.html#par_textimage
state_pop <- read_csv("~/R Projects/covid_cases_vs_deaths/data/nst-est2019-alldata.csv") %>% 
   transmute(state=NAME,pop=POPESTIMATE2019) %>% 
   right_join(data.frame(state=state.name,state.abb=state.abb))


library(readr)
# https://covid.cdc.gov/covid-data-tracker/#vaccinations
raw_data <- read_csv("~/R Projects/covid_cases_vs_deaths/data/temp.csv")
names(raw_data)[1] <- "state"
vax <- raw_data %>% mutate(state = str_remove(state," State")) %>% 
   filter(state %in% state.name) %>% 
   mutate(across(2:ncol(raw_data),as.double))

vax_pct <- vax[,c(1,16)]
names(vax_pct) <- c("state","pct_full_vax")

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
   # smooth the data with a centered mean of 7 days
   mutate(cases_1day = (cases_total - lag(cases_total, 1))) %>%
   mutate(deaths_1day = (deaths_total - lag(deaths_total, 1))) %>% 
   filter(date == max(date))



vax_effect <- right_join(vax_pct,us_states) %>%
   right_join(state_pop) %>% 
   select(state,state.abb,pct_full_vax,deaths_7day,cases_7day,pop) %>% 
   mutate(pct_unvaxed=100-pct_full_vax) %>% 
   mutate(deaths_per_million=deaths_7day/pop*1000000,cases_per_million=cases_7day/pop*1000000)

vax_effect %>% ggplot(aes(pct_unvaxed,deaths_per_million)) + 
   #   geom_point() + 
   geom_smooth(method = "gam",se = FALSE) + 
   geom_text(aes(label=state.abb)) +
   labs(title = "Fewer Vaxed, More Dying",
        subtitle = paste("New Deaths in Week Ending",max(us_states$date)),
        caption = "Sources: Johns Hopkins, CDC, Census Bureau")

vax_effect %>% ggplot(aes(pct_unvaxed,cases_per_million)) + 
   #   geom_point() + 
   geom_smooth(method = "gam",se = FALSE) + 
   geom_text(aes(label=state.abb)) +
   labs(title = "Fewer Vaxed, More Cases",
        subtitle = paste("New Case in Week Ending",max(us_states$date)),
        caption = "Sources: Johns Hopkins, CDC. Census Bureau")

