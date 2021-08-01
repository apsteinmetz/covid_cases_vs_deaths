# vaccines vs covid
# using Johns Hopkins data
library(tidyverse)
library(coronavirus)
library(covid19nytimes)
library(RSocrata)
library(ggpubr)
library(readr)



# source https://github.com/nytimes/covid-19-data.git
us_states_long <- covid19nytimes::refresh_covid19nytimes_states()
covid19_df <- refresh_coronavirus_jhu()


# get 2020 prez vote totals
# ----------------------------------
PopVote2020 <- read_csv("data/PopVote2020.csv")
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ
PopVote2020_county <- read_csv("data/countypres_2000-2020.csv") %>% 
   filter(year == "2020",party == "REPUBLICAN") %>% 
   group_by(state,county_name) %>% 
   summarise(repubvotes=sum(candidatevotes),totalvotes=sum(totalvotes)) %>% 
   mutate(repub_percent_2020=repubvotes/totalvotes) %>% 
   {.}
   
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
#raw_vax <- as_tibble(read.socrata("https://data.cdc.gov/resource/unsk-b7fc.json"))
write_csv(raw_vax,file="data/raw_vax.csv")
raw_vax <- read_csv("data/raw_vax.csv")

# by county
#raw_vax_county <- as_tibble(read.socrata(
#   "https://data.cdc.gov/resource/8xkx-amqh.json?$select=date, recip_county, recip_state, series_complete_12pluspop, series_complete_pop_pct"))
#write_csv(raw_vax_county,file="data/raw_vax_county.csv")
raw_vax_county <- read_csv("data/raw_vax_county.csv")

# Process data
# ---------------------------------------------------
# Days lag to use for changes in values over time
lag2 = 2 * 7

print("Using pct of 12+ population")
vax_pct <- raw_vax %>% 
   mutate(across(4:ncol(raw_vax),as.double)) %>% 
   rename(state.abb = location,pct_full_vax = series_complete_12pluspop) %>% 
   group_by(state.abb) %>% 
   arrange(state.abb,date) %>% 
   mutate(pct_full_vax_old = lag(pct_full_vax,lag2)) %>% 
   # mutate(Date = as.Date(Date,format="%m/%d/%Y")) %>% 
   filter(date == max(date)) %>% 
   right_join(state_pop) %>% 
   select(state,state.abb,pct_full_vax,pct_full_vax_old) %>% 
   # select(date,state.abb,pct_full_vax,pct_full_vax_old)
   {.}
# print(max(vax_pct$date))

vax_pct_county <- raw_vax_county %>% 
   # mutate(across(4:ncol(raw_vax),as.double)) %>% 
   rename(state.abb = recip_state,county = recip_county,pct_full_vax = series_complete_12pluspop) %>% 
   group_by(state.abb,county) %>% 
   arrange(state.abb,date) %>% 
   mutate(pct_full_vax_old = lag(pct_full_vax,lag2)) %>% 
   # mutate(Date = as.Date(Date,format="%m/%d/%Y")) %>% 
   filter(date == max(date)) %>% 
   right_join(state_pop) %>% 
   select(state,state.abb,pct_full_vax,pct_full_vax_old) %>% 
   # select(date,state.abb,pct_full_vax,pct_full_vax_old)
   {.}

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
   mutate(cases_7day_old = (lag(cases_total, lag2)-lag(cases_total, lag2+7)) / 7) %>%
   mutate(deaths_7day_old = (lag(deaths_total, lag2)-lag(deaths_total, lag2+7)) / 7)%>% 
   mutate(deaths_last90 = deaths_total-lag(deaths_total,90)) %>% 
   filter(date == max(date)) %>% 
   right_join(state_pop) %>% 
   mutate(deaths_per_million=deaths_7day/pop*1000000,cases_per_million=cases_7day/pop*1000000) %>% 
   mutate(deaths_per_million_old=deaths_7day_old/pop*1000000,cases_per_million_old=cases_7day_old/pop*1000000) %>% 
   mutate(deaths_last90_per_million=deaths_last90/pop*1000000) %>% 
   mutate(cases_arrow_color = ifelse(cases_per_million-cases_per_million_old>0,"red",rgb(0.2,0.7,0.1,0.5))) %>% 
   mutate(deaths_arrow_color = ifelse(deaths_per_million-deaths_per_million_old>0,"red",rgb(0.2,0.7,0.1,0.5))) %>% 
   {.}
   
# oops bad data for delaware on july 31 so take it out for now
us_states <- us_states %>% filter(state != "Delaware")

vax_effect <- right_join(vax_pct,us_states) %>%
   mutate(pct_unvaxed=100-pct_full_vax) %>% 
   mutate(pct_unvaxed_old=100-pct_full_vax_old) %>% 
   {.}


# Plot data
# ---------------------------------------------------
vax_effect %>% ggplot(aes(pct_unvaxed,cases_per_million)) + 
   #   geom_point() + 
   geom_smooth(method = "gam",se = FALSE) + 
   geom_text(aes(label=state.abb)) +
   labs(title = "Fewer Vaxed, More Cases",
        x = "Percent of Age 12+ Population Unvaccinated",
        subtitle = paste("New Case in Week Ending",max(us_states$date)),
        caption = "Sources: Johns Hopkins, CDC. Census Bureau")


vax_effect %>% ggplot(aes(pct_unvaxed,deaths_per_million)) + 
   geom_smooth(method = "gam",se = FALSE) + 
   geom_text(aes(label=state.abb)) +
   labs(title = "Fewer Vaxed, More Dying",
        subtitle = paste("New Deaths in Week Ending",max(us_states$date)),
        x = "Percent of Age 12+ Population Unvaccinated",
        caption = "Sources: Johns Hopkins, CDC, Census Bureau")


# Deltas
vax_effect %>% ggplot(aes(pct_unvaxed,cases_per_million)) + 
   geom_point(aes(x=pct_unvaxed_old, y=cases_per_million_old)) + 
   geom_segment(aes(x=pct_unvaxed_old, xend=pct_unvaxed, 
                    y=cases_per_million_old, yend=cases_per_million*0.98), 
                arrow = arrow(type="closed",angle=10,length = unit(0.2, "inches"),),
                color=vax_effect$cases_arrow_color) +
   geom_smooth(method = "gam",se = FALSE) + 
   geom_text(aes(label=state.abb)) +
   labs(title = "Fewer Vaxed, More Cases",
        subtitle = paste("New Cases and Change in New Cases For Week Ending",max(us_states$date)),
        x = "Percent of Age 12+ Population Unvaccinated",
        caption = "Sources: Johns Hopkins, CDC, Census Bureau")

vax_effect %>% ggplot(aes(pct_unvaxed,deaths_per_million)) + 
   geom_point(aes(x=pct_unvaxed_old,y=deaths_per_million_old)) + 
   geom_segment(aes(x=pct_unvaxed_old, xend=pct_unvaxed, 
                    y=deaths_per_million_old, yend=deaths_per_million*0.98), 
                arrow = arrow(type="closed",angle=10,length = unit(0.2, "inches"),),
                color=vax_effect$deaths_arrow_color) +
   geom_smooth(method = "gam",se = FALSE) + 
   geom_text(aes(label=state.abb)) +
   labs(title = "Fewer Vaxed, More deaths",
        subtitle = paste("New deaths and Change in New deaths For Week Ending",max(us_states$date)),
        x = "Percent of Age 12+ Population Unvaccinated",
        caption = "Sources: Johns Hopkins, CDC, Census Bureau")

# acceleration in vaccination
vax_effect %>% 
   mutate(vax_delta = pct_full_vax-pct_full_vax_old) %>% 
   ggplot(aes(fct_reorder(state.abb,vax_delta),vax_delta)) + 
   geom_col() +
   coord_flip() +
   labs(y="Change in Vaccinated Pop. (Percentage Points)",
        x = "")
   
# are low-vax states getting the message?
# change in vaccination
vax_effect %>% 
   mutate(vax_delta = pct_full_vax-pct_full_vax_old) %>% 
   ggplot(aes(pct_unvaxed,vax_delta)) + geom_point() + 
   labs(y="Change in Vaccinated Pop. (Percentage Points)",
        x = "Percent of Population Not Fully Vaxed")

# POLITICS
vax_politics <- vax_effect %>%
   left_join(PopVote2020) %>% 
   mutate(repub_percent_2020=as.numeric(str_remove(rep_percent,"%")))

vax_politics %>% 
   ggplot(aes(repub_percent_2020,pct_unvaxed)) + 
   #   geom_point() + 
   geom_smooth(method = "glm",se = FALSE) + 
   geom_text(aes(label=state.abb)) +
   labs(title = "More Republican, Less Vaxed",
        x = "Trump Share of Vote in 2020",
        y = "Percent of State Unvaccinated",
        subtitle = paste("As of",max(us_states$date)),
        caption = "Sources: Johns Hopkins, CDC. Census Bureau, Cook Political Report") +
   stat_cor()

vax_politics %>% 
   ggplot(aes(repub_percent_2020,cases_per_million)) + 
   #   geom_point() + 
   #geom_smooth(se = FALSE) + 
   geom_smooth(method = "glm",se = FALSE) + 
   geom_text(aes(label=state.abb)) +
   labs(title = "More Republican, More Cases",
        x = "Trump Share of Vote in 2020",
        y = "New Daily COVID Cases Per Million Residents",
        subtitle = paste("7-Day Average As of",max(us_states$date)),
        caption = "Sources: Johns Hopkins, CDC. Census Bureau, Cook Political Report") +
   stat_cor()

vax_politics %>% 
   ggplot(aes(repub_percent_2020,deaths_per_million)) + 
   #   geom_point() + 
   #geom_smooth(se = FALSE) + 
   geom_smooth(method = "glm",se = FALSE) + 
   geom_text(aes(label=state.abb)) +
   labs(title = "More Republican, More Deaths",
        x = "Trump Share of Vote in 2020",
        y = "Daily COVID Deaths Per Million Residents",
        subtitle = paste("7-Day Average As of",max(us_states$date)),
        caption = "Sources: Johns Hopkins, CDC. Census Bureau, Cook Political Report") +
   stat_cor()

#POLITICS BY COUNTY

not working
vax_politics_county <- PopVote2020_county %>% 
   transmute(state=str_to_title(state), county = str_to_title(county_name),repub_percent_2020) %>% 
   left_join(state_pop,by="state") %>% 
   left_join(raw_vax_county,by=c("state.abb","recip_state"))
   
raw_vax_county %>% 
   mutate(county = str_trim(str_remove(recip_county,"County"))) %>% 
   rename(state.abb = recip_state)
