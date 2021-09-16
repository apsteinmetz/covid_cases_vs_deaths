# vaccines vs covid
# using Johns Hopkins data
library(tidyverse)
library(coronavirus)
library(covid19nytimes)
library(RSocrata)
library(ggpubr)

# get 2020 prez vote totals
# ----------------------------------
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ
PopVote2020 <- read_csv("data/PopVote2020.csv") %>% 
   transmute(state,repub_percent_2020=as.numeric(str_remove(rep_percent,"%"))) %>% 
   {.}
# https://electionlab.mit.edu/data
PopVote2020_county <- read_csv("data/countypres_2000-2020.csv") %>%
   rename(county = county_name) %>% 
   mutate(county = str_to_title(county)) %>% 
   mutate(state = str_to_title(state)) %>% 
   filter(year == "2020") %>% 
   filter(party == "REPUBLICAN") %>% 
   group_by(state,county) %>% 
   summarise(repubvotes=sum(candidatevotes),totalvotes=totalvotes) %>% 
   mutate(repub_percent_2020=repubvotes/totalvotes) %>% 
   ungroup %>% 
   {.}

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

if (REFRESH_VAX_DATA){
   # source https://github.com/nytimes/covid-19-data.git
   us_states_long <- covid19nytimes::refresh_covid19nytimes_states()
   us_counties_long <- covid19nytimes::refresh_covid19nytimes_counties()
   
   covid19_df <- refresh_coronavirus_jhu()

      # by state
   # takes a while so save data
   raw_vax <- as_tibble(read.socrata("https://data.cdc.gov/resource/unsk-b7fc.json"))
   write_csv(raw_vax,file="data/raw_vax.csv")
   
   # by county
   # takes a while so save data
   raw_vax_county <- 
      as_tibble(
         read.socrata("https://data.cdc.gov/resource/8xkx-amqh.json?$select=date, fips, recip_county, recip_state, series_complete_12pluspop, series_complete_pop_pct")
      )
   write_csv(raw_vax_county,file="data/raw_vax_county.csv")
}

raw_vax <- read_csv("data/raw_vax.csv")
raw_vax_county <- read_csv("data/raw_vax_county.csv")

# Days lag to use for changes in values over time
lag2 = 2 * 7

# ---------------------------------------------------
# Process data
print("Using pct of 12+ population")
vax_pct <- raw_vax %>% 
   mutate(across(4:ncol(raw_vax),as.double)) %>% 
   rename(state.abb = location,pct_full_vax = series_complete_12pluspop) %>% 
   group_by(state.abb) %>% 
   arrange(state.abb,date) %>% 
   mutate(pct_full_vax_prior = lag(pct_full_vax,lag2)) %>% 
   mutate(admin_per_100k_prior = lag(admin_per_100k,lag2)) %>% 
   # mutate(Date = as.Date(Date,format="%m/%d/%Y")) %>% 
   filter(date == max(date)) %>% 
   right_join(state_pop) %>% 
   select(state,state.abb,pct_full_vax,pct_full_vax_prior,admin_per_100k,admin_per_100k_prior) %>% 
   # select(date,state.abb,pct_full_vax,pct_full_vax_prior)
   {.}
# print(max(vax_pct$date))

vax_pct_county <- raw_vax_county %>% 
   rename(state.abb = recip_state,county = recip_county,pct_full_vax = series_complete_12pluspop) %>% 
   mutate(pct_full_vax = as.numeric(pct_full_vax)) %>% 
   mutate(county = str_remove(county," County| Parish")) %>% 
   group_by(fips,state.abb,county) %>% 
   arrange(state.abb,fips,date) %>% 
   mutate(pct_full_vax_prior = lag(pct_full_vax,lag2)) %>% 
   # mutate(Date = as.Date(Date,format="%m/%d/%Y")) %>% 
   filter(date == max(date)) %>% 
   right_join(county_pop) %>% 
   select(fips,state,state.abb,pct_full_vax,pct_full_vax_prior) %>% 
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
   mutate(cases_7day_prior = (lag(cases_total, lag2)-lag(cases_total, lag2+7)) / 7) %>%
   mutate(deaths_7day_prior = (lag(deaths_total, lag2)-lag(deaths_total, lag2+7)) / 7)%>% 
   mutate(deaths_last90 = deaths_total-lag(deaths_total,90)) %>% 
   filter(date == max(date)) %>% 
   right_join(state_pop) %>% 
   mutate(deaths_per_million=deaths_7day/pop*1000000,cases_per_million=cases_7day/pop*1000000) %>% 
   mutate(deaths_per_million_prior=deaths_7day_prior/pop*1000000,cases_per_million_prior=cases_7day_prior/pop*1000000) %>% 
   mutate(deaths_last90_per_million=deaths_last90/pop*1000000) %>% 
   mutate(cases_arrow_color = ifelse(cases_per_million-cases_per_million_prior>0,"red",rgb(0.2,0.7,0.1,0.5))) %>% 
   mutate(deaths_arrow_color = ifelse(deaths_per_million-deaths_per_million_prior>0,"red",rgb(0.2,0.7,0.1,0.5))) %>% 
   {.}

us_counties <- us_counties_long %>%
   # discard dates before cases were tracked.
   filter(date > as.Date("2020-03-01")) %>%
   pivot_wider(names_from = "data_type", values_from = "value") %>%
   rename(fips = location_code) %>%
   separate(location,into = c("county","state"),sep=",") %>% 
   select(date, fips,state, county, cases_total, deaths_total) %>%
   mutate(state = as_factor(state)) %>%
   arrange(fips, date) %>%
   group_by(state,county) %>%
   # smooth the data with prior 7 days
   mutate(cases_7day = (cases_total - lag(cases_total, 7)) / 7) %>%
   mutate(deaths_7day = (deaths_total - lag(deaths_total, 7)) / 7)%>%
   mutate(cases_7day_prior = (lag(cases_total, lag2)-lag(cases_total, lag2+7)) / 7) %>%
   mutate(deaths_7day_prior = (lag(deaths_total, lag2)-lag(deaths_total, lag2+7)) / 7)%>%
   mutate(deaths_last90 = deaths_total-lag(deaths_total,90)) %>%
   filter(date == max(date)) %>%
   right_join(county_pop) %>%
   mutate(deaths_per_million=deaths_7day/pop*1000000,cases_per_million=cases_7day/pop*1000000) %>%
   mutate(deaths_per_million_prior=deaths_7day_prior/pop*1000000,cases_per_million_prior=cases_7day_prior/pop*1000000) %>%
   mutate(deaths_last90_per_million=deaths_last90/pop*1000000) %>%
   mutate(cases_arrow_color = ifelse(cases_per_million-cases_per_million_prior>0,"red",rgb(0.2,0.7,0.1,0.5))) %>%
   mutate(deaths_arrow_color = ifelse(deaths_per_million-deaths_per_million_prior>0,"red",rgb(0.2,0.7,0.1,0.5))) %>%
   remove_missing() %>% 
   {.}

# oops bad data for Idaho so take it out for now
 us_states <- us_states %>% filter(state != "Idaho")

 vax_effect <- right_join(vax_pct,us_states) %>%
    mutate(pct_unvaxed=100-pct_full_vax) %>% 
    mutate(pct_unvaxed_prior=100-pct_full_vax_prior) %>% 
    {.}
 vax_effect_county <- right_join(vax_pct_county,us_counties) %>%
    mutate(pct_unvaxed=100-pct_full_vax) %>% 
    mutate(pct_unvaxed_prior=100-pct_full_vax_prior) %>% 
    #remove bad data
    filter(pct_unvaxed != 100,pct_unvaxed != 0,deaths_per_million >= 0) %>% 
    {.}
 

# Plot data by State
# ---------------------------------------------------
vax_effect %>% ggplot(aes(pct_unvaxed,cases_per_million)) + 
   #   geom_point() + 
   geom_smooth(method = "gam",se = FALSE) + 
   geom_text(aes(label=state.abb)) +
   labs(title = "Fewer Vaxed, More Cases",
        x = "Percent of Age 12+ Population  Not Fully Vaxed",
        subtitle = paste("New Case in Week Ending",max(us_states$date)),
        caption = "Sources: Johns Hopkins, CDC. Census Bureau")


vax_effect %>% ggplot(aes(pct_unvaxed,deaths_per_million)) + 
   geom_smooth(method = "gam",se = FALSE) + 
   geom_text(aes(label=state.abb)) +
   labs(title = "More Vaxed, Less Dying",
        subtitle = paste("New Deaths in Two Weeks Ending",max(us_states$date)),
        x = "Percent of Age 12+ Population Not Fully Vaxed",
        y = "Deaths per Million Residents",
        caption = "Sources: Johns Hopkins, CDC, Census Bureau")


# Deltas
vax_effect %>% ggplot(aes(pct_unvaxed,cases_per_million)) + 
   geom_point(aes(x=pct_unvaxed_prior, y=cases_per_million_prior)) + 
   geom_segment(aes(x=pct_unvaxed_prior, xend=pct_unvaxed, 
                    y=cases_per_million_prior, yend=cases_per_million*0.98), 
                arrow = arrow(type="closed",angle=10,length = unit(0.2, "inches"),),
                color=vax_effect$cases_arrow_color) +
   geom_smooth(method = "gam",se = FALSE) + 
   geom_text(aes(label=state.abb)) +
   labs(title = "Fewer Vaxed, More Cases",
        subtitle = paste("New Cases and Change in New Cases For Week Ending",max(us_states$date)),
        x = "Percent of Age 12+ Population Not Fully Vaxed",
        caption = "Sources: Johns Hopkins, CDC, Census Bureau")

vax_effect %>% ggplot(aes(pct_unvaxed,deaths_per_million)) + 
   geom_point(aes(x=pct_unvaxed_prior,y=deaths_per_million_prior)) + 
   geom_segment(aes(x=pct_unvaxed_prior, xend=pct_unvaxed, 
                    y=deaths_per_million_prior, yend=deaths_per_million*0.98), 
                arrow = arrow(type="closed",angle=10,length = unit(0.2, "inches"),),
                color=vax_effect$deaths_arrow_color) +
   geom_smooth(method = "gam",se = FALSE) + 
   geom_text(aes(label=state.abb)) +
   labs(title = "Fewer Vaxed, More deaths",
        subtitle = paste("New Deaths and Change in New deaths For 2 Weeks Ending",max(us_states$date)),
        x = "Percent of Age 12+ Population Not Fully Vaxed",
        caption = "Sources: Johns Hopkins, CDC, Census Bureau")

# acceleration in vaccination
vax_effect %>% 
   mutate(vax_delta = pct_full_vax-pct_full_vax_prior) %>% 
   ggplot(aes(fct_reorder(state.abb,vax_delta),vax_delta)) + 
   geom_col() +
   coord_flip() +
   labs(title="Progress in Vaccinations",
        subtitle = paste0(lag2,"-Day Change As of ",max(us_states$date)),
        y="Change in Vaccinated Pop. (Percentage Points)",
        x = "",
        caption="Source: CDC")

# are low-vax states getting the message?
# change in vaccination
vax_effect %>% 
   mutate(vax_delta = admin_per_100k - admin_per_100k_prior) %>% 
   mutate(covid_delta = (cases_per_million-cases_per_million_prior)) %>% 
   ggplot(aes(covid_delta,vax_delta)) + 
   #   geom_point() + 
   geom_text(aes(label=state.abb)) +
   geom_smooth(method = "glm",se=F) +
   labs(title = "Do Surging Cases Encourage Vaccination?",
        subtitle = paste0(lag2,"-Day Change As of ",max(us_states$date)),
        y="Change In Doses Given per 100k Pop.",
        x = "Change in Cases Per Million Pop.",
        caption = "Sources: Johns Hopkins, CDC, Census Bureau")

# --------------------------------
# POLITICS
vax_politics <- vax_effect %>%
   left_join(PopVote2020)

vax_politics %>% 
   ggplot(aes(repub_percent_2020,pct_unvaxed)) + 
   #   geom_point() + 
   geom_smooth(method = "glm",se = FALSE) + 
   geom_text(aes(label=state.abb)) +
   labs(title = "More Republican, Less Vaxed",
        x = "Trump Share of Vote in 2020",
        y = "Percent of State Not Fully Vaxed",
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
   # geom_smooth(se = FALSE) + 
   geom_smooth(method = "glm",se = FALSE) + 
   geom_text(aes(label=state.abb)) +
   labs(title = "More Republican, More Deaths",
        x = "Trump Share of Vote in 2020",
        y = "Daily COVID Deaths Per Million Residents",
        subtitle = paste("7-Day Average As of",max(us_states$date)),
        caption = "Sources: Johns Hopkins, CDC. Census Bureau, Cook Political Report") +
   stat_cor()

# ---------------------------------------------------
# Plot data by County

STATE_CODE <- "CA"
vax_effect_county %>% 
   filter(state.abb == STATE_CODE) %>% 
   ggplot(aes(pct_unvaxed,cases_per_million)) + 
   #   geom_point() + 
   geom_smooth(method = "gam",se = FALSE) + 
   geom_text(aes(label=county)) +
#   geom_point()+
   labs(title = "Fewer Vaxed, More Cases",
        x = "Percent of Age 12+ Population  Not Fully Vaxed",
        subtitle = paste(STATE_CODE,"New Case in Week Ending",max(us_states$date)),
        caption = "Sources: Johns Hopkins, CDC. Census Bureau")


vax_effect_county %>% 
   filter(state.abb == STATE_CODE) %>% 
   ggplot(aes(pct_unvaxed,deaths_per_million)) + 
   geom_smooth(method = "gam",se = FALSE) + 
#   geom_text(aes(label=state.abb)) +
   geom_text(aes(label=county)) +
   #   geom_point()+
   labs(title = "More Vaxed, Less Dying",
        subtitle = paste(STATE_CODE," New Deaths in Two Weeks Ending",max(us_states$date)),
        x = "Percent of Age 12+ Population Not Fully Vaxed",
        y = "Deaths per Million Residents",
        caption = "Sources: Johns Hopkins, CDC, Census Bureau")





#POLITICS BY COUNTY

vax_politics_county <- vax_effect_county %>%
   left_join(PopVote2020_county)


vax_politics_county %>% 
   filter(state.abb == STATE_CODE) %>% 
   ggplot(aes(repub_percent_2020,deaths_per_million)) + 
   geom_smooth(method = "glm",se = FALSE) + 
   geom_text(aes(label=county)) +
#   geom_point() +
   scale_x_continuous(labels = scales::percent) + 
   labs(title = "More Republican, More Deaths",
        subtitle = paste(STATE_CODE," Counties Daily Average, Last 14 Days as of",max(us_states$date)),
        x = "Trump Share of Vote in 2020",
        y = "Daily COVID Deaths Per Million Residents",
        caption = "Sources: Johns Hopkins, CDC. Census Bureau, Cook Political Report") +
   stat_cor()

