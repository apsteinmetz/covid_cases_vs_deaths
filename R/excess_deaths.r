#excess mortality
library(tidyverse)
library(covid19nytimes)
library(RSocrata)


xsd_path <- "https://data.cdc.gov/resource/xkkf-xrst.json"
#xsd <- read_csv("data/Excess_Deaths_Associated_with_COVID-19.csv")


raw_xsd <- read.socrata(xsd_path) %>% as_tibble()
# since all data comes in as type char, write and re-read to use auto parsing
write_csv(raw_xsd,file= "data/raw_xsd.csv")
xsd <- read_csv("data/raw_xsd.csv", col_types = cols(note = col_character(), 
                                                  week_ending_date = col_date(format = "%Y-%m-%d")))
xsd <- as_tibble(xsd) %>% 
  mutate(observed_number = as.numeric(observed_number)) %>% 
  mutate(average_expected_count = as.numeric(average_expected_count)) %>% 
  mutate(week_ending_date = as.Date(week_ending_date))

agg_xsd <- xsd %>% 
  rename(date = `week_ending_date`) %>%
  group_by(date) %>% 
  filter(state == 'United States') %>% 
  filter(type == 'Predicted (weighted)',outcome == "All causes") %>% 	
  transmute(date = date,
            expected_deaths=average_expected_count,
            actual_deaths = observed_number) %>%
  mutate(excess_deaths = actual_deaths - expected_deaths) %>% 
  unique()
{.}


us_states_long <- covid19nytimes::refresh_covid19nytimes_states() %>% as_tibble()
agg_cvd <- us_states_long %>% group_by(date) %>% 
  filter(data_type == "deaths_total") %>% 
  summarise(deaths = sum(value)) %>% 
  mutate(covid_deaths = (deaths - lag(deaths,7)))

agg <- full_join(agg_xsd,agg_cvd) %>%
  filter(!is.na(actual_deaths)) %>% 
  select(-deaths) %>% 
  pivot_longer(cols=contains("deaths"),names_to="Type",values_to="weekly_deaths") %>% 
  # remove last data point to avoid reporting lags
  ungroup %>% 
  filter(date < max(date)) %>% 
  {.}

agg %>% filter(str_detect(Type,"actual|expected")) %>%  
  ggplot(aes(date,weekly_deaths,color=Type)) + geom_line() +
  scale_y_continuous(labels=scales::comma) +
  labs(title= "U.S. Mortality From All Causes",caption = "source: CDC")

agg %>% filter(str_detect(Type,"excess|covid")) %>%  
  filter(date > as.Date("2020-01-01")) %>% 
  ggplot(aes(date,weekly_deaths,color=Type)) + geom_line() +
  scale_y_continuous(labels=scales::comma) +
  geom_hline(yintercept = 0) +
  labs(title= "U.S. Excess Mortality and \nReported COVID Deaths",
       caption = "source: CDC, Johns Hopkins")

agg %>% mutate(year = lubridate::year(date)) %>% 
  group_by(year, Type) %>%
  tally(weekly_deaths) %>% 
  ggplot(aes(year,n,fill=Type)) + geom_col(position = "dodge")
