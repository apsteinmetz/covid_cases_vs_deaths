# OHIO Summary data
library(tidyverse)
library(lubridate)
ohio_raw <- read_csv("data/OhioCOVIDSummaryData.csv", 
                                 col_types = cols(`Admission Date` = col_date(format = "%m/%d/%Y"), 
                                                  `Date Of Death` = col_date(format = "%m/%d/%Y"), 
                                                  `Onset Date` = col_date(format = "%m/%d/%Y")))


fix_df_colnames <- function(df){
  names(df)<-names(df) %>% 
    str_replace_all(c(" " = "_" , "," = "" )) %>% 
    tolower()
  return(df)
}

ohio <- ohio_raw %>% fix_df_colnames() %>%
  mutate(sex = as_factor(sex),
         county = as_factor(county),
         age_range = as_factor(age_range))

# proportion female/male
ohio %>% 
  filter(sex !="Total") %>% 
  group_by(sex) %>% 
  summarise(died          = sum(death_due_to_illness_count),
            hospitalized = sum(hospitalized_count),
            cases = sum(case_count))

# proportion by age
ohio %>% 
  filter(age_range !="Total") %>% 
  group_by(age_range) %>% 
  summarise(died          = sum(death_due_to_illness_count),
            hospitalized = sum(hospitalized_count),
            cases = sum(case_count)) %>% 
  {.}


ohio <- ohio %>% mutate(onset_to_hosp = as.numeric(admission_date - onset_date),
                        onset_to_death = as.numeric(date_of_death - onset_date),
                hosp_to_death = as.numeric(date_of_death - admission_date),
                fatal_flag = if_else(!is.na(date_of_death),TRUE,FALSE),
                onset_year = year(onset_date),
                onset_week = epiweek(onset_date))

onset_to_death <- ohio %>%
  filter(county != "Grand Total") %>% 
  group_by(onset_week) %>%
  summarise(mean_onset_to_death = mean(onset_to_death,na.rm = TRUE))

