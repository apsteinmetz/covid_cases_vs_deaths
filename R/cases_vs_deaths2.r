# correlate deaths and cases by state
library(tidyverse)
library(tidymodels)
library(covid19nytimes)
library(timetk)
library(lubridate)
library(broom)


# source https://github.com/nytimes/covid-19-data.git
us_states_long <- covid19nytimes::refresh_covid19nytimes_states()
save(us_states_long,file="data/us_states_long.rdata")

# Create rolling average changes
# pivot wider
# this will also be needed when we create lags
us_states <- us_states_long %>%
  filter(date > as.Date("2020-03-01")) %>% 
  pivot_wider(names_from="data_type",values_from="value") %>% 
  rename(state=location) %>%
  select(date,state,cases_total,deaths_total) %>%
  mutate(state = as_factor(state)) %>% 
  arrange(state,date) %>% 
  group_by(state) %>%
  #smooth the data with 7 day moving average
  #mutate(cases_7day = (cases_total - lag(cases_total,7))/7) %>%
  #mutate(deaths_7day = (deaths_total - lag(deaths_total,7))/7) %>%
  {.}

# ------------------------------------------
# state by state analysis
# states hit their peaks (to date) at very different times, making state
# a useful feature. We could use one dummy variable for each state but it
# is simpler with tidymodels to conduct a separate set of models for each state

# illustrate selected states
coeff <- 40
us_states %>% 
  filter(state %in% c("Florida","Texas","California","Michigan")) %>% 
  ggplot(aes(date,cases_7day)) + geom_line(color="orange") +
  facet_wrap(~state,scales = "free") +
  theme(legend.position = "none") +
  geom_line(aes(y=deaths_7day*coeff),color="red") +
  scale_y_continuous(labels = scales::comma,
                     name = "Cases",
                     sec.axis = sec_axis(deaths_7day~./coeff,
                                         name="Deaths",
                                         labels = scales::comma)) +
  theme(
    axis.title.y = element_text(color = "orange", size=13),
    axis.title.y.right = element_text(color = "red", size=13)
  ) +
  labs(title =  "U.S. Cases vs. Deaths",
       subtitle = "7-Day Average",
       x = "Date")


# create lags
us_states_lags <- us_states %>%
  group_by(state) %>% 
  # create lags by day
  tk_augment_lags(cases_total,.lags = 1:60,.names="auto") %>% 
  # create lags by week is using weekly data
  # tk_augment_lags(deaths_7day,.lags = -8:0,.names="auto") %>% 
  {.}
# fix names to remove minus sign
#names(us_states_lags) <- names(us_states_lags) %>% str_replace_all("lag-","lead")


# preprocess
us_split <- initial_split(us_states_lags,prop=0.75)

us_split %>% testing() %>% glimpse()

us_recipe <- training(us_split) %>% 
  recipe(deaths_total ~.) %>%
  step_diff(all_numeric()) %>% 
  step_smooth(all_numeric(), period = 7) %>% #weekly
  prep()

us_testing <- us_recipe %>% bake(testing(us_split))
us_training <- juice(us_recipe)


us_rf <- rand_forest(trees = 100, mode = "regression") %>% 
  set_engine("randomForest") %>% 
  fit(deaths_total ~.,data=us_training)


# ------------------------------------------------
# make long form to nest
# initialize models data frame
models_st <- us_states_lags %>% ungroup %>% 
  pivot_longer(cols = contains("lead"),
               names_to = "lead",
               values_to = "led_deaths") %>% 
  select(state,date,cases_7day,lead,led_deaths) %>% 
  mutate(lead = as.numeric(str_remove(lead,"deaths_7day_lead"))) %>% 
  {.}

# make separate tibbles for each regression
models_st <- models_st %>% 
  nest(data=c(date,cases_7day,led_deaths)) %>% 
  arrange(lead)

#Run a linear regression on lagged cases and date vs deaths
# Polynomial degree for date.
degree = 1

models_st <- models_st %>% 
  mutate(model = map(data,
                     function(df) 
                       lm(led_deaths~cases_7day+poly(date,degree),data = df)))


# Add regression coefficient
# get adjusted r squared
models_st <- models_st %>% 
  mutate(adj_r = map(model,function(x) glance(x) %>% 
                       pull(adj.r.squared))
         %>% unlist)

models_st %>%
  ggplot(aes(lead,adj_r)) + geom_line() +
  facet_wrap(~state)


# best fit lag by state
best_fit <- models_st %>% 
  group_by(state) %>% 
  summarize(adj_r = max(adj_r)) %>% 
  left_join(models_st)

best_fit %>% ggplot(aes(adj_r)) + 
  geom_histogram(bins = 10,color="white") +
  geom_vline(xintercept = models$adj_r[lag_override],color="red") +
  annotate(geom="text",x=0.5,y=12,label="Adj-R in National Model") +
  labs(y = "State Count",
       x="Adjusted R-Squared",
       title = "Goodness of Fit of State Models",
       caption = "Source:NY Times,My Analysis")

best_fit %>% ggplot(aes(lead)) + 
  geom_histogram(binwidth = 5,color="white") +
  labs(y = "State Count",
    x="Best Fit Model Days from Case to Death",
    title = "COVID-19 Lag Time From Cases to Death",
    caption = "Source:NY Times,My Analysis")

# ----------------------------------------------------
# Reality check with longitudinal data from Ohio

models_st %>% 
  filter(state=="Ohio") %>% 
  ggplot(aes(lead,adj_r)) + geom_line() +
  labs(x = "Lead Time in Days for Deaths",
       y= "Adjusted R-squared",
       title = "Ohio: Model Fit by Lag Days")


# source: https://coronavirus.ohio.gov/static/COVIDSummaryData.csv



ohio_raw <- read_csv("https://coronavirus.ohio.gov/static/COVIDSummaryData.csv", 
                     col_types = cols(`Admission Date` = col_date(format = "%m/%d/%Y"), 
                                      `Date Of Death` = col_date(format = "%m/%d/%Y"), 
                                      `Onset Date` = col_date(format = "%m/%d/%Y")))


fix_df_colnames <- function(df){
  names(df)<-names(df) %>% 
    str_replace_all(c(" " = "_" , "," = "" )) %>% 
    tolower()
  return(df)
}

ohio <- ohio_raw %>% 
  fix_df_colnames() %>%
  mutate(sex = as_factor(sex),
         county = as_factor(county),
         age_range = as_factor(age_range))


ohio <- ohio %>% mutate(onset_to_death = as.numeric(date_of_death - onset_date),
                        fatal_flag = if_else(!is.na(date_of_death),TRUE,FALSE),
                        onset_year = year(onset_date),
                        onset_week = epiweek(onset_date))
  

onset_to_death <- ohio %>%
  filter(county != "Grand Total") %>% 
  group_by(onset_year,onset_week) %>%
  summarise(mean_onset_to_death = mean(onset_to_death,na.rm = TRUE)) %>%
  mutate(date=as.Date(paste(onset_year,onset_week,1),"%Y %U %u")) %>% 
  remove_missing()

# helper function to annotate plots 
pos_index <- function(index_vec,fraction){
  return(index_vec[round(length(index_vec)*fraction)])
}
avg_lag <- mean(onset_to_death$mean_onset_to_death)
onset_to_death %>% ggplot(aes(date,mean_onset_to_death)) + 
  geom_col() +
  geom_hline(yintercept = avg_lag) +
  annotate(geom="text",
           label=paste("Average Lag =",round(avg_lag)),
           y=20,x=pos_index(onset_to_death$date,.8)) +
  labs(x = "Onset Date",
       y = "Mean Onset to Death",
       title = "Ohio Days from Illness Onset Until Death Over Time",
       subtitle = paste("Average =",
                        round(mean(onset_to_death$mean_onset_to_death)),"Days"))


