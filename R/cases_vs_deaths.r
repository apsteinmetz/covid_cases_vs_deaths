# correlate deaths and cases by state
library(tidyverse)
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
  mutate(cases_7day = (cases_total - lag(cases_total,7))/7) %>%
  mutate(deaths_7day = (deaths_total - lag(deaths_total,7))/7) %>%
  {.}

#reduce to weekly so there not overlapping days
#us_states_wk <- us_states %>% 
#  filter(wday(date,label = TRUE)  == "Mon")

  
 
# national analysis
# ----------------------------------------------
# aggregate state to national
us <- us_states %>%
  group_by(date) %>% 
  summarize(across(.cols=where(is.double),
                   .fns = function(x)sum(x,na.rm = T),
                   .names="{.col}"))

us %>% 
  ggplot(aes(date,cases_total)) + geom_line(color="orange") +
  theme(legend.position = "none") +
  geom_line(aes(y=deaths_total*coeff),color="red") +
  scale_y_continuous(labels = scales::comma,
                     name = "Cases",
                     sec.axis = sec_axis(deaths_total~./coeff,
                                         name="Deaths",
                                         labels = scales::comma)) +
  theme(
    axis.title.y = element_text(color = "orange", size=13),
    axis.title.y.right = element_text(color = "red", size=13)
  ) +
  labs(title =  "U.S. Cases vs. Deaths",
       x = "Date")

# does a simple scatterplot tell us anything 
# about the relationship of deaths to cases? No.
us %>% 
  ggplot(aes(deaths_7day,cases_7day)) + geom_point() + geom_smooth()


#visualize the relationship between rolling average of weekly scases and deaths
us %>% 
  ggplot(aes(date,cases_7day)) + geom_line(color="orange") +
  theme(legend.position = "none") +
  geom_line(aes(x=date,y=deaths_7day*coeff),color="red") +
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


# passage of time affects deaths more than cases
lm(deaths_7day~cases_7day+date,data=us) %>% summary()

# now make the models

#create columns for deaths led 0 to 30 days ahead
us_lags <- us %>%
  # create lags by day
  tk_augment_lags(deaths_7day,.lags = -60:0,.names="auto") %>% 
  # create lags by week is using weekly data
  # tk_augment_lags(deaths_7day,.lags = -8:0,.names="auto") %>% 
  {.}
# fix names to remove minus sign
names(us_lags) <- names(us_lags) %>% str_replace_all("lag-","lead")

# make long form to nest
# initialize models data frame
models <- us_lags %>% ungroup %>% 
  pivot_longer(cols = contains("lead"),
               names_to = "lead",
               values_to = "led_deaths") %>% 
  select(date,cases_7day,lead,led_deaths) %>% 
  mutate(lead = as.numeric(str_remove(lead,"deaths_7day_lead"))) %>% 
  {.}

# make separate tibbles for each regression
models <- models %>% 
  nest(data=c(date,cases_7day,led_deaths)) %>% 
  arrange(lead)

#Run a linear regression on lagged cases and date vs deaths

# Polynomial degree for date.
degree = 1

models <- models %>% 
  mutate(model = map(data,
                     function(df) 
                       lm(led_deaths~cases_7day+poly(date,degree),data = df)))


# Add regression coefficient
# get adjusted r squared
models <- models %>% 
  mutate(adj_r = map(model,function(x) glance(x) %>% 
                       pull(adj.r.squared))
         %>% unlist)

# Show model fit by lead time
models %>%
  ggplot(aes(lead,adj_r)) + geom_line() +
  labs(subtitle = paste("Best fit lead =",best_fit$lead,"days"),
       title = "Model Fit By Lag Days",
       x = "Lead Time in Days for Deaths",
       y= "Adjusted R-squared")

# make predictions using best model
best_fit <- models %>% 
  summarize(adj_r = max(adj_r)) %>% 
  left_join(models)

# ------------------------------------------
# see how well our model predicts
show_predictions <- function(single_model){
  complete_cases <- single_model$data[[1]] %>% 
    remove_missing()
  
  predictions <- enframe(predict(single_model$model[[1]],na.action = exclude),
                         name = NULL,
                         value = paste0("predicted_in_","ahead","_days"))
  
  bind_cols(complete_cases,predictions) %>% 
    rename(cases=cases_7day,
           deaths_actual=led_deaths,
           deaths_predicted=predicted_in_ahead_days) %>%
    select(-cases) %>% 
    pivot_longer(cols = where(is.numeric)) %>% 
    ggplot(aes(date,value,color=name)) + geom_line()
  
}

# best adj_r is not the best fit
show_predictions(best_fit)

# local maximum of 14 days gives  much more satisfactory result
lag_override <- 14
show_predictions(models[lag_override,])

# ------------------------------------------
# state by state analysis
# states hit their peaks (to date) at very different times, making state
# a useful feature. We could use one dummy variable for each state but it
# is simpler with tidymodels to conduct a separate set of models for each state

# illustrate selected states
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
  # create lags by day
  tk_augment_lags(deaths_7day,.lags = -60:0,.names="auto") %>% 
  # create lags by week is using weekly data
  # tk_augment_lags(deaths_7day,.lags = -8:0,.names="auto") %>% 
  {.}
# fix names to remove minus sign
names(us_states_lags) <- names(us_states_lags) %>% str_replace_all("lag-","lead")

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


