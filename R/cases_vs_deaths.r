# correlate deaths and cases by state
library(tidyverse)
# library(covid19nytimes)
library(COVID19)
library(timetk)
library(lubridate)
library(broom)
library(knitr)
library(cowplot)

# create rolling average function
# while most displays of smoothed COVID data show trailing 7 days,
# we create rolling averages based on the middle of the time window.
# this allows us to make predictions based on a specific day rather
# than the trailing period.  Since these numbers have steep trends
# the numbers centered on just a few days ago could be very different.
mean_roll_7 <- slidify(mean, .period = 7, .align = "middle")

# devtools::install_github("covid19R/covid19nytimes")
# us_states_long_raw <- covid19nytimes::refresh_covid19nytimes_states()
us_states_raw <- COVID19::covid19(country="US",level=2,verbose=FALSE)


# if link is broken
# load("../data/us_states_long.rdata")

cutoff_start <- as.Date("2020-06-15") 
# cutoff_end <- max(us_states_long$date) - 7 # discard last week since there are reporting lags
# cutoff_end <- max(us_states_long_raw$date)
cutoff_end <- min(max(us_states_raw$date),Sys.Date()-1)

# # Remove tiny territories
territories <- c("Guam", "Northern Mariana Islands","American Samoa")

# use data since vaccines became available
# cutoff_start <- as.Date("2021-03-15") # not widespread enough until then

#use covid19 package
# rename for change to new data package
us_states <- us_states_raw %>% 
  as_tibble() %>% 
  filter(date >= cutoff_start) %>% 
  filter(date <= cutoff_end) %>% 
  rename(location = administrative_area_level_2) %>% 
  filter(!(location %in% territories)) %>% 
  rename(deaths_total = deaths) %>% 
  rename(cases_total = confirmed) %>% 
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
  mutate(cases_1day = mean_roll_7(cases_1day)) %>%
  mutate(deaths_1day = mean_roll_7(deaths_1day)) %>%
  # filter(date < max(date - 3)) %>%
  {.}

  

# #use data from nytimes covid19 package which comes in long form
# us_states_long <- us_states_long %>% filter(!(location %in% territories))

# save(us_states_long, file = "us_states_long.rdata")
# 
# us_states_long %>%
#   head() %>%
#   kable()
# 
# 
# us_states_long <- us_states_long_raw %>% 
#   filter(date >= cutoff_start) %>% 
#   filter(date <= cutoff_end)
# # Create rolling average changes
# # pivot wider
# # this will also be needed when we create lags
# # discard dates before cases were tracked.
# us_states <- us_states_long %>%
#   # discard dates before cases were tracked.
#   filter(date > as.Date("2020-03-01")) %>%
#   pivot_wider(names_from = "data_type", values_from = "value") %>%
#   rename(state = location) %>%
#   select(date, state, cases_total, deaths_total) %>%
#   mutate(state = as_factor(state)) %>%
#   arrange(state, date) %>%
#   group_by(state) %>%
#   # smooth the data with prior 7 days
#   mutate(cases_7day = (cases_total - lag(cases_total, 7)) / 7) %>%
#   mutate(deaths_7day = (deaths_total - lag(deaths_total, 7)) / 7)%>% 
#   # smooth the data with a centered mean of 7 days
#   mutate(cases_1day = (cases_total - lag(cases_total, 1))) %>%
#   mutate(deaths_1day = (deaths_total - lag(deaths_total, 1))) %>% 
#   mutate(cases_1day = mean_roll_7(cases_1day)) %>%
#   mutate(deaths_1day = mean_roll_7(deaths_1day)) %>% 
#   # filter(date < max(date - 3)) %>% 
#   {.}

# national analysis
# ----------------------------------------------
# aggregate state to national
us <- us_states %>%
  group_by(date) %>%
  summarize(across(
    .cols = where(is.numeric),
    .fns = function(x) sum(x, na.rm = T),
    .names = "{.col}"
  ))

us[10:20, ] %>% kable()

# does a simple scatterplot tell us anything
# about the relationship of deaths to cases? No.
us %>%
  ggplot(aes(deaths_1day, cases_1day)) +
  geom_point() +
  labs(
    title = "Not Useful",
    caption = "Source: covid19datahub.io, Arthur Steinmetz"
  )

# visualize the relationship between rolling average of weekly cases and deaths
coeff <- 30
us %>%
  ggplot(aes(date, cases_1day)) +
  geom_line(color = "orange") +
  theme(legend.position = "none") +
  geom_line(aes(x = date, y = deaths_1day * coeff), color = "red") +
  scale_y_continuous(
    labels = scales::comma,
    name = "Cases",
    sec.axis = sec_axis(deaths_1day ~ . / coeff,
      name = "Deaths",
      labels = scales::comma
    )
  ) +
  theme(
    axis.title.y = element_text(color = "orange", size = 13),
    axis.title.y.right = element_text(color = "red", size = 13)
  ) +
  labs(
    title = "U.S. Cases vs. Deaths",
    subtitle = "7-Day Average",
    caption = "Source: covid19datahub.io, Arthur Steinmetz",
    x = "Date"
  )
# passage of time affects deaths more than cases
lm(deaths_1day ~ cases_1day + date, data = us) %>% tidy()

# create columns for deaths led 0 to 40 days ahead
max_lead <- 40
us_lags <- us %>%
  # create lags by day
  tk_augment_lags(deaths_1day, .lags = 0:-max_lead, .names = "auto")
# fix names to remove minus sign
names(us_lags) <- names(us_lags) %>% str_replace_all("lag-|lag", "lead")

# use only case dates where we have complete future knowledge of deaths for all lead times.
us_lags <- us_lags %>% filter(date < cutoff_end - max_lead)

us_lags[1:10, 1:7] %>% kable()
# make long form to nest
# initialize models data frame
models <- us_lags %>%
  ungroup() %>%
  pivot_longer(
    cols = contains("lead"),
    names_to = "lead",
    values_to = "led_deaths"
  ) %>%
  select(date, cases_1day, lead, led_deaths) %>%
  # make sure string represents correct smoothed column
  mutate(lead = as.numeric(str_remove(lead, "deaths_1day_lead"))) %>%
  nest(data = c(date, cases_1day, led_deaths)) %>%
  # Run a regression on lagged cases and date vs deaths
  mutate(model = map(
    data,
    function(df) {
#      lm(led_deaths ~ cases_1day + poly(date, 2), data = df)
      # Alt Model
      lm(led_deaths ~ cases_1day, data = df)
    }
  ))

# Add regression coefficient
# get adjusted r squared
models <- models %>%
  mutate(adj_r = map(model, function(x) {
    glance(x) %>%
      pull(adj.r.squared)
  })
  %>% unlist())
models
# Show model fit by lead time
# make predictions using best model
best_fit <- models %>%
  summarize(adj_r = max(adj_r)) %>%
  left_join(models, by = "adj_r")

models %>%
  ggplot(aes(lead, adj_r)) +
  geom_line() +
  labs(
    subtitle = paste("Best fit lead =", best_fit$lead, "days"),
    title = "Model Fit By Lag Days",
    x = "Lead Time in Days for Deaths",
    caption = "Source: covid19datahub.io, Arthur Steinmetz",
    y = "Adjusted R-squared"
  )
best_fit$model[[1]] %>% tidy()

# ------------------------------------------
# see how well our model predicts
make_predictions <- function(newdata,single_model){
  predicted_deaths <- predict(single_model$model[[1]], newdata = newdata)
  date <- seq.Date(from = min(newdata$date + single_model$lead),
                   to = max(newdata$date) + single_model$lead, 
                   by = 1)
  display <- full_join(newdata, tibble(date, predicted_deaths))
  return(display)
}

# -----------------------------------------------------------
# Function to create prediction plot
show_predictions <- function(newdata,model,label) {
  display <- make_predictions(newdata,model)
  gg <- display %>%
    pivot_longer(cols = where(is.numeric)) %>%
    filter(name %in% c("deaths_1day", "predicted_deaths")) %>%
    ggplot(aes(date, value, color = name)) +
    geom_line(size = 1.5) +
    labs(
      title = paste("Actual vs. Predicted Deaths",label),
      x = "Date",
      y = "Count",
      caption = "Source: covid19datahub.io, Arthur Steinmetz"
    ) +
    theme_light() + 
    theme(legend.position = "top",legend.title = element_blank()) +
    annotate("text",x = as.numeric(max(newdata$date) - min(newdata$date))/2 + min(newdata$date),
             y = 0, 
             label = "See OUTSIDERDATA.NET for methodology and code.")
  
  gg
}
# --------------------------------------------------------
show_predictions(us,best_fit,"National Model")


fatality <- best_fit$data[[1]] %>%
  filter(cases_1day > 0) %>%
  filter(date > as.Date("2020-04-15")) %>%
  mutate(rate = led_deaths / cases_1day)

fatality %>% ggplot(aes(date, rate)) +
  geom_line() +
  geom_smooth() +
  labs(
    x = "Date", y = "Fatality Rate",
    title = "Fatality Rates",
    subtitle = "Fatality Rate as a Percentage of Lagged Cases",
    caption = "Source: covid19datahub.io, Arthur Steinmetz"
  ) +
  scale_y_continuous(labels = scales::percent)

# ------------------------------------------
# state by state analysis

state_subset <- c("New York", "Texas", "California", "Ohio")

# illustrate selected states
us_states %>%
  filter(state %in% state_subset) %>%
  ggplot(aes(date, cases_1day)) +
  geom_line(color = "orange") +
  facet_wrap(~state, scales = "free") +
  theme(legend.position = "none") +
  geom_line(aes(y = deaths_1day * coeff), color = "red") +
  scale_y_continuous(
    labels = scales::comma,
    name = "Cases",
    sec.axis = sec_axis(deaths_1day ~ . / coeff,
      name = "Deaths",
      labels = scales::comma
    )
  ) +
  theme(
    axis.title.y = element_text(color = "orange", size = 13),
    axis.title.y.right = element_text(color = "red", size = 13)
  ) +
  labs(
    title = "U.S. Cases vs. Deaths",
    subtitle = "7-Day Average",
    caption = "Source: covid19datahub.io, Arthur Steinmetz",
    x = "Date"
  )
# create lags
us_states_lags <- us_states %>%
  # create lags by day
  tk_augment_lags(deaths_1day, .lags = -max_lead:0, .names = "auto") 
# fix names to remove minus sign
names(us_states_lags) <- names(us_states_lags) %>% str_replace_all("lag-", "lead")

# make long form to nest
# initialize models data frame
models_st <- us_states_lags %>%
  ungroup() %>%
  pivot_longer(
    cols = contains("lead"),
    names_to = "lead",
    values_to = "led_deaths"
  ) %>%
  select(state, date, cases_1day, deaths_1day,lead, led_deaths) %>%
  mutate(lead = as.numeric(str_remove(lead, "deaths_1day_lead")))

# make separate tibbles for each regression
models_st <- models_st %>%
  nest(data = c(date, cases_1day, deaths_1day,led_deaths)) %>%
  arrange(lead)

# Run a linear regression on lagged cases and date vs deaths
models_st <- models_st %>%
  mutate(model = map(
    data,
    function(df) {
      #  lm(led_deaths ~ cases_1day + poly(date, 2), data = df)
    lm(led_deaths ~ cases_1day, data = df)
    }
  ))


# Add regression coefficient
# get adjusted r squared
models_st <- models_st %>%
  mutate(adj_r = map(model, function(x) {
    glance(x) %>%
      pull(adj.r.squared)
  })
  %>% unlist())

models_st %>%
  filter(state %in% state_subset) %>%
  ggplot(aes(lead, adj_r)) +
  geom_line() +
  facet_wrap(~state) +
  labs(
    title = "Best Fit Lead Time",
    caption = "Source: covid19datahub.io, Arthur Steinmetz"
  )

# best fit lag by state
best_fit_st <- models_st %>%
  group_by(state) %>%
  summarize(adj_r = max(adj_r)) %>%
  left_join(models_st)

best_fit_st %>% ggplot(aes(adj_r)) +
  geom_histogram(bins = 10, color = "white") +
  geom_vline(xintercept = best_fit$adj_r[[1]], color = "red") +
  annotate(geom = "text", x = 0.75, y = 18, label = "Adj-R in National Model") +
  labs(
    y = "State Count",
    x = "Adjusted R-Squared",
    title = "Goodness of Fit of State Models",
    caption = "Source: covid19datahub.io,Arthur Steinmetz"
  )
best_fit_st %>% ggplot(aes(lead)) +
  geom_histogram(binwidth = 5, color = "white") +
  scale_y_continuous(labels = scales::label_number(accuracy = 1)) +
  geom_vline(xintercept = best_fit$lead[[1]], color = "red") +
  annotate(geom = "text", x = best_fit$lead[[1]] + 7, y = 10, label = "Lead in National Model") +
  labs(
    y = "State Count",
    x = "Best Fit Model Days from Case to Death",
    title = "COVID-19 Lag Time From Cases to Death",
    caption = "Source: covid19datahub.io,Arthur Steinmetz"
  )
# Make state predictions
# show states with largest predicted increase in deaths over national lead period
target_state = "New York"

fit_state = best_fit_st %>% filter(state == target_state)
# manual lag override
fit_state = models_st %>% filter(state == target_state,lead==12)

gg1 <- show_predictions(fit_state$data[[1]],fit_state,target_state) + 
  labs(y = "Deaths",x="")

# Prediction Error

model_errors <-make_predictions(fit_state$data[[1]],fit_state) %>% 
  transmute(date=date,error=deaths_1day-predicted_deaths) %>% 
  # discard dates with misaligned data
  mutate(error = ifelse(date < as.Date("2020-07-07"),NA,error))
  
gg2 <- model_errors %>% 
  ggplot(aes(date,error)) + geom_line() +
  labs(title = "Model Error: Actual minus Predicted",
       y="Error",x="") +
  geom_hline(yintercept = 0) + 
  theme_light()


plot_grid(
  gg1, gg2,
  ncol = 1,
  rel_heights = c(2,1)
)

recent_state <- us_states %>% group_by(state) %>% 
  filter(date == max(date)) %>% 
  select(date,state,cases_1day,deaths_1day) %>% 
  {.}


recent_state <- recent_state %>% 
  full_join(tibble(state=recent_state$state,
                   future_date = recent_state$date[1] + best_fit$lead[[1]],
                   predicted_deaths = predict(best_fit$model[[1]],
                                              n.ahead=best_fit$lead[[1]],
                                              newdata = recent_state))) %>% 
  mutate(pred_change = predicted_deaths - deaths_1day,
         pred_pct_change = predicted_deaths/deaths_1day-1)

