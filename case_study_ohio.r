# OHIO DATA ----------------------------------------------------
# assumes data from cases_vs_deaths.r is loaded

# NOTE: Ohio stopped reporting in March 2021
best_fit_st %>%
  select(-data, -model) %>%
  filter(state == "Ohio") %>%
  kable()
# source: https://coronavirus.ohio.gov/static/dashboards/COVIDSummaryData.csv
ohio_raw <- read_csv("https://coronavirus.ohio.gov/static/dashboards/COVIDSummaryData.csv",
                     col_types = cols(
                       `Admission Date` = col_date(format = "%m/%d/%Y"),
                       `Date Of Death` = col_date(format = "%m/%d/%Y"),
                       `Onset Date` = col_date(format = "%m/%d/%Y")
                     )
)

# helper function to fix column names to best practice
fix_df_colnames <- function(df) {
  names(df) <- names(df) %>%
    str_replace_all(c(" " = "_", "," = "")) %>%
    tolower()
  return(df)
}

# clean up the data
ohio <- ohio_raw %>%
  rename(death_count = `Death Due to Illness Count`) %>%
  filter(County != "Grand Total") %>%
  fix_df_colnames() %>%
  # data not clean before middle of march
  filter(onset_date >= cutoff_start)

# compare ohio data to same NY Times data
comps <- ohio %>%
  group_by(onset_date) %>%
  summarise(OH = sum(case_count), .groups = "drop") %>%
  mutate(OH = mean_roll_7(OH)) %>%
  ungroup() %>%
  mutate(state = "Ohio") %>%
  rename(date = onset_date) %>%
  left_join(us_states, by = c("date", "state")) %>%
  transmute(date, OH, NYTimes = cases_1day)

comps %>%
  pivot_longer(c("OH", "NYTimes"), names_to = "source", values_to = "count") %>%
  ggplot(aes(date, count, color = source)) +
  geom_line() +
  labs(
    title = "Case Counts from Different Sources",
    caption = "Source: State of Ohio, NY Times",
    subtitle = "NY Times and State of Ohio",
    x = "Date",
    y = "Daily Case Count (7-day Rolling Average)"
  )


# aggregate the data to weekly
# simulate invsibility of data more than test_lag ahead
test_lag = 30
ohio <- ohio %>%
  mutate(
    onset_to_death = as.numeric(date_of_death - onset_date),
    onset_year = year(onset_date),
    onset_week = epiweek(onset_date), 
    onset_to_death_test = ifelse(onset_date < test_lag,onset_to_death,NA),
    death_count_test = ifelse(onset_date < test_lag,death_count,0)
  )


onset_to_death <- ohio %>%
  filter(death_count > 0) %>%
  group_by(onset_year, onset_week) %>%
  summarise(
    death_count_sum = sum(death_count),
    mean_onset_to_death = weighted.mean(onset_to_death,
                                        death_count,
                                        na.rm = TRUE
    ),
    death_count_sum_test = sum(death_count_test),
    mean_onset_to_death_test = weighted.mean(onset_to_death_test,
                                             death_count,
                                             na.rm = TRUE
    )
  ) %>%
  mutate(date = as.Date(paste(onset_year, onset_week, 1), "%Y %U %u"))

onset_to_death %>% ggplot(aes(date, death_count_sum)) +
  geom_col() +
  labs(
    title = "Ohio Weekly Deaths",
    caption = "Source: State of Ohio, Arthur Steinmetz",
    subtitle = "Based on Illness Onset Date",
    x = "Date of Illness Onset",
    y = "Deaths"
  ) + 
  geom_col(aes(y=death_count_sum_test),color="red")

# helper function to annotate plots
pos_index <- function(index_vec, fraction) {
  return(index_vec[round(length(index_vec) * fraction)])
}

avg_lag <- round(mean(onset_to_death$mean_onset_to_death))

onset_to_death %>% ggplot(aes(date, mean_onset_to_death)) +
  geom_col() +
  geom_hline(yintercept = avg_lag) +
  annotate(
    geom = "text",
    label = paste("Average Lag =", round(avg_lag)),
    y = 20, x = pos_index(onset_to_death$date, .8)
  ) +
  labs(
    x = "Onset Date",
    y = "Mean Onset to Death",
    title = "Ohio Days from Illness Onset Until Death Over Time",
    caption = "Source: State of Ohio, Arthur Steinmetz",
    subtitle = paste(
      "Average =",
      avg_lag, "Days"
    )
  )

ohio_fatality_rate <- ohio %>%
  group_by(onset_date) %>%
  summarize(
    case_count = sum(case_count),
    death_count = sum(death_count),
    death_count_test = sum(death_count_test),
    .groups = "drop"
  ) %>%
  mutate(fatality_rate = death_count / case_count) %>%
  mutate(fatality_rate_7day = mean_roll_7(fatality_rate)) %>% 
  mutate(fatality_rate_test = death_count_test / case_count) %>%
  mutate(fatality_rate_7day_test = mean_roll_7(fatality_rate_test))

ohio_fatality_rate %>%
  filter(onset_date > as.Date("2020-04-15")) %>%
  ggplot(aes(onset_date, fatality_rate_7day)) +
  geom_line() +
  geom_smooth() +
  labs(
    x = "Illness Onset Date", y = "Ohio Fatality Rate",
    caption = "Source: State of Ohio, Arthur Steinmetz",
    title = "Ohio Fatality Rate as a Percentage of Tracked Cases"
  ) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 0.12, by = .01)) + 
  scale_x_date(date_breaks = "month",date_labels = "%b") +
  geom_line(aes(y= fatality_rate_test),color="red")

