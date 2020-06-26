#NYT COVID19 data
#26 July  2020
#John J Davis IV

#@JDRuns
#This code is free to use and modify for any purpose.

#Make sure you uncomment to install these packages if you don't have them!
#install.packages("tidyverse")
#install.packages("zoo")
library(tidyverse)
library(zoo)

covid_state_raw <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

min_cases <- 50
min_deaths <- 10

covid_state_cases <- covid_state_raw %>%
  mutate(date = as.Date(date)) %>%
  group_by(fips) %>%
  mutate(new_cases = cases - dplyr::lag(cases)) %>%
  mutate(new_cases_7d_avg = rollmean(new_cases, k=7, na.pad=TRUE, align="right")) %>%
  mutate(new_cases_7d_med = rollmedian(new_cases, k=7, na.pad=TRUE, align="right")) %>%
  filter(cases >= min_cases) %>%
  mutate(days_since_min_cases = as.numeric(date - min(date[cases >= min_cases]))) %>%
  filter(!is.na(new_cases_7d_med)) %>%
  mutate(max_new_cases_med = max(new_cases_7d_med)) %>%
  ungroup()
  

#Same for deaths
covid_state_deaths <- covid_state_raw %>%
  mutate(date = as.Date(date)) %>%
  group_by(fips) %>%
  mutate(new_deaths = deaths - dplyr::lag(deaths)) %>%
  mutate(new_deaths_7d_avg = rollmean(new_deaths, k=7, na.pad=TRUE, align="right")) %>%
  mutate(new_deaths_7d_med = rollmedian(new_deaths, k=7, na.pad=TRUE, align="right")) %>%
  filter(deaths >= min_deaths) %>%
  mutate(days_since_min_deaths = as.numeric(date - min(date[deaths >= min_deaths]))) %>%
  filter(!is.na(new_deaths_7d_med)) %>%
  mutate(max_new_deaths_med = max(new_deaths_7d_med)) %>%
  ungroup()

case_threshold <- 20
death_threshold <- 5

#Plot cases
covid_state_cases %>%
  filter(max_new_cases_med >= case_threshold) %>%
  #filter(state != "New York") %>%
  ggplot(aes(x=date, y=new_cases_7d_avg, 
             group=fips, color=state)) + 
  geom_line(lwd=1, alpha = 0.6) +
  facet_wrap(~state, scale="free_y") + 
  #facet_wrap(~state) +
  theme(legend.position = "none")

#Plot deaths
covid_state_deaths %>%
  filter(max_new_deaths_med >= death_threshold) %>%
  ggplot(aes(x=date, y=new_deaths_7d_med+.01, 
             group=fips, color=state)) + 
  geom_line(lwd=1, alpha = 0.6) +
  #scale_y_log10() + 
  #facet_wrap(~state, scales = "free_y") +   
  facet_wrap(~state) +
  theme(legend.position = "none")
