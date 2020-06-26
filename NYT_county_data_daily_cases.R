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

#Read data directly from New York Times' GitHub (updates ~daily)
covid_county_raw <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

#In case you want to plot cases as a function of days since X cases or Y deaths
min_cases <- 50
min_deaths <- 10

#Some data cleanup
state_terr_abb <- c(state.abb, "DC", "VI", "GU", "PR", "US")
state_terr_name <- c(state.name, 
                     "District of Columbia", "Virgin Islands", 
                     "Guam", "Puerto Rico", "US")

#Format data and take rolling average and rolling median of daily case count
covid_cases <- covid_county_raw %>%
  mutate(date = as.Date(date)) %>%
  filter(county != "Unknown") %>%
  mutate(fips = ifelse(county == "New York City", "NYC", fips)) %>%
  mutate(fips = ifelse(county == "Kansas City", "KSC", fips)) %>%
  group_by(fips) %>%
  mutate(new_cases = cases - dplyr::lag(cases)) %>%
  mutate(new_cases_7d_avg = rollmean(new_cases, k=7, na.pad=TRUE, align="right")) %>%
  mutate(new_cases_7d_med = rollmedian(new_cases, k=7, na.pad=TRUE, align="right")) %>%
  filter(cases >= min_cases) %>%
  mutate(days_since_min_cases = as.numeric(date - min(date[cases >= min_cases]))) %>%
  filter(!is.na(new_cases_7d_med)) %>%
  mutate(max_new_cases_med = max(new_cases_7d_med)) %>%
  ungroup()
#See notes on NYT GitHub about NYC and Kansas City

#Same for deaths
covid_deaths <- covid_county_raw %>%
  mutate(date = as.Date(date)) %>%
  filter(county != "Unknown") %>%
  mutate(fips = ifelse(county == "New York City", "NYC", fips)) %>%
  mutate(fips = ifelse(county == "Kansas City", "KSC", fips)) %>%
  group_by(fips) %>%
  mutate(new_deaths = deaths - dplyr::lag(deaths)) %>%
  mutate(new_deaths_7d_avg = rollmean(new_deaths, k=7, na.pad=TRUE, align="right")) %>%
  mutate(new_deaths_7d_med = rollmedian(new_deaths, k=7, na.pad=TRUE, align="right")) %>%
  filter(deaths >= min_deaths) %>%
  mutate(days_since_min_deaths = as.numeric(date - min(date[deaths >= min_deaths]))) %>%
  filter(!is.na(new_deaths_7d_med)) %>%
  mutate(max_new_deaths_med = max(new_deaths_7d_med)) %>%
  ungroup()


#Only plot counties that have reached these levels of cases or deaths at least one day
case_threshold <- 20
death_threshold <- 5

#Play around with uncommenting different lines for different ways to look at the data

#Plot cases
covid_cases %>%
  filter(max_new_cases_med >= case_threshold) %>%
  filter(fips !="NYC") %>%
  ggplot(aes(x=date, y=new_cases_7d_avg, 
             group=fips, color=state)) + 
  geom_line(lwd=1, alpha = 0.6) +
  facet_wrap(~state, scale="free_y") + 
  #facet_wrap(~state) +
  theme(legend.position = "none")

#Plot deaths
covid_deaths %>%
  filter(max_new_deaths_med >= death_threshold) %>%
  ggplot(aes(x=date, y=new_deaths_7d_med, 
             group=fips, color=state)) + 
  geom_line(lwd=1, alpha = 0.6) +
  #facet_wrap(~state) +
  facet_wrap(~state, scales = "free_y") +   
  theme(legend.position = "none")

