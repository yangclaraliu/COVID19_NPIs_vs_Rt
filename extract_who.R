library(NCoVUtils)
library(tidyverse)
library(countrycode)
who <- get_who_cases(daily = T) %>% 
    dplyr::select(-starts_with("China-"),
                  -starts_with("Global"),
                  -starts_with("Region"),
                  -starts_with("Cases"),
                  -starts_with("International"),
                  -starts_with("RA-"),
                  -ends_with("deaths"))
colnames(who)    

cbind(colnames(who),countrycode::countrycode(colnames(who), 
                         origin = "country.name", 
                         destination = "region",
                         nomatch = NULL)) %>% 
    as_tibble %>% 
    setNames(c("cn", "region")) -> tags

tags$region[which(tags$cn %in% c("DominicanRepublic",
                               "LaoPeoplesDemocraticRepublic",
                               "IsleofMan",
                               "RepublicofKorea",
                               "UnitedStatesofAmerica",
                               "SouthAfrica",
                               "SaintLucia"))] <- 
countrycode::countrycode(c("Dominican Republic",
                           "Laos",
                           "Isle of Man",
                           "Republic of Korea",
                           "United States of America",
                           "South Africa",
                           "Saint Lucia"), 
                         origin = "country.name", 
                         destination = "region",
                         nomatch = NULL)

tags["exist"] <- tags$region %in% unique(countrycode(joined$hi$cnt,"iso3c","region"))

tags <- tags[tags$exist == T,]

tags$cn

start <- who %>% 
    dplyr::select(Date, tags$cn) %>% 
    pivot_longer(tags$cn) %>% 
    left_join(tags, by = c("name" = "cn")) %>% 
    group_by(Date, region) %>% 
    summarise(value = sum(value, na.rm = T)) %>% 
    filter(value != 0) %>% 
    group_by(region) %>% 
    mutate(rk = rank(Date, ties.method = "first")) %>% 
    arrange(region) %>% 
    filter(rk == 1) %>% 
    arrange(region) %>% 
    data.frame %>%
    mutate(Date = if_else(region == "East Asia & Pacific", as.Date("2020-01-01"), Date),
           date = Date) %>% 
    mutate(Date = as.numeric(Date),
           rk = factor(rk,
                       labels = c("First case\ndetected")))

