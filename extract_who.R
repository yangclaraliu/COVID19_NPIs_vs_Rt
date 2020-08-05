who <- covidregionaldata::get_national_data(source = "WHO") %>% 
  mutate(region = countrycode(country,
                              origin = "country.name",
                              destination = "region",
                              nomatch = NULL)) %>% 
  dplyr::filter(region %in% (countrycode::codelist$region %>% unique)) %>% 
  filter(!is.na(region))

who %>% 
    group_by(date, region) %>% 
    summarise(value = sum(cases_new, na.rm = T)) %>% 
    filter(value != 0) %>% 
    group_by(region) %>% 
    mutate(rk = rank(date, ties.method = "first")) %>% 
    arrange(region) %>% 
    filter(rk == 1) %>% 
    arrange(region) %>% 
    data.frame %>%
    mutate(date = if_else(region == "East Asia & Pacific", 
                          as.Date("2020-01-01"), 
                          date),
           date = date) %>% 
    mutate(rk = factor(rk,
                       labels = c("First case\ndetected"))) -> start

