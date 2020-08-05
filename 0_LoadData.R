rm(list=ls())

# packages and functions
pacman::p_load(tidyverse,
               here, 
               plm, 
               countrycode, 
               magrittr, 
               zoo, 
               ggdendro, 
               cowplot, 
               lubridate,
               data.table,
               covidregionaldata)

# Get  Rt and policy action data from the web and save to local files if save_data == TRUE
root_path <- here::here()

# Load data
oxford_data       <- here("data","oxford_data_2020-07-05.csv") %>% read.csv(as.is = TRUE)
rt_estimates      <- here("data","rt_estimates_2020-07-05.csv") %>% read.csv 
UN_household      <- here("data", "un_data_comp_size.rds") %>% readRDS %>% as_tibble
covid19_pkg_data  <- here("data", "covid19_pkd_data_20200422.rds") %>% readRDS
urban_rural       <- here("data", "UN_Total_Urban_Rural_2018.csv") %>% read_csv

#Check for missingness
oxford_data_missing <- oxford_data %>% 
    filter_at(
        vars(contains("_") & !contains("Flag")), 
        any_vars(!is.na(.))) %>%
    mutate(marker = 1) %>%
    pivot_wider(names_from = Date,
                values_from = marker) %>% 
    pivot_longer(cols = starts_with("2020"), names_to = "Date") %>%
    group_by(Date) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    mutate(Date = ymd(Date))

oxford_data_missing %>%
    # filter(Date >= "2020-06-05") %>% View()
    # As of July 7th, things look fine until 2020-06-22
    ggplot(aes(x = Date, y= value)) +
    geom_line(stat="identity") +
    labs(x = "Date", y = "Number of countries with data") +
    cowplot::theme_cowplot() +
    theme(axis.text = element_text(size = 20),
          axis.title = element_text(size = 26)) +
    geom_vline(xintercept = as.Date("2020-06-22"))

here("figs", "fig_missing.png") %>%
    ggsave(width = 20, height = 10)

# Build stringency_data from oxford_data
oxford_data %>%
    as_tibble %>%
    mutate(Date = lubridate::ymd(Date)) %>%
    rename(cnt = CountryCode, date = Date, stringency = StringencyIndex) %>%
    select(cnt, date, stringency) %>%
    # filter(date >= as.Date("2020-06-01")) %>% 
    pivot_wider(names_from = date, values_from = stringency) %>% 
    pivot_longer(cols = starts_with("2020"),
                 names_to = "date",
                 values_to = "stringency") %>% 
    mutate(date = lubridate::ymd(date)) %>% 
    arrange(date) %>% 
    filter(cnt != "GIB") %>% 
    group_by(cnt) %>% 
    mutate(stringency = imputeTS::na_locf(stringency)) -> stringency

stringency %>%
    filter(date <= "2020-06-22") %>% 
    mutate(region = countrycode(cnt, "iso3c","region")) %>%
    filter(!is.na(region)) %>% 
    ggplot(., aes(x = date, y = stringency, group = cnt)) + 
    geom_line(aes(color = region), alpha = 0.1) +
    #facet_wrap(~region, ncol = 4) +
    theme_cowplot() +
    geom_smooth(aes(group = region, color = region, fill = region))+
    ggsci::scale_color_lancet()+
    ggsci::scale_fill_lancet()+
    labs(x = "Date",
         y = "Stringency Index",
         color = "Region",
         fill = "Region")  +
    theme(axis.text = element_text(size = 20),
          legend.position = "bottom") -> p_stringency

ggsave(filename = "figs/stringency_region.png",
       plot = p_stringency,
       width = 15,height = 10)

ggplot2::ggplot_build(p_stringency)$data[[2]] %>% 
    group_by(group) %>% 
    mutate(rk = rank(desc(y))) %>%
    filter(rk == 1) %>% 
    pull(x) -> turnpoint_val

# print the turningpoint identified by GAM by World Bank Region
# this content is found in supplemental A3
cbind(turnpoint_val,
      stringency %>%
          filter(date <= "2020-06-22") %>% 
          mutate(region = countrycode(cnt, "iso3c","region")) %>%
          filter(!is.na(region)) %>% pull(region) %>% table %>% names) %>% 
    as_tibble %>% 
    setNames(c("date","region")) %>% 
    mutate(date = as.numeric(date) %>% round) %>% 
    left_join(seq(lubridate::ymd("2020-01-01"),
                  lubridate::ymd("2020-07-01"),
                  by = "day") %>% 
                  enframe(., name = "ID", value = "date") %>% 
                  mutate(date_val = as.numeric(date)),
              by = c("date" = "date_val"))

seq(lubridate::ymd("2020-01-01"),
    lubridate::ymd("2020-07-01"),
    by = "day") %>% 
    enframe(., name = "ID", value = "date") %>% 
    mutate(date_val = as.numeric(date)) %>%
    
    filter(date_val %in% round(turnpoint_val))

# Build policy_dic, lookup tibble of policy codes and names 
colnames(oxford_data) %>% 
    .[!grepl("_Flag", .)] %>% 
    .[grep(paste(c(paste0("C",1:8),
                   paste0("E",1:4),
                   paste0("H",1:5)), collapse = "|"), .,ignore.case = F)] %>% 
    str_split(pattern = "_") %>% 
    do.call("rbind",.) %>% 
    as_tibble %>% 
    setNames(c("policy_code","policy_name")) %>% 
    mutate(policy_max = c(3, 3, 2, 4, 2, 3, 2, 4,
                          2, 2, NA, NA,
                          2, 3, 2, NA, NA)) -> policy_dic

# Build policy_data
oxford_data %>%
    as_tibble %>%
    # Make this a long tibble with rows for policy actions and columns for 
    # policy_value and policy_flag
    select(CountryCode, Date, contains("_")) %>%
    pivot_longer(cols = contains("_"), names_to = "policy_name") %>%
    mutate(flag = ifelse(str_detect(policy_name, "_Flag"), "policy_flag", "policy_value")) %>%
    mutate(policy_name = str_replace(policy_name, "_.*", ""),
           Date = lubridate::ymd(Date)) %>%
    rename(cnt = CountryCode, date = Date) %>%
    pivot_wider(names_from = flag, values_from = value, values_fill = list(value = -1)) %>%
    filter(date <= "2020-06-22",
           policy_name != "M1",
           cnt != "GIB") %>% 
    # policy_flag = -1 when policy has no flag (C8, E2, E3, E4, H3, H4, H5)
    # policy_value_max = maximum value that policy intervention can take
    # policy_value_ct = cumulative times policy intervention has been > 0
    # policy_value_cnt_max = maximum value that policy intervention can take 
    # in a country 
    # policy_value_cnt_ctmax = maximum value that cumulative policy 
    # intervention can take in a country
    # (used for E3, E4, H4, H5 which are listed as $ values)
    group_by(policy_name) %>% 
    mutate(policy_value = if_else(policy_name %in% c("E3","E4","H4","H5") & 
                                      policy_value > 0, 
                                  1, 
                                  policy_value)) -> policy_data

# Impute missing data
policy_data %>% 
    group_by(cnt, policy_name) %>% 
    arrange(date) %>% 
    nest() %>% 
    mutate(missing = map(data, ~any(is.na(.x$policy_value))) %>% unlist,
           all = map(data, ~all(is.na(.x$policy_value))) %>% unlist) %>% 
    group_by(missing) %>% 
    group_split() -> policy_data

policy_data[[2]] %>% 
    mutate(data = map(data, arrange, date),
           data = map(data, mutate, 
                      policy_value = imputeTS::na_interpolation(policy_value))) %>% 
    bind_rows(policy_data[[1]]) %>% 
    dplyr::select(-c(missing, all)) %>% 
    unnest(cols = data) -> policy_data
    
# discretise interventions measured by monetary values
policy_data %>% 
    group_by(cnt, policy_name) %>% 
    nest() %>% 
    mutate(change = if_else(policy_name %in% c("E3","E4","H4","H5"),
                            T,
                            F)) %>% 
    group_by(change) %>% 
    group_split -> policy_data

policy_data[[2]] %>% 
    mutate(x = map(data, ~cumsum(.x$policy_value))) %>% 
    unnest(cols = c(data, x)) %>% 
    mutate(policy_value = x) %>% 
    dplyr::select(-x) %>%
    bind_rows(policy_data[[1]] %>% 
                  unnest(cols = data)) -> policy_data

# set maximum and minimum values
policy_data %>% 
    group_by(policy_name) %>% 
    summarise(policy_value_max = max(policy_value, na.rm = T)) %>% 
    right_join(policy_data, by = "policy_name") %>% 
    arrange(cnt, policy_name) %>% 
    mutate(policy_value_max = if_else(policy_name %in% c("E3","E4","H4","H5"),
                                      1,
                                      policy_value_max),
           policy_value_min = 1) %>% 
    #filter(is.na(policy_value_max))
    mutate(policy_value_hi = if_else(policy_value < policy_value_max, 
                                     0, 
                                     1),
           policy_value_lo = if_else(policy_value > 0,
                                     1,
                                     0))-> policy_data

# policy_data %>% 
#     ggplot(., aes(x = date, 
#                   y = policy_value, 
#                   group = cnt))+
#     geom_line() +
#     geom_point() +
#     facet_wrap(~policy_name, scale = "free")

# Build other data sets
rt_estimates %<>% 
  mutate(cnt = countrycode(country, "country.name", "iso3c"),
         date = lubridate::ymd(date)) %>%
  as_tibble

hsize <- UN_household[,c(3,4,17)] %>% 
  setNames(c("cnt","hh_size","multi_gen"))

pop <- covid19_pkg_data %>% 
  dplyr::select(cnt, 
                pop_65,
                pop_density,
                pop_death_rate) %>% 
  distinct() %>% 
  drop_na()

urban <- urban_rural %>% 
  filter(!is.na(cnt)) %>% 
  dplyr::select(cnt, Percentage_urban) %>% 
  rename(urban = Percentage_urban)

# Join datasets for policy_data and rt_estimates into joined_data

# runs much faster if you define cnt_policy_r2 beforehand
cnt_policy_r2 <- intersect(unique(policy_data$cnt), unique(rt_estimates$cnt)) %>% .[!is.na(.)]

# policy_data %>%
#     left_join(rt_estimates, by = c("date", "cnt")) %>% 
#     dplyr::select(-X) %>% 
# #   inner_join(rt_estimates, by = c("date", "cnt")) %>% 
#     filter(cnt %in% cnt_policy_r2) %>%
#     ungroup %>% 
#     mutate(region = countrycode(cnt, "iso3c", "region", nomatch = NULL)) %>%
#     filter(!is.na(region)) -> joined_data

# Pivot to wide datasets and set factors
policy_data %>% 
    dplyr::select(-c(policy_flag, 
                     change,
                     policy_value, 
                     policy_value_max,
                     policy_value_lo, 
                     policy_value_min)) %>% 
    pivot_wider(names_from = policy_name, values_from = policy_value_hi) %>% 
    mutate_at(vars(policy_dic$policy_code),
            ~factor(., ordered = T), levels = 0:1) %>% 
    group_by(cnt, date) %>% 
    left_join(rt_estimates %>% 
                  dplyr::select(-c(X, country)),
              by = c("cnt", "date")) %>% 
    mutate_at(vars(policy_dic$policy_code),
              ~factor(., ordered = T, levels = 0:1)) -> joined_hi

policy_data %>% 
    dplyr::select(-c(policy_flag, 
                     change,
                     policy_value, 
                     policy_value_max,
                     policy_value_hi, 
                     policy_value_min)) %>% 
    pivot_wider(names_from = policy_name, values_from = policy_value_lo) %>% 
    mutate_at(vars(policy_dic$policy_code),
              ~factor(., ordered = T), levels = 0:1) %>% 
    group_by(cnt, date) %>% 
    left_join(rt_estimates %>% 
                  dplyr::select(-c(X, country)),
              by = c("cnt", "date")) %>% 
    mutate_at(vars(policy_dic$policy_code),
              ~factor(., ordered = T, levels = 0:1)) -> joined_lo

policy_data %>% 
    dplyr::select(-c(policy_flag, 
                     change,
                     policy_value_max,
                     policy_value_hi, 
                     policy_value_min,
                     policy_value_lo)) %>% 
    pivot_wider(names_from = policy_name, values_from = policy_value) %>%
    left_join(rt_estimates %>% 
                  dplyr::select(-c(X, country)),
              by = c("cnt", "date")) %>% 
    mutate_at(vars(policy_dic$policy_code),
              ~zoo::na.locf(.) %>% round) %>% # filter(cnt == "HRV") %>% View()
    mutate_at(vars(C1, C2, C6, H2),
              ~factor(., ordered = T, levels = 0:3)) %>%
    mutate_at(vars(C4, C8),
              ~factor(., ordered = T, levels = 0:4))  %>%
    mutate_at(vars(C3, C5, C7, E1, E2, H1, H3),
              ~factor(., ordered = T, levels = 0:2)) -> joined_mid

# joined_data_cov has country-level covariates for random effects model
covariates <- hsize %>%
    full_join(pop, by = "cnt") %>% 
    full_join(urban, by = "cnt") %>% 
    drop_na() %>%
    filter(cnt %in% unique(joined_mid$cnt)) %>%
    mutate_at(vars(-cnt), .funs = function(x) ntile(x, 5)) %>%
    mutate_at(vars(hh_size, multi_gen, pop_65, pop_density, pop_death_rate, urban),
              ~factor(., ordered = T))

joined_mid_cov <- covariates %>% 
    full_join(joined_mid, by = "cnt") %>%
    drop_na(colnames(covariates))

joined_hi_cov <- covariates %>% 
    full_join(joined_hi, by = "cnt") %>%
    drop_na(colnames(covariates))

joined_lo_cov <- covariates %>% 
    full_join(joined_lo, by = "cnt") %>%
    drop_na(colnames(covariates))

joined_data_cov <- covariates %>% 
    full_join(joined_mid, by = "cnt") %>%
    drop_na(colnames(covariates)) %>% 
    left_join(covid19_pkg_data %>% 
                  select(cnt, date, walking, driving, transit),
              by = c("date", "cnt"))

# save
saveRDS(list(hi = joined_hi, lo = joined_lo, mid = joined_mid,
        hi_cov = joined_hi_cov, lo_cov = joined_lo_cov, mid_cov = joined_mid_cov,
             stringency = stringency, policy_dic = policy_dic), 
    here("data", "joined_all.RDS"))
