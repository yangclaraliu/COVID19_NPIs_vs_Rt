library(jsonlite)
library(tidyverse)
library(lubridate)

#' Function to create and update rt_estimates_<date>.csv based on Epiforecasts Rt estimates
read_epipose_rt_csv <- function()
{
    rt_url     <- "https://raw.githubusercontent.com/epiforecasts/covid-global/34310a5412e4f7448e3ee335a2a00260c9fd09a5/national-summary/rt.csv"
    rt_est_raw <- rt_url %>% read.csv
    rt_est     <- rt_est_raw %>% select(!type & !prob_control) 

    return(rt_est)
}

read_oxford_policy_csv <- function()
{
    action_url <- "https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv"

    action_data_raw <- action_url %>% read.csv
    action_data <- action_data_raw %>%
        as_tibble %>%
        select(CountryCode, Date, contains("_")) %>%
        pivot_longer(cols = contains("_"), names_to = "policy_name") %>%
        mutate(flag = ifelse(str_detect(policy_name, "_Flag"), "policy_flag", "policy_value")) %>%
        mutate(policy_name = str_replace(policy_name, "_.*", ""),
               Date = (Date %>% as.character %>% as_date)) %>%
        rename(cnt = CountryCode, date = Date) %>%
        pivot_wider(names_from = flag, values_from = value)
    
    stringency_data <- action_data_raw %>%
        as_tibble %>%
        mutate(Date = (Date %>% as.character %>% as_date)) %>%
        rename(cnt = CountryCode, date = Date, stringency = StringencyIndex) %>%
        select(cnt, date, stringency)

    return(list(action_data = action_data, stringency_data = stringency_data))    
}

create_plm_dataset_v3 <- function( rt_estimates,
                                   mobility_data,
                                   GDP_2019,
                                   stringency_data,
                                   UN_household,
                                   covid19_pkg_data,
                                   urban_rural)
{
    rt_estimates$cnt  <- unlist(lapply(rt_estimates$country, parse_iso_code))
    GDP_2019$cnt      <- unlist(lapply(GDP_2019$Country, parse_iso_code)) 
    
    mobility_data$cnt <- unlist(lapply(mobility_data$region, parse_iso_code))
    mobility_spread   <- mobility_data %>% pivot_wider(names_from = transportation_type, values_from = utilization)
    
    policy_spread     <- policy_data
    joined_data       <- 
        full_join(
            full_join(
                full_join(
                    policy_spread,rt_estimates, by = c("date", "cnt")), 
                stringency_data,by = c("date", "cnt")),
            mobility_spread,by = c("date", "cnt"))
    joined_data       <- joined_data[joined_data$cnt!="NA",]
    
    # For interventions represented by continuous (USD) instead of ordinal variables (E3, E4, H4, H5)
    # X_n   = variable divided by GDP (normalised to [0,1])
    # X_nc  = cumulative X_n between Jan 1 and current time
    # X_ind = 1 if X_nc > 0, 0 otherwise
    joined_data       <- joined_data %>% replace_na(list(E3=0))  %>% replace_na(list(H4=0))  %>% replace_na(list(H5=0))
    joined_data$E3_n  <- joined_data$E3_nc <- joined_data$E3_ind <- joined_data$E3
    joined_data$E4_n  <- joined_data$E4_nc <- joined_data$E4_ind <- joined_data$E4
    joined_data$H4_n  <- joined_data$H4_nc <- joined_data$H4_ind <- joined_data$H4
    joined_data$H5_n  <- joined_data$H5_nc <- joined_data$H5_ind <- joined_data$H5
    
    for( cnt_in in unique(joined_data$cnt)[-1])
    {
        if(dim(GDP_2019[GDP_2019$cnt==cnt_in,])[1]) GDP_in <- GDP_2019[GDP_2019$cnt==cnt_in,]$GDP else GDP_in <- 10e8  # proxy for small countries where we don't have GDP data
        joined_data[joined_data$cnt==cnt_in,]$E3_n         <- joined_data[joined_data$cnt==cnt_in,]$E3 / GDP_in 
        joined_data[joined_data$cnt==cnt_in,]$E4_n         <- joined_data[joined_data$cnt==cnt_in,]$E4 / GDP_in 
        joined_data[joined_data$cnt==cnt_in,]$H5_n         <- joined_data[joined_data$cnt==cnt_in,]$H5 / GDP_in 
        joined_data[joined_data$cnt==cnt_in,]$H4_n         <- joined_data[joined_data$cnt==cnt_in,]$H4 / GDP_in 
        
        if(cnt_in=="SVN") joined_data[joined_data$cnt==cnt_in,]$E3_n <- 0    # Oxford data has an issue for Slovenia at the moment, so set to zero for now
        
        joined_data[joined_data$cnt==cnt_in,]$E3_nc        <- cumsum(joined_data[joined_data$cnt==cnt_in,]$E3_n)
        joined_data[joined_data$cnt==cnt_in,]$E4_nc        <- cumsum(joined_data[joined_data$cnt==cnt_in,]$E4_n)
        
        joined_data[joined_data$cnt==cnt_in,]$E3_ind       <- as.numeric(cumsum(joined_data[joined_data$cnt==cnt_in,]$E3_n)>0)
        joined_data[joined_data$cnt==cnt_in,]$E4_ind       <- as.numeric(cumsum(joined_data[joined_data$cnt==cnt_in,]$E4_n)>0)
        
        joined_data[joined_data$cnt==cnt_in,]$H5_nc        <- cumsum(joined_data[joined_data$cnt==cnt_in,]$H5_n)
        joined_data[joined_data$cnt==cnt_in,]$H4_nc        <- cumsum(joined_data[joined_data$cnt==cnt_in,]$H4_n)
        
        joined_data[joined_data$cnt==cnt_in,]$H5_ind       <- as.numeric(cumsum(joined_data[joined_data$cnt==cnt_in,]$H5_n)>0)
        joined_data[joined_data$cnt==cnt_in,]$H4_ind       <- as.numeric(cumsum(joined_data[joined_data$cnt==cnt_in,]$H4_n)>0)
    }
}    