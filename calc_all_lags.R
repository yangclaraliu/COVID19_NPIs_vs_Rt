calc_all_lags <- function(scen_all_lags, max_date = "2020-06-22"){
    
    all_regions=list(
        "Middle East & North Africa", 
        "Latin America & Caribbean",
        "East Asia & Pacific", 
        "Europe & Central Asia", 
        "Sub-Saharan Africa",
        regions_dic)
    names(all_regions) <- all_regions %>% map(paste, collapse="\n")
    names(all_regions)[length(all_regions)] <- "World"
    
    all_regions %>%
        map(gen_regress_data, scen = scen_all_lags, max_date = max_date) %>% 
        map(find_lag) %>% 
        map(use_series, "lag_diagnostic") %>%
        bind_rows(.id = "region") %>%
        select(region, deviance, lags) -> tmp
    
    return(tmp)
}
