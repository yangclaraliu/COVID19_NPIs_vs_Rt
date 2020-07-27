find_lag <- function(E, model_type = "within", lagrg = -21:-1){
    
    f_lags <- sapply(
        lagrg,
        function(t) paste0("plm::lag(median, ", t, ")~", 
                           paste(policy_raw, collapse = " + ")))
    model_lags <- f_lags %>% 
        map(as.formula) %>% 
        map(plm, data = E, model = model_type)
    
    lag_diagnostic <- model_lags %>%
        map_dfr(aicbic_plm) %>% 
        mutate(lags = c(-21:-1))
    
    return(list(model_lags = model_lags, lag_diagnostic = lag_diagnostic))
}
