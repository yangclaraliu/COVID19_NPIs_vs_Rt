aicbic_select <- function(optim_lag, data){
    
    AIC_back <- select_var(lag = optim_lag,
                           model_type = "within",
                           criterion = "AIC",
                           data = data)
    BIC_back <- select_var(lag = optim_lag,
                           model_type = "within",
                           criterion = "BIC",
                           data = data)
    
    n <- length(policy_raw) #lazy option - change later
    indicators <- data.frame(
        model = 1:n,
        AIC_r2 = sapply(1:n, function(x) summary(AIC_back$model[[x]])$r.squared["rsq"]),
        AIC_ar2 = sapply(1:n, function(x) summary(AIC_back$model[[x]])$r.squared["adjrsq"]),
        AIC_val = AIC_back$optim_stats,
        BIC_r2 = sapply(1:n, function(x) summary(BIC_back$model[[x]])$r.squared["rsq"]),
        BIC_ar2 = sapply(1:n, function(x) summary(BIC_back$model[[x]])$r.squared["adjrsq"]),
        BIC_val = BIC_back$optim_stats
    )
    
    list(AIC_back = AIC_back, BIC_back = BIC_back, indicators = indicators)
}