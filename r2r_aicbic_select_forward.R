aicbic_select_forward <- function(optim_lag, data){
  source("r2r_select_var_forward.R")
  AIC_forward <- select_var_forward(lag = optim_lag,
                                    model_type = "within",
                                    criterion = "AIC",
                                    data = data)
  BIC_forward <- select_var_forward(lag = optim_lag,
                                    model_type = "within",
                                    criterion = "BIC",
                                    data = data)
  n <- length(policy_raw) #lazy option - change later
  indicators <- data.frame(
    model = 1:n,
    AIC_r2 = sapply(1:n, function(x) summary(AIC_forward$model[[x]])$r.squared["rsq"]),
    AIC_ar2 = sapply(1:n, function(x) summary(AIC_forward$model[[x]])$r.squared["adjrsq"]),
    AIC_val = AIC_forward$optim_stats,
    BIC_r2 = sapply(1:n, function(x) summary(BIC_forward$model[[x]])$r.squared["rsq"]),
    BIC_ar2 = sapply(1:n, function(x) summary(BIC_forward$model[[x]])$r.squared["adjrsq"]),
    BIC_val = BIC_forward$optim_stats
  )
  
  list(AIC_forward = AIC_forward, BIC_forward = BIC_forward, indicators = indicators)
}