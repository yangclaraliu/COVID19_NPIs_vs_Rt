select_var <- function(lag, model_type, criterion, data){
    # initialise
    
    lags_range = -0:-21
    f_index <- rep(1, length(policy_raw))
    f_lags <- sapply(lags_range, 
                     function(t) paste0("plm::lag(median, ", t, ")~", paste(policy_raw, collapse = " + ")))
    
    lag_index_tmp <- which(lags_range == lag)
    f_tmp <- f_lags[lag_index_tmp]#f_lags[optim_lag_index]
    g <- plm(as.formula(f_tmp), data = data, model = model_type)
    diagnostics <- aicbic_plm(g)
    res <- list(optim_stats = rep(NA,length(f_index)),
                model = list(),
                var_combo = list())
    res$var_combo[[length(f_index)]]   <- f_index
    res$model[[length(f_index)]]       <- g
    res$optim_stats[[length(f_index)]] <- aicbic_plm(g)[[criterion]]
    
    # initialise loop
    remain <- which(f_index != 0)
    f_index_tmp <- f_index
    f_index_tmp %>% 
        t %>% 
        data.frame() %>% 
        slice(rep(1,
                  length(f_index_tmp))) -> test_grid
    for(i in 1:length(f_index_tmp)) test_grid[i,remain[i]] <- 0
    
    lapply(1:nrow(test_grid), function(x){
        paste0("lag(median,", lag, ") ~ ",
               paste(policy_raw[which(test_grid[x,] == 1)], collapse = " + "))
    }) %>% 
        map(as.formula) %>% 
        map(plm, data = data, model = model_type) -> models_tmp
    stats_tmp <- models_tmp %>% 
        map_dfr(aicbic_plm)
    
    res$optim_stats[length(f_index)-1] <- min(stats_tmp[[criterion]])
    res$model[[length(f_index)-1]] <- models_tmp[[which.min(stats_tmp[[criterion]]) ]]
    res$var_combo[[length(f_index)-1]] <- test_grid[which.min(stats_tmp[[criterion]]),]
    f_index_tmp <- test_grid[which.min(stats_tmp[[criterion]]),] 
    
    # loop backwards
    for(i in (length(f_index)-2):1){
        remain <- which(f_index_tmp != 0)
        test_grid <- f_index_tmp %>% 
            slice(rep(1,length(remain)))
        for(j in 1:nrow(test_grid)) test_grid[j,remain[j]] <- 0
        lapply(1:nrow(test_grid), function(x){
            paste0("lag(median,", lag, ") ~ ",
                   paste(policy_raw[which(test_grid[x,] == 1)], collapse = " + "))
        }) %>% 
            map(as.formula) %>% 
            map(plm, data = data, model = model_type) -> models_tmp
        stats_tmp <- models_tmp %>% 
            map_dfr(aicbic_plm)
        min(stats_tmp[[criterion]])  -> res$optim_stats[i]
        models_tmp[[which.min(stats_tmp[[criterion]]) ]] -> res$model[[i]]
        test_grid[which.min(stats_tmp[[criterion]]),] -> res$var_combo[[i]]
        f_index_tmp <- test_grid[which.min(stats_tmp[[criterion]]),]  
    }
    return(res)
}