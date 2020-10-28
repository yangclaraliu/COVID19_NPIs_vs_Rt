select_var_forward <- function(lag, model_type, criterion, data){
  # initialise
  require(tidyverse)
  require(magrittr)

  lags_range = -0:-21
  f_tmp <- rep(0, length(policy_raw))
  f_index <- list()
  for(i in 1:length(f_tmp)){
    f_index[[i]] <- f_tmp; f_index[[i]][i] <- 1
  }
  f_lags <- sapply(lags_range, 
                   function(t) paste0("plm::lag(median, ", t, ") ~ "))
  f_index %>% 
    map(~which(. == 1)) %>% 
    map(~policy_raw[.]) %>%
    map(~paste0(f_lags, .)) -> f_all
  lag_index_tmp <- which(lags_range == lag)
  f_all %>% map(~.[lag_index_tmp]) -> f_tmp
  f_tmp %>% 
    map(as.formula) %>% 
    map(plm, 
        data = data,
        model = model_type) -> g
  g %>% map(aicbic_plm) -> diagnostics
  
  diagnostics %>% 
    bind_rows(.id = "id") %>% 
    mutate(rk = rank(!!sym(criterion))) %>% 
    filter(rk == 1) %>% 
    pull(id) %>% 
    as.numeric -> chosen_index

  res <- list(optim_stats = rep(NA,length(f_index)),
              model = list(),
              var_combo = list())
  
  res$var_combo[[1]]   <- f_index[[chosen_index]]
  res$model[[1]]       <- g[[chosen_index]]
  res$optim_stats[[1]] <- diagnostics[[chosen_index]][[criterion]]
  
  #
  for(i in 2:13){
    base <- res$var_combo[[i-1]]
    to_mod <- which(res$var_combo[[i-1]] != 1)
    f_index <- list()
    for(j in 1:length(to_mod)) {
      tmp <- base
      tmp[to_mod[j]] <- 1
      f_index[[j]] <- tmp
      rm(tmp)
    }
    f_index %>% 
      map(~which(. == 1)) %>% 
      map(~policy_raw[.]) %>%
      map(~paste0(., collapse = " + ")) %>% 
      map(~paste0(f_lags, .)) -> f_all
    f_all %>% map(~.[lag_index_tmp]) -> f_tmp
    f_tmp %>% 
      map(as.formula) %>% 
      map(plm, 
          data = data,
          model = model_type) -> g
    g %>% map(aicbic_plm) -> diagnostics
    
    diagnostics %>% 
      bind_rows(.id = "id") %>% 
      mutate(rk = rank(!!sym(criterion), 
                       ties.method = "random")) %>% 
      filter(rk == 1) %>% 
      pull(id) %>% 
      as.numeric -> chosen_index
    
    res$var_combo[[i]]   <- f_index[[chosen_index]]
    res$model[[i]]       <- g[[chosen_index]]
    res$optim_stats[[i]] <- diagnostics[[chosen_index]][[criterion]]
  }
 
  # # initialise loop
  # remain <- which(f_index != 0)
  # f_index_tmp <- f_index
  # f_index_tmp %>% 
  #   t %>% 
  #   data.frame() %>% 
  #   slice(rep(1,
  #             length(f_index_tmp))) -> test_grid
  # for(i in 1:length(f_index_tmp)) test_grid[i,remain[i]] <- 0
  # 
  # lapply(1:nrow(test_grid), function(x){
  #   paste0("lag(median,", lag, ") ~ ",
  #          paste(policy_raw[which(test_grid[x,] == 1)], collapse = " + "))
  # }) %>% 
  #   map(as.formula) %>% 
  #   map(plm, data = data, model = model_type) -> models_tmp
  # stats_tmp <- models_tmp %>% 
  #   map_dfr(aicbic_plm)
  # 
  # res$optim_stats[length(f_index)-1] <- min(stats_tmp[[criterion]])
  # res$model[[length(f_index)-1]] <- models_tmp[[which.min(stats_tmp[[criterion]]) ]]
  # res$var_combo[[length(f_index)-1]] <- test_grid[which.min(stats_tmp[[criterion]]),]
  # f_index_tmp <- test_grid[which.min(stats_tmp[[criterion]]),] 
  # 
  # # loop backwards
  # for(i in (length(f_index)-2):1){
  #   remain <- which(f_index_tmp != 0)
  #   test_grid <- f_index_tmp %>% 
  #     slice(rep(1,length(remain)))
  #   for(j in 1:nrow(test_grid)) test_grid[j,remain[j]] <- 0
  #   lapply(1:nrow(test_grid), function(x){
  #     paste0("lag(median,", lag, ") ~ ",
  #            paste(policy_raw[which(test_grid[x,] == 1)], collapse = " + "))
  #   }) %>% 
  #     map(as.formula) %>% 
  #     map(plm, data = data, model = model_type) -> models_tmp
  #   stats_tmp <- models_tmp %>% 
  #     map_dfr(aicbic_plm)
  #   min(stats_tmp[[criterion]])  -> res$optim_stats[i]
  #   models_tmp[[which.min(stats_tmp[[criterion]]) ]] -> res$model[[i]]
  #   test_grid[which.min(stats_tmp[[criterion]]),] -> res$var_combo[[i]]
  #   f_index_tmp <- test_grid[which.min(stats_tmp[[criterion]]),]  
  # }
  return(res)
}