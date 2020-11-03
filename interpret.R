hcd %>% 
  map(pvclust::pvpick) %>% 
  map(~.$clusters) %>% 
  map(enframe) %>% 
  map(unnest, cols = c(value)) %>% 
  bind_rows(.id = "scen") %>% 
  mutate(scen = factor(scen, 
                       levels = c("lo","mid","hi"),
                       labels = c("Low","Mid","High")))-> clust_index

expand.grid(c(-1,-5,-10),
            c("AIC", "BIC"),
            c("2020-04-13", "2020-06-22")) %>% 
  setNames(c("optim_lag",
             "criterion",
             "max_date")) %>% 
  mutate(max_date = lubridate::ymd(max_date))-> test_grid


eff_size %>% 
  filter(scen != "Mid") %>% 
  group_by(scen, var) %>% 
  # tally() %>% View()
  group_split() %>% 
  map(right_join, 
      test_grid,
      by = c("optim_lag",
             "criterion",
             "max_date")) %>%
  map(mutate,
      var = zoo::na.locf(var),
      scen = zoo::na.locf(scen),
      UL = estimate + 1.96*std.error,
      LL = estimate - 1.96*std.error,
      UL_sign = UL/abs(UL),
      LL_sign = LL/abs(LL),
      stat = case_when(UL > 0 & LL > 0 ~ "pos",
                       UL < 0 & LL < 0 ~ "neg",
                       TRUE ~ "null")) %>% 
  map(filter,
      stat %in% c("pos", "null")) %>% 
  map(dplyr::select, var, scen, optim_lag, criterion, max_date, stat)

  dplyr::select(criterion, stat) %>% 
  filter(stat %in% c("pos", "null")) 
