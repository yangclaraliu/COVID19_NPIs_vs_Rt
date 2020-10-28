pb <- progress::progress_bar$new(
  format = "[:bar] :current/:total (:percent in :elapsed) eta: :eta",
  total = nrow(res_tab),
  width = 80)

source("r2r_aicbic_select_forward.R")
# forward selection
for(i in 1:nrow(res_tab_forward)){
  data_tmp <- gen_regress_data(scen = res_tab_forward$scen[i], 
                               max_date = res_tab_forward$max_date[i], 
                               reg = regions_dic)
  res_tab_forward$select[i] <- list(aicbic_select_forward(optim_lag = res_tab_forward$optim_lag[i], 
                                                          data = data_tmp))
  pb$tick()
}
#
#
# lapply(res_tab_forward$select,"[[",3)[[1]] 
# lapply(res_tab$select,"[[",3)[[1]]

lapply(res_tab_forward$select,"[[",3) %>% 
  map(mutate, rk_AIC = rank(AIC_val)) %>% 
  map(mutate, rk_BIC = rank(BIC_val, ties.method = "first")) %>% 
  map(filter, rk_AIC == 1| rk_BIC == 1) %>% 
  map(dplyr::select, model, rk_AIC, rk_BIC) %>% 
  map(pivot_longer, cols =starts_with ("rk"), names_to = "criterion") #%>% 
  map(filter, value == 1) %>% 
  bind_rows(.id = "set") %>% 
  dplyr::select(-value) %>% 
  mutate(criterion = gsub("rk_","",criterion)) -> chosen_forward

#
  res_tab$select[[1]]$BIC_back$model[[9]]
  res_tab_forward$select[[1]]$BIC_forward$model[[9]]
  
  res_tab_forward[1:3] %>% 
    mutate(set = 1:n() %>% 
             as.character) %>% 
    left_join(chosen_forward, by = "set") %>% 
    filter(scen != "Mid") -> p_table_forward
  
  p_val_forward <- list()
  
  for(i in p_table_forward$set %>% unique){
    p_table_forward %>% 
      arrange(set, criterion) %>% 
      filter(set == i) %>% 
      pull(model) -> model_selected
    
    sum_AIC <- summary(res_tab_forward$select[[as.numeric(i)]][["AIC_forward"]]$model[model_selected[1]][[1]])
    
    sum_AIC$coefficients[,4] %>% 
      enframe %>%
      setNames(c("var","p_val")) %>% 
      mutate(p_lab = case_when(p_val > 0.05 ~ "ns",
                               p_val <= 0.05 & p_val > 0.01 ~ "*",
                               p_val <= 0.01 & p_val > 0.001 ~ "**",
                               p_val <= 0.001 ~ "***"),
             set = i,
             criterion = "AIC") -> res_1
    
    sum_BIC <- summary(res_tab_forward$select[[as.numeric(i)]][["BIC_forward"]]$model[model_selected[2]][[1]])

    sum_BIC$coefficients[,4] %>% 
      enframe %>%
      setNames(c("var","p_val")) %>% 
      mutate(p_lab = case_when(p_val > 0.05 ~ "ns",
                               p_val <= 0.05 & p_val > 0.01 ~ "*",
                               p_val <= 0.01 & p_val > 0.001 ~ "**",
                               p_val <= 0.001 ~ "***"),
             set = i,
             criterion = "BIC") -> res_2
    
    p_val_forward[[i]] <- rbind(res_1, res_2)
    
  }
  
  p_val_forward %<>% bind_rows()
  # 
  lapply(lapply(res_tab_forward$select,"[[",1),"[[","var_combo") -> tmp 
  lapply(1:18, function(x) tmp[[x]] %>% do.call("rbind", .)) %>%
    map(data.frame) %>% 
    map(mutate, model = 1:n()) %>% 
    bind_rows(.id = "set") -> AIC_panel_forward
  lapply(lapply(res_tab_forward$select,"[[",2),"[[","var_combo") -> tmp 
  lapply(1:18, function(x) tmp[[x]] %>% do.call("rbind", .)) %>% 
    map(data.frame) %>% 
    map(mutate, model = 1:n()) %>% 
    bind_rows(.id = "set") -> BIC_panel_forward

  #
  
  chosen_forward[chosen_forward$criterion=="AIC",] %>% 
    left_join(AIC_panel_forward, by = c("set","model")) %>% 
    pivot_longer(cols = starts_with("X"), names_to = "var") %>% 
    bind_rows(chosen_forward[chosen_forward$criterion=="BIC",] %>% 
                left_join(BIC_panel_forward, by = c("set","model")) %>% 
                pivot_longer(cols = starts_with("X"), names_to = "var")) %>%  
    mutate(var = parse_number(var),
           value = factor(value)) %>% 
    left_join(joined$policy_dic %>% 
                filter(policy_code %in% policy_raw) %>% 
                mutate(var = 1:n()),
              by = "var") %>% 
    left_join(res_tab[,1:3] %>% 
                mutate(set = 1:n() %>% as.character),
              by = "set") %>% 
    mutate(lags = case_when(optim_lag == -1 ~ 1,
                            optim_lag == -5 ~ 2,
                            optim_lag == -10 ~ 3),
           scen = factor(scen, 
                         levels = c("Low","Mid","High"),
                         labels = c("Any Efforts",
                                    "Multi-level Efforts",
                                    "Max. Efforts")),
           max_date = factor(max_date,
                             levels = c("2020-04-13",
                                        "2020-06-22"),
                             labels = c("Truncated",
                                        "Full"))) -> var_select_res_forward

  # plotting
  var_select_res_forward %>%
    filter(scen != "Multi-level Efforts") %>% 
    left_join(p_val_forward %>% 
                separate(var, 
                         sep = "\\.", 
                         into = c("policy_code", "component")),
              by = c("policy_code",
                     "set",
                     "criterion")) %>% 
    #filter(max_date == "2020-04-13") %>% 
    ggplot(.)  +
    geom_rect(aes(xmin = lags-0.5,
                  ymin = var-0.5, 
                  xmax = lags+0.5,
                  ymax = var+0.5, 
                  fill = value)) +
    geom_text(aes(x = lags,
                  y = var,
                  label = p_lab)) + 
    # facet_grid(scen ~ max_date + criterion) +
    ggh4x::facet_nested(scen ~ max_date + criterion) +
    geom_point(aes(x = 1, y = 5, color = cat), alpha = 0) +
    scale_color_manual(values = c('#a6cee3',
                                  '#1f78b4',
                                  '#b2df8a',
                                  '#33a02c')) +
    geom_segment(data = data.frame(y = rep(0.5,4),
                                   yend = rep(13.5,4),
                                   x = seq(0.5,3.5,1),
                                   xend = seq(0.5,3.5,1)),
                 aes(x = x, xend = xend, y = y, yend = yend)) +
    geom_segment(data = data.frame(y = seq(0.5,13.5,1),
                                   yend = seq(0.5,13.5,1),
                                   x = rep(0.5,14),
                                   xend = rep(3.5,14)),
                 aes(x = x, xend = xend, y = y, yend = yend)) +
    theme_cowplot() +
    # theme_bw() +
    scale_y_continuous(breaks = seq(1,13,1),
                       labels = joined$policy_dic %>% 
                         filter(policy_code %in% policy_raw) %>% 
                         pull(lab),
                       trans = "reverse") +
    scale_x_continuous(breaks = c(1,2,3),
                       labels = c("-1", "-5", "-10")) +
    theme(axis.text    = element_text(size = 20),
          axis.title = element_text(size = 20),
          legend.text  = element_text(size = 20),
          legend.title = element_text(size = 20),
          legend.position = "bottom",
          strip.background = element_rect(fill = "white", 
                                          color = "black"),
          axis.text.y = element_text(color = c(rep('#a6cee3',7),
                                               '#b2df8a',
                                               rep('#1f78b4',2),
                                               rep('#33a02c',3))),
          legend.box = "vertical",
          legend.box.just = "left",
          legend.margin=margin(),
          text = element_text(family = "Times New Roman"),
          strip.text.y = element_text(face = "italic"),
          strip.text = element_text(size = 20)) +
    scale_fill_manual(values = c("snow2","darkgrey"),
                      labels = c("Variable Excluded","Variable Chosen")) +
    labs(x = "Temporal Lags", 
         y = "", 
         fill = "Optimal Model", 
         color = "Intervention Category") +
    guides(color = guide_legend(override.aes = list(alpha = 1,
                                                    size = 3),
                                nrow = 2)) -> p4_forward
  
  here("figs/R2R", "fig4_forward.png") %>%
    ggsave(width = 16, height = 10, plot = p4_forward)
  