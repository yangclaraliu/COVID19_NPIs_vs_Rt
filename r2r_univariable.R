g <- res_tab[,c("scen", "optim_lag", "max_date")] %>% 
  mutate(set = 1:n()) %>% 
  filter(scen != "Mid")

uni_res <- list()

for(i in 1:nrow(g)){
  if(g$scen[i] == "High") tmp <- joined$hi
  if(g$scen[i] == "Low") tmp <- joined$lo
  
  tmp %>% 
    ungroup %>% 
    filter(date <= g$max_date[i]) %>% 
    left_join(country_list %>% dplyr::select(-name), by = "cnt") -> tmp
  
  tmp <- pdata.frame(tmp, index=c("cnt","date"), drop.index=TRUE, row.names=TRUE)
  
  joined$policy_dic$policy_code %>% 
    .[! . %in% c("H4", "H5", "E3", "E4")] -> var_names
  
  tmp_f <- paste0("lag(median,", g$optim_lag[i], ") ~ ", var_names)
  
  tmp_f %>% 
    map(as.formula) %>% 
    map(plm, data = tmp, model = "within") %>% 
    map(summary) %>% 
    map(., ~.$coefficients) %>% 
    do.call("rbind",.) %>%
    data.frame() %>%
    rownames_to_column(var = "varname") %>% 
    mutate(set = g$set[i]) -> uni_res[[i]]
  
}

uni_res %>%
  bind_rows() %>% 
  as_tibble() %>% 
  mutate(varname = gsub(".L","",varname)) %>%
  left_join(g, by = "set") %>% 
  mutate(set = factor(set),
         combo = case_when(optim_lag == -1 & max_date == "2020-04-13" ~ "Lag1\nTruncated",
                           optim_lag == -5 & max_date == "2020-04-13" ~ "Lag5\nTruncated",
                           optim_lag == -10 & max_date == "2020-04-13" ~ "Lag10\nTruncated",
                           optim_lag == -1 & max_date == "2020-06-22" ~ "Lag1\nFull",
                           optim_lag == -5 & max_date == "2020-06-22" ~ "Lag5\nFull",
                           optim_lag == -10 & max_date == "2020-06-22" ~ "Lag10\nFull"),
         combo = factor(combo,
                        levels = c("Lag1\nTruncated",
                                   "Lag5\nTruncated",
                                   "Lag10\nTruncated",
                                   "Lag1\nFull",
                                   "Lag5\nFull",
                                   "Lag10\nFull")),
         scen = if_else(scen == "Low", "Any effort", "Max. Effort")) %>%
  left_join(joined$policy_dic %>% dplyr::select(policy_code, lab),
            by = c("varname" = "policy_code")) %>% 
  mutate(varname = paste0(varname, ": ", lab)) %>% 
  ggplot(., aes(x = combo, 
                y = reorder(varname, 
                            desc(varname)))) +
  geom_tile(aes(fill = Estimate)) +
  geom_text(aes(label = round(Estimate,2)),
            size = 8) + 
  facet_wrap(~scen) +
  theme_cowplot() +
  labs(x = "",
       y = "",
       fill = "Coefficients") +
  theme(strip.background = element_rect(fill = NA),
        # legend.position = "top",
        title = element_text(size = 15),
        axis.text = element_text(size = 15),
        strip.text = element_text(size = 20, face = 3)) +
  ggsci::scale_fill_material("teal",
                             reverse = T) -> stack_estimates

uni_res %>%
  bind_rows() %>% 
  as_tibble() %>% 
  mutate(varname = gsub(".L","",varname)) %>%
  left_join(g, by = "set") %>% 
  mutate(set = factor(set),
         combo = case_when(optim_lag == -1 & max_date == "2020-04-13" ~ "Lag1\nTruncated",
                           optim_lag == -5 & max_date == "2020-04-13" ~ "Lag5\nTruncated",
                           optim_lag == -10 & max_date == "2020-04-13" ~ "Lag10\nTruncated",
                           optim_lag == -1 & max_date == "2020-06-22" ~ "Lag1\nFull",
                           optim_lag == -5 & max_date == "2020-06-22" ~ "Lag5\nFull",
                           optim_lag == -10 & max_date == "2020-06-22" ~ "Lag10\nFull"),
         combo = factor(combo,
                        levels = c("Lag1\nTruncated",
                                   "Lag5\nTruncated",
                                   "Lag10\nTruncated",
                                   "Lag1\nFull",
                                   "Lag5\nFull",
                                   "Lag10\nFull")),
         scen = if_else(scen == "Low", "Any effort", "Max. Effort")) %>%
  rename(p = `Pr...t..`) %>% 
  mutate(p_cat = case_when(p >= 0.05 ~ "ns",
                           p < 0.05 & p >= 0.01 ~ "*",
                           p < 0.01 & p >= 0.001 ~ "**",
                           p < 0.001 ~ "***"),
         p_cat = factor(p_cat, levels = c("ns","*","**","***"))) %>%
  left_join(joined$policy_dic %>% dplyr::select(policy_code, lab),
            by = c("varname" = "policy_code")) %>% 
  mutate(varname = paste0(varname, ": ", lab)) %>% 
  ggplot(., aes(x = combo, 
                y = reorder(varname, 
                            desc(varname)))) +
  geom_tile(aes(fill = p_cat), color = "black") +
  geom_text(aes(label = p_cat, color = p_cat), 
            size = 10,
            show.legend = FALSE) +
  facet_wrap(~scen) +
  theme_cowplot() +
  labs(x = "",
       y = "",
       fill = "p-values") +
  theme(strip.background = element_rect(fill = NA),
        # legend.position = "top",
        title = element_text(size = 15),
        axis.text = element_text(size = 15),
        strip.text = element_text(size = 20, face = 3)) +
  scale_fill_manual(drop = FALSE,
                    values = pal_material(palette = "teal")(10)[rev(seq(1,10,3))]) +
  scale_color_manual(values = c("white", "black")) -> stack_p

ggsave(
  filename = "figs/R2R/uni_estimates.png",
  plot = stack_estimates,
  width = 20,
  height = 10)

ggsave(
  filename = "figs/R2R/uni_p.png",
  plot = stack_p,
  width = 20,
  height = 10)
