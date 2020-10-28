library(tidyverse)
library(cowplot)
library(ggsci)

if(!exists("joined")) joined <- here("data", "joined_all.RDS") %>% readRDS

joined$hi %>% 
  group_by(cnt, date) %>% 
  dplyr::select(-c(median,starts_with(c("lower", "upper")))) %>% 
  mutate_at(vars(starts_with(c("C","E","H"), 
                             ignore.case = F)), 
            ~as.numeric(as.character(.))) -> tmp 

tmp %>% 
  pivot_longer(starts_with(c("C","E","H"), 
                           ignore.case = F)) %>%
  group_by(cnt, name) %>% 
  group_split() %>% 
  map(arrange, date) %>% 
  map(mutate, value_diff = c(NA, diff(value))) %>% 
  bind_rows() %>% 
  filter(value_diff != 0 & !is.na(value_diff)) %>% 
  ungroup %>% 
  mutate(marker = if_else(value_diff == 1, "Implementation", "Removal")) %>% 
  dplyr::select(-value, -value_diff) -> tmp

combinations(joined$policy_dic$policy_code %>% .[!. %in% c("E3", "E4","H4","H5")],
             2) %>% 
  as_tibble %>% 
  setNames(c("policy_code_1",
             "policy_code_2")) %>% 
  left_join(joined$policy_dic[,c("policy_code",
                                 "lab",
                                 "cat")],
            by = c("policy_code_1" = "policy_code")) %>% 
  rename(lab_1 = lab,
         cat_1 = cat) %>% 
  left_join(joined$policy_dic[,c("policy_code",
                                 "lab",
                                 "cat")],
            by = c("policy_code_2" = "policy_code")) %>% 
  rename(lab_2 = lab,
         cat_2 = cat) %>% 
  mutate(cluster_1 = case_when(lab_1 %in% c("Testing policy",
                                            "Stay at home requirements",
                                            "Income support",
                                            "Debt contract relief",
                                            "Contact tracing") ~ 1,
                               lab_1 %in% c("Close public transport",
                                            "Workplace closing") ~ 2,
                               lab_1 %in% c("Cancel public events",
                                            "School closing",
                                            "Public information campaigns") ~ 3,
                               TRUE ~ as.numeric(NA)),
         cluster_2 = case_when(lab_2 %in% c("Testing policy",
                                            "Stay at home requirements",
                                            "Income support",
                                            "Debt contract relief",
                                            "Contact tracing") ~ 1,
                               lab_2 %in% c("Close public transport",
                                            "Workplace closing") ~ 2,
                               lab_2 %in% c("Cancel public events",
                                            "School closing",
                                            "Public information campaigns") ~ 3,
                               TRUE ~ as.numeric(NA)),
         same_cluster = cluster_1 == cluster_2,
         same_cluster = if_else(is.na(same_cluster), F, same_cluster)) -> panel_grid

p_list <- list()
for(i in 1:nrow(panel_grid)){
  col_tmp <- "black"
  if(panel_grid$same_cluster[i] == T) col_tmp <- "#1B9E77"
  
  tmp %>% 
    group_by(cnt, marker) %>% 
    pivot_wider(names_from = name,
                values_from = date) %>% 
    unnest() %>% 
    ggplot(., aes(x = !!sym(panel_grid$policy_code_1[i]),
                  y = !!sym(panel_grid$policy_code_2[i]),
                  color = marker)) +
    geom_point(alpha = 0.4) +
    labs(x = panel_grid$policy_code_1[i],
         y = panel_grid$policy_code_2[i],
         color = "Time Marker") +
    scale_color_manual(values = c("#D95F02","#7570B3")) +
    theme_cowplot() +
    theme(axis.title = element_text(color = col_tmp)) +
    geom_abline(a = 0,
                b = 1) -> p_list[[i]]
}

start <- seq(1,nrow(panel_grid),20)
end <- c(start[-1] - 1, nrow(panel_grid))

for(i in 1:length(start)){
  do.call(ggpubr::ggarrange, c(p_list[start[i]:end[i]], 
                               ncol = 5,
                               nrow = 4,
                               common.legend = T)) -> p 
  ggsave(filename = paste0("figs/R2R/","figS2_",i,".png"),
         plot = p,
         width = 15,
         height = 10)
}








