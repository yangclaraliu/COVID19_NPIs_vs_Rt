library(tidyverse)
library(cowplot)
library(ggsci)

if(!exists("joined")) joined <- here("data", "joined_all.RDS") %>% readRDS

joined$lo %>% # change to $hi 
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

pvclust::pvpick(hcd$lo)$clusters %>% # change to $hi
  map(enframe) %>% 
  bind_rows(.id = "cluster_id") %>% 
  dplyr::select(-name) %>% 
  rename(policy_code = value) -> tmp_clusters

arrangements::combinations(joined$policy_dic$policy_code %>% .[!. %in% c("E3", "E4","H4","H5")],
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
  left_join(tmp_clusters, c("policy_code_1" = "policy_code")) %>% 
  rename(cluster_id_1 = cluster_id) %>% 
  left_join(tmp_clusters, c("policy_code_2" = "policy_code")) %>% 
  rename(cluster_id_2 = cluster_id) %>% 
  mutate(same_cluster = cluster_id_1 == cluster_id_2,
         same_cluster = if_else(is.na(same_cluster), F, same_cluster)) %>% 
  mutate(cluster_id = if_else(same_cluster == T, cluster_id_1, as.character(NA))) -> panel_grid

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
    theme_minimal() +
    geom_point(alpha = 0.4) +
    geom_abline(aes(intercept = 0,
                    slope = 1)) +
    labs(x = panel_grid$policy_code_1[i],
         y = panel_grid$policy_code_2[i],
         color = "Time Marker",
         title = paste0("Cluster#:", panel_grid[i,"cluster_id"] %>% pull)) +
    theme(axis.title = element_text(color = col_tmp),
          panel.border = element_rect(color = col_tmp, fill = NA),
          panel.grid = element_blank(),
          plot.title = element_text(color = col_tmp)) +
    scale_color_manual(values = c("#D95F02","#7570B3")) -> p_list[[i]]
}

start <- seq(1,nrow(panel_grid),20)
end <- c(start[-1] - 1, nrow(panel_grid))

for(i in 1:length(start)){
  do.call(ggpubr::ggarrange, c(p_list[start[i]:end[i]], 
                               ncol = 5,
                               nrow = 4,
                               common.legend = T)) -> p 
  ggsave(filename = paste0("figs/R2R/","figS2_lo_",i,".png"),
         plot = p,
         width = 15,
         height = 10)
}








