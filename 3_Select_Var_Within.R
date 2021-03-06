# Reload data
if(!exists("joined")) joined <- here("v2", "joined_all.RDS") %>% readRDS
policy_raw <- joined$hi %>% 
  colnames %>% 
  str_subset("[A-Z][0-9]") %>%
  .[!.%in% c("M1","E3","E4","H4","H5")]

# get available data list
unique(joined$hi$cnt) %>% 
    enframe(value = "cnt") %>% 
    mutate(country_name = countrycode::countrycode(cnt, "iso3c", "country.name"),
           region = countrycode::countrycode(cnt, "iso3c", "region")) -> country_list

# generate data frame for panel regression
regions_dic <- country_list$region %>% unique %>% sort
data_low <- gen_regress_data(scen = "Low", max_date = "2020-06-22", reg = regions_dic)
data_mid <- gen_regress_data(scen = "Mid", max_date = "2020-06-22", reg = regions_dic)
data_hi <- gen_regress_data(scen = "High", max_date = "2020-06-22", reg = regions_dic)

all_lags_low <- calc_all_lags("Low", max_date = "2020-06-22")
all_lags_mid <- calc_all_lags("Mid", max_date = "2020-06-22")
all_lags_hi  <- calc_all_lags("High", max_date = "2020-06-22")

bind_rows(`Any effort scenario` = all_lags_low,
          `Multilevel effort scenario` = all_lags_mid, 
          `Maximum effort scenario` = all_lags_hi, 
          .id = "scenario") %>%
  # filter(scenario == "Multilevel effort scenario") %>% 
  # pull(scenario) %>% table
  plot_all_lags +
  scale_color_nejm()

# ggsave(filename = here("figs", "fig3.png"),
#          width = 20, 
#          height = 10)

all_lags_low <- calc_all_lags("Low", max_date = "2020-04-15")
all_lags_mid <- calc_all_lags("Mid", max_date = "2020-04-15")
all_lags_hi  <- calc_all_lags("High", max_date = "2020-04-25")

bind_rows(`Any effort scenario` = all_lags_low,
          `Multilevel effort scenario` = all_lags_mid, 
          `Maximum effort scenario` = all_lags_hi, 
          .id = "scenario") %>%
    plot_all_lags +
  scale_color_nejm()

# here("figs", "fig3_truncated.png") %>%
#     ggsave(width = 20, height = 10)

# already kinda did this, but let's double check

# joined$policy_dic$policy_code %>% 
#   .[!. %in% c("E3", "E4", "H4", "H5")] %>% 
#   paste(., collapse = " + ") %>% 
#   paste0("lag(median,", c(0, -1, -5, -10), ") ~ ",.) -> f
#          
# f %>% 
#   map(as.formula) %>% 
#   map(plm, data = data_hi, model = "within") -> g1
# 
# f %>% 
#   map(as.formula) %>% 
#   map(plm, data = data_hi, model = "random") -> g2
# 
# map2(g1, g2, phtest)

# f <- f_lags[optim_lag_index]
# g1 <- plm(as.formula(f), data = E, model = "within")
# g2 <- plm(as.formula(f), data = E, model = "random")
# phtest(g1,g2)
# detect.lindep(g1)
# punbalancedness(g1)

# selected model
expand.grid(scen = c("Low", "Mid", "High"), 
            optim_lag = c(-1, -5, -10), 
            max_date = c(as.Date("2020-04-13"), as.Date("2020-06-22"))) %>% 
    mutate(select = NA) -> res_tab -> res_tab_forward

pb <- progress::progress_bar$new(
    format = "[:bar] :current/:total (:percent in :elapsed) eta: :eta",
    total = nrow(res_tab),
    width = 80)

# backward selection
for(i in 1:nrow(res_tab)){
    data_tmp <- gen_regress_data(scen = res_tab$scen[i], 
                                 max_date = res_tab$max_date[i], 
                                 reg = regions_dic)
    res_tab$select[i] <- list(aicbic_select(optim_lag = res_tab$optim_lag[i], 
                                            data = data_tmp))
    pb$tick()
}

lapply(res_tab$select,"[[",3) %>% 
    map(mutate, rk_AIC = rank(AIC_val)) %>% 
    map(mutate, rk_BIC = rank(BIC_val, ties.method = "first")) %>% 
    map(filter, rk_AIC == 1| rk_BIC == 1) %>% 
    map(dplyr::select, model, rk_AIC, rk_BIC) %>% 
    map(pivot_longer, cols =starts_with ("rk"), names_to = "criterion") %>% 
    map(filter, value == 1) %>% 
    bind_rows(.id = "set") %>% 
    dplyr::select(-value) %>% 
    mutate(criterion = gsub("rk_","",criterion)) -> chosen

# save(res_tab, res_tab_forward, file = "results/res_tab.rdata")
# extract the p-values

res_tab[1:3] %>% 
  mutate(set = 1:n() %>% 
           as.character) %>% 
  left_join(chosen, by = "set") %>% 
  filter(scen != "Mid") -> p_table

p_val <- list()
for(i in p_table$set %>% unique){
  p_table %>% 
    arrange(set, criterion) %>% 
    filter(set == i) %>% 
    pull(model) -> model_selected
  
  sum_AIC <- summary(res_tab$select[[as.numeric(i)]][["AIC_back"]]$model[model_selected[1]][[1]])
  
  sum_AIC$coefficients[,4] %>% 
    enframe %>%
    setNames(c("var","p_val")) %>% 
    mutate(p_lab = case_when(p_val > 0.05 ~ "ns",
                             p_val <= 0.05 & p_val > 0.01 ~ "*",
                             p_val <= 0.01 & p_val > 0.001 ~ "**",
                             p_val <= 0.001 ~ "***"),
           set = i,
           criterion = "AIC") -> res_1
  
  sum_BIC <- summary(res_tab$select[[as.numeric(i)]][["BIC_back"]]$model[model_selected[2]][[1]])
  
  sum_BIC$coefficients[,4] %>% 
    enframe %>%
    setNames(c("var","p_val")) %>% 
    mutate(p_lab = case_when(p_val > 0.05 ~ "ns",
                             p_val <= 0.05 & p_val > 0.01 ~ "*",
                             p_val <= 0.01 & p_val > 0.001 ~ "**",
                             p_val <= 0.001 ~ "***"),
           set = i,
           criterion = "BIC") -> res_2
  
  p_val[[i]] <- rbind(res_1, res_2)
  
}

p_val %<>% bind_rows()

# 
lapply(lapply(res_tab$select,"[[",1),"[[","var_combo") -> tmp 
lapply(1:18, function(x) tmp[[x]] %>% do.call("rbind", .)) %>% 
    map(mutate, model = 1:n()) %>% 
    bind_rows(.id = "set") -> AIC_panel
lapply(lapply(res_tab$select,"[[",2),"[[","var_combo") -> tmp 
lapply(1:18, function(x) tmp[[x]] %>% do.call("rbind", .)) %>% 
    map(mutate, model = 1:n()) %>% 
    bind_rows(.id = "set") -> BIC_panel

chosen[chosen$criterion=="AIC",] %>% 
    left_join(AIC_panel, by = c("set","model")) %>% 
    pivot_longer(cols = starts_with("X"), names_to = "var") %>% 
    bind_rows(chosen[chosen$criterion=="BIC",] %>% 
                  left_join(BIC_panel, by = c("set","model")) %>% 
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
                                        "Full"))) -> var_select_res

var_select_res %>%
  filter(scen != "Multi-level Efforts") %>% 
  left_join(p_val %>% 
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
         fill = "", 
         color = "Intervention Category") +
    guides(color = guide_legend(override.aes = list(alpha = 1,
                                                    size = 3),
                                nrow = 2)) -> p4
    
here("figs", "fig4_v3.png") %>%
    ggsave(width = 16, height = 10, plot = p4)

lapply(1:nrow(chosen), function(i) {
    res_tab$select[[as.numeric(chosen$set[i])]] %>% 
           .[[(paste0(chosen$criterion[i], "_back"))]] %>% 
           .[["model"]] %>% 
           .[[chosen$model[i]]]
    }) -> chosen_models

chosen_models %>% 
    map(broom::tidy) %>% 
    bind_rows(.id = "id") %>% 
    left_join(chosen %>% mutate(id = 1:n() %>% as.character),
              by = "id") %>% 
    left_join(res_tab[,1:3] %>% mutate(set = 1:n() %>% as.character), by = "set") %>% 
    separate(., term, into = c("var","dim"), sep = "\\.") -> eff_size

eff_size %>% 
    filter(dim != "L") %>% 
    dplyr::select(id, set) %>% 
    distinct() %>% 
    left_join(res_tab[,1:3] %>% mutate(set = as.character(1:n())), by = "set")

eff_size %>% 
  filter(dim == "L",
         scen != "Mid") %>% 
  left_join(joined$policy_dic %>% 
              filter(policy_code %in% policy_raw) %>% 
              dplyr::select(policy_code, cat, lab) %>% 
              mutate(policy_no = 1:n()),
            by = c("var" = "policy_code")) %>%
  mutate(x = estimate - 1.96*std.error,
         xend = estimate + 1.96*std.error,
         optim_lag = factor(optim_lag,
                            levels = c(-1, -5, -10),
                            labels = c("1 day", "5 days", "10 days")),
         max_date = factor(max_date,
                           levels = c("2020-04-13",
                                      "2020-06-22"),
                           labels = c("Truncated",
                                      "Full Time-series")),
         scen = factor(scen,
                       levels = c("Low","Mid","High"),
                       labels = c("Any Efforts",
                                  "Multi-level Efforts",
                                  "Max. Efforts")),
         var = factor(var,
                      labels = joined$policy_dic %>% filter(policy_code %in% policy_raw) %>% pull(lab)),
         stat_sig = case_when(x >= 0 ~ "Significantly Positive",
                              xend <0 ~ "Significantly Negative",
                              TRUE ~ "Null Effect") %>% 
           factor(., levels = c("Significantly Negative",
                                "Null Effect",
                                "Significantly Positive"))) %>% 
  # filter(var %in% joined$policy_dic$lab[1:7]) %>% 
  ggplot(.) +
  geom_pointrange(aes(x = optim_lag,
                      y = estimate,
                      ymin = x,
                      ymax = xend,
                      # color = interaction(optim_lag, criterion),
                      color = cat,
                      shape = stat_sig),
                  # position = position_dodge2(width = 0.5),
                  size = 0.75) +
  scale_alpha_manual(values = c(1,0.5, 0.2)) +
  scale_shape_manual(values = c(16, 1, 13)) +
  ggh4x::facet_nested(scen + max_date ~ var + criterion,
                      labeller = label_wrap_gen(multi_line = T,
                                                width = 12)) +
  scale_color_manual(values = c('#a6cee3',
                                '#1f78b4',
                                '#b2df8a',
                                '#33a02c'))+
  theme_bw()+
  # geom_vline(xintercept = 1.5, color = "snow2") +
  geom_hline(yintercept = 0, color = "black", linetype = 2) +
  theme(panel.grid = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 20),
        strip.background = element_rect(fill = NA),
        axis.text.x = element_text(vjust = 0.5,
                                   angle = 90,
                                   hjust = 1),
        axis.text    = element_text(size = 14),
        axis.title = element_text(size = 20),
        legend.text  = element_text(size = 20),
        strip.text = element_text(size = 18)) +
  labs(y = "Effects",
       x = "Temporal Lags",
       color = "Model Specification",
       shape = "Effect Type") +
  guides(color = guide_legend(nrow = 2),
         shape = guide_legend(nrow = 2)) -> p5

# here("figs/", "fig5.png") %>%
#     ggsave(width = 27, height = 10, plot = p5)

pacman::p_load(rnaturalearch)

res_tab[1:3] %>% 
  mutate(set = 1:n() %>% 
           as.character) %>% 
  left_join(chosen, by = "set") %>% 
  mutate(index = 1:n()) %>% 
  filter(scen != "Mid") %>% 
  pull(index) -> tar

chosen_models[tar] %>% 
  map(~.$residuals) %>% 
  map(data.frame) %>% 
  map(rownames_to_column) %>% 
  map(as_tibble) %>% 
  map(setNames, c("rowname", "value")) %>% 
  map(mutate, iso3c = substr(rowname,1,3),
      date = substr(rowname,5,14)) %>% 
  map(dplyr::select, -rowname) %>% 
  map(group_by, iso3c) %>% 
  map(summarise, value = mean(abs(value))) %>% 
  map(arrange, desc(value)) %>% 
  map(mutate, 
      region = countrycode::countrycode(iso3c,
                                        origin = "iso3c",
                                        destination = "region"),
      country = countrycode::countrycode(iso3c,
                                         origin = "iso3c",
                                         destination = "country.name")) %>% 
  map(rownames_to_column, var = "rank") %>% 
  bind_rows() -> r

r %>% 
  group_by(country, region) %>% 
  mutate(rank = as.numeric(rank)) %>% 
  summarise(rank = mean(rank),
            value = mean(value)) %>% 
  arrange(rank) %>% 
  tail(30) %>% 
  write_csv(path = "results/residuals_rank_tail.csv")
# %>% pull(region) %>% table

r %>% 
  group_by(country, region) %>% 
  mutate(rank = as.numeric(rank)) %>% 
  summarise(rank = mean(rank),
            value = mean(value)) %>% 
  arrange(rank) %>% 
  tail(30) %>% 
  pull(region) %>% table
