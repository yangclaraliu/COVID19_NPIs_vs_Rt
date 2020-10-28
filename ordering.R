if(!exists("joined")) joined <- here("v2", "joined_all.RDS") %>% readRDS
policy_raw <- joined$hi %>% 
  colnames %>% 
  str_subset("[A-Z][0-9]") %>%
  .[!.%in% c("M1","E3","E4","H4","H5")]

# Any effort scenario
policy_begin <- joined$lo %>% 
  dplyr::select(cnt, date, policy_raw) %>% 
  pivot_longer(cols = policy_raw) %>% 
  filter(value != 0) %>% 
  group_by(cnt, name) %>% 
  mutate(rk = rank(date)) %>% 
  filter(rk == 1)  %>%
  dplyr::select(-value,
                -rk) %>% 
  group_by(cnt) %>% 
  mutate(rk = rank(date))

policy_begin %>% pull(name) %>% table

policy_begin %>%
  group_by(cnt) %>% 
  left_join(joined$policy_dic, by = c("name" = "policy_code")) %>%
  mutate(lab = factor(lab, 
                      levels = joined$policy_dic$lab)) %>%
  group_by(lab) %>% 
  nest() %>% 
  mutate(skewness = map(data, ~e1071::skewness(.$rk)) %>% unlist,
         skew_cat = case_when(abs(skewness) > 1 ~ "Highly Skewed",
                              abs(skewness) <= 1 & abs(skewness) >= 0.5 ~ "Moderately Skewed",
                              abs(skewness) < 0.5 ~ "Roughly Symmetric"),
         md = map(data, ~median(.$rk)) %>% unlist) %>% 
  unnest(data) -> policy_begin_tab

policy_begin_tab %>% 
  ggplot(.) +
  geom_histogram(aes(x = rk,
                     color = skew_cat,
                     fill = skew_cat)) +
  facet_wrap(~ lab,
             ncol = 4,
             labeller = label_wrap_gen()) +
  geom_vline(aes(xintercept = md), 
             color = "red",
             size = 1.2) + 
  cowplot::theme_cowplot() +
  labs(x = "Sequential Order",
       y = "Count",
       title = "Any Effort Scenario") +
  theme(strip.background = element_rect(fill = NA, color = "black"),
        panel.border = element_rect(color = "black")) +
  ggsci::scale_fill_lancet() +
  ggsci::scale_color_lancet()

ggsave(filename = here("figs", "any_sequential_odering.png"),
       width = 20,
       height = 10)

# Max effort scenario
policy_max <- joined$hi %>% 
  dplyr::select(cnt, date, policy_raw) %>% 
  pivot_longer(cols = policy_raw) %>% 
  filter(value != 0) %>% 
  group_by(cnt, name) %>% 
  mutate(rk = rank(date)) %>% 
  filter(rk == 1)  %>%
  dplyr::select(-value,
                -rk) %>% 
  group_by(cnt) %>% 
  mutate(rk = rank(date))

policy_max %>% pull(name) %>% table

policy_max %>%
  group_by(cnt) %>% 
  left_join(joined$policy_dic, by = c("name" = "policy_code")) %>%
  mutate(lab = factor(lab, 
                      levels = joined$policy_dic$lab)) %>%
  group_by(lab) %>% 
  nest() %>% 
  mutate(skewness = map(data, ~e1071::skewness(.$rk)) %>% unlist,
         skew_cat = case_when(abs(skewness) > 1 ~ "Highly Skewed",
                              abs(skewness) <= 1 & abs(skewness) >= 0.5 ~ "Moderately Skewed",
                              abs(skewness) < 0.5 ~ "Roughly Symmetric"),
         md = map(data, ~median(.$rk)) %>% unlist) %>% 
  unnest(data) -> policy_max_tab

policy_max_tab %>% 
  ggplot(.) +
  geom_histogram(aes(x = rk,
                     color = skew_cat,
                     fill = skew_cat)) +
  facet_wrap(~ lab,
             ncol = 4,
             labeller = label_wrap_gen()) +
  geom_vline(aes(xintercept = md), 
             color = "red",
             size = 1.2) + 
  cowplot::theme_cowplot() +
  labs(x = "Sequential Order",
       y = "Count",
       title = "Maximum Effort Timing") +
  theme(strip.background = element_rect(fill = NA, color = "black"),
        panel.border = element_rect(color = "black")) +
  ggsci::scale_fill_lancet() +
  ggsci::scale_color_lancet()

ggsave(filename = here("figs", "max_sequential_odering.png"),
       width = 20,
       height = 10)
