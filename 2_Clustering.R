if(!exists("joined")) joined <- here("data", "joined_all.RDS") %>% readRDS

policy_raw <- joined$hi %>% 
    colnames %>% 
    str_subset("[A-Z][0-9]") %>%
    .[!.%in% c("M1","E3","E4","H4","H5")] # remove NPIs not considered

# [START]
# this needs to be ran only once, and it may take some time
# this segment conducts the temporal clustering analyses.
# joined %>%
#     .[c("hi","mid","lo")] %>%
#     map(ungroup) %>%
#     map(dplyr::select, policy_raw) %>%
#     map(mutate_all, as.character) %>%
#     map(mutate_all, as.numeric) %>%

# this specific line below is for calculating the statistical significance of 
# temporal clusters based on bootstrapping.

#     map(pvclust::pvclust,
#         method.hclust = "ward.D2",
#         method.dist = "euclidean",
#         nboot = 10000) -> hcd
# 
# save(hcd, file = "results/hcd.rdata")
# [END]

load("results/hcd.rdata")

# raw results from temporal clustering
joined %>% 
    .[c("hi","mid","lo")] %>%
    map(ungroup) %>%
    map(dplyr::select, policy_raw) %>%
    map(t) %>% 
    map(dist, method = "euclidean") %>% 
    map(hclust, method = "ward.D2") -> hcd_raw

# extract cluster characteristics for the dendrograms
hcd_raw %>% 
    map(dendro_data) -> hcd_data

# this is the significance boxes - quite manual
sig_boxes <- list()
sig_boxes[[1]] <- data.frame(xmin = c(1.45, 4.55, 8.45),
                             xmax = c(4.45, 6.5, 13.5),
                             ymin = c(-48, -48, -48),
                             ymax = c(88, 73, 110))
p_max <- hclust_plot_max()
p_max

sig_boxes[[2]] <- data.frame(xmin = c(0.45, 8.55),
                             xmax = c(8.45, 13.55),
                             ymin = c(-99, -99),
                             ymax = c(270, 310))
p_mid <- hclust_plot_mid()
p_mid

sig_boxes[[3]] <- data.frame(xmin = c(0.45, 4.55),
                             xmax = c(4.45, 13.55),
                             ymin = c(-54, -54),
                             ymax = c(85, 118))
p_any <- hclust_plot_any()
p_any

fig_dendogram <- ggpubr::ggarrange(p_any, p_max, ncol = 2, common.legend = T, legend = "bottom")

ggsave(filename = here("figs", "fig2_v2.png"),
       plot = fig_dendogram,
       width = 30,
       height = 10)
# 
ggsave(file = here("figs", "fig2_mid.png"),
       plot = p_mid,
       width = 20,
       height = 10)
