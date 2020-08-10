plot_all_lags <- function(all_lags){
    ggplot(all_lags, aes(x = lags,
                         y = deviance,
                         group = scenario,
                         color = scenario))+
        facet_wrap(~region, nrow = 2)+
        geom_line(size = 1.2)+
        cowplot::theme_cowplot() +
        theme(axis.text    = element_text(size = 20),
              axis.title   = element_text(size = 20),
              legend.text  = element_text(size = 20),
              legend.title = element_text(size = 20),
              strip.text   = element_text(size = 20),
              legend.position = "top",
              strip.background = element_rect(fill=NA),
              panel.border = element_rect(colour = "black", fill=NA, size=1))+
        geom_vline(data = all_lags %>% 
                       group_by(region, scenario) %>% 
                       top_n(-1, deviance) %>% 
                       select(lags), 
                   linetype = "dashed", aes(xintercept = lags, color = scenario)) + 
    labs(x = "Temporal Lags",
         y = "Deviance",
         color = "Scenarios")
}
