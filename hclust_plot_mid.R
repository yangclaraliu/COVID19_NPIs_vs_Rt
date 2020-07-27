hclust_plot_mid <- function(){
    ggplot() +
        geom_segment(data = hcd_data[[2]]$segments,
                     aes(x = x,
                         y = y,
                         xend = xend,
                         yend = yend)) +
        geom_point(data = hcd_data[[2]]$labels %>% 
                       left_join(policy_dic, 
                                 by = c("label" = "policy_code")),
                   aes(x = x,
                       y = y,
                       color = cat)) +
        geom_label(data = hcd_data[[2]]$labels %>% 
                       left_join(policy_dic, 
                                 by = c("label" = "policy_code")),
                   aes(x = x,
                       y = y,
                       label = lab,
                       color = cat),
                   #hjust = -0.1,
                   size = 10,
                   label.padding = unit(0.55, "lines"),
                   label.size = 1.5,
                   show.legend = F) +
        geom_rect(data = sig_boxes[[2]],
                  aes(xmin = xmin,
                      xmax = xmax,
                      ymin = ymin,
                      ymax = ymax),
                  fill = NA,
                  color = "firebrick",
                  size = 1.5,
                  linetype = 2) +
        coord_flip() +
        # geom_text(data = data.frame(x = 13.55,
        #                             y = 130,
        #                             label = "Timing of \nAny Effort"),
        #           aes(x = x, y = y, label = label),
        #           size = 10) +
        scale_y_reverse(limits = c(460,-100))+
        guides(color = guide_legend(override.aes = aes(size = 4)))+
        theme(plot.title = element_text(size = 30, 
                                        face = "italic",
                                        hjust = 0.5),
              # panel.border = element_rect(color = "black",
              #                             fill = NA),
              legend.key = element_rect(fill = "white"),
              legend.text = element_text(size = 25),
              legend.position = "bottom") +
        theme_dendro() +
        scale_color_manual(values = c('#a6cee3',
                                      '#1f78b4',
                                      '#b2df8a',
                                      '#33a02c'))+
        labs(title = "Timing of Multi-level Efforts",
             color = " ")  -> p_mid
    return(p_mid)
}
