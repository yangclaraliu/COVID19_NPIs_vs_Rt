hclust_plot_max <- function(){
    ggplot()+
    geom_segment(data = hcd_data[[1]]$segments,
                 aes(x = x,
                     y = y,
                     xend = xend,
                     yend = yend)) +
    geom_point(data = hcd_data[[1]]$labels %>% 
                   left_join(policy_dic, 
                             by = c("label" = "policy_code")),
               aes(x = x,
                   y = y,
                   color = cat)) +
    geom_label(data = hcd_data[[1]]$labels %>% 
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
    geom_rect(data = sig_boxes[[1]],
              aes(xmin = xmin,
                  xmax = xmax,
                  ymin = ymin,
                  ymax = ymax),
              fill = NA,
              color = "firebrick",
              size = 1.5,
              linetype = 2) +
    # geom_text(data = data.frame(x = 13.55,
    #                             y = 130,
    #                             label = "Timing of \nMaximum Efforts"),
    #           aes(x = x, y = y, label = label),
    #           size = 10) +
    coord_flip() +
    scale_y_reverse(limits = c(180,-48))+
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
                                  '#33a02c')) +
    labs(title = "Timing of Max. Efforts",
         color = " ")  -> p_max
    return(p_max)
}
