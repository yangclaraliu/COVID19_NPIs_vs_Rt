hclust_plot_max <- function(){
  ggplot()+
    geom_hline(yintercept = c(0, 50, 100, 150),
               linetype = 3) +
    geom_segment(data = hcd_data[[1]]$segments,
                 aes(x = x,
                     y = y,
                     xend = xend,
                     yend = yend)) +
    geom_point(data = hcd_data[[1]]$labels %>% 
                 left_join(joined$policy_dic, 
                           by = c("label" = "policy_code")),
               aes(x = x,
                   y = y,
                   color = cat)) +
    geom_label(data = hcd_data[[1]]$labels %>% 
                 left_join(joined$policy_dic, 
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
    scale_y_reverse(limits = c(180,-48),
                    breaks = c(150,100,50,0))+
    guides(color = guide_legend(override.aes = aes(size = 4)))+
    theme_cowplot() +
    theme(plot.title = element_text(size = 30, 
                                    face = "italic",
                                    hjust = 0.5),
          # panel.border = element_rect(color = "black",
          #                             fill = NA),
          legend.key = element_rect(fill = "white"),
          legend.text = element_text(size = 25),
          legend.position = "bottom",
          axis.line = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) +
    # theme_dendro() +
    scale_color_manual(values = c('#a6cee3',
                                  '#1f78b4',
                                  '#b2df8a',
                                  '#33a02c')) +
    labs(title = "Timing of Max. Efforts",
         color = " ",
         y = "Height") +
    geom_segment(aes(x = 0, xend = 0, y = 0, yend = 150), size = 1.5) +
    geom_text(aes(y = c(0, 50, 75,100, 150), 
                  x = -0.2, 
                  label = c("0","50","\nHeight","100","150")), 
              size = 5) -> p_max
    return(p_max)
}
