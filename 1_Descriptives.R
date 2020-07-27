root_path <- here::here()
library(tidyverse)
library(lubridate)
library(data.table)

if(!exists("joined")) joined <- file.path(root_path, "data", "joined_all.RDS") %>% readRDS
if(!exists("who") | !exists("tags") | !exists("start")) file.path(root_path, "extract_who.R") %>% source
start %<>% mutate(tag = "Turning\npoint", tag_date = as.Date("2020-04-13"), rk = paste0(rk,"\n"))

# save_data <- FALSE
policy_raw <- joined$policy_dic$policy_code
bind_rows(`Maximum effort scenario` = joined$hi, 
                         `Any effort scenario` = joined$lo, .id = "scenario") %>%
    dplyr::select(date, cnt, scenario, policy_raw) %>%
    data.table::as.data.table() %>% 
    data.table::melt(., id.vars = c("cnt", "date","scenario")) %>% 
    .[, value := as.numeric(as.character(value))] %>% 
    data.table::dcast(., cnt + date + scenario ~ variable) %>% 
    .[, ':=' (P1 = if_else(C1 + C2 + C3 + C4 + C5 + C6 + C7> 0, 1, 0),
              P2 = if_else(C8 >0, 1, 0),
              P3 = if_else(E1 + E2 + E3 > 0, 1, 0),
              P4 = if_else(H1 + H2 + H3 + H4 > 0, 1, 0),
              region = countrycode::countrycode(cnt, 
                                                origin = "iso3c", 
                                                destination = "region"))] %>%
    .[,!..policy_raw] %>% 
    .[,!"cnt"] %>% 
    .[, keyby = .(date, region, scenario),
      .(P1 = sum(P1),
        P2 = sum(P2),
        P3 = sum(P3),
        P4 = sum(P4))] %>% 
    data.table::melt(., id.vars = c("date", "region", "scenario")) %>% 
    merge(., start[c("region", "date", "rk")], all.x = T, by = c("date", "region")) -> counts_data

countrycode::codelist %>% 
    dplyr::select(iso3c, region, country.name.en) %>% 
    filter(!is.na(region),
           !is.na(iso3c)) %>% 
    group_by(region) %>% 
    tally() %>% 
    mutate(lab = paste0("n = ", n)) -> region_count

region_count %>% mutate(scenario = "Max. Efforts") %>% 
    bind_rows(region_count %>% mutate(scenario = "Any Effort")) %>% 
    mutate(scenario = factor(scenario,levels = c("Max. Efforts", "Any Effort"))) -> region_labs

fig_counts <- counts_data %>%
    merge(., region_count, all.x = TRUE, by = "region") %>%
    .[, proportion := value/n] %>% 
    .[, scenario := factor(scenario,
                           levels = c("Maximum effort scenario", "Any effort scenario"),
                           labels = c("Max. Efforts", "Any Effort"))] %>% 
    .[!is.na(region)] %>% 
    ggplot(., aes(x = date, 
                  y = proportion,
                  group = variable,
                  color = variable)) +
    geom_point(alpha = 0.4, size = 2.5) +
    # stat_smooth(method = "loess", se = FALSE, formula = y~x, span = 1) +
    facet_grid(scenario ~ region,
               labeller = label_wrap_gen()) + 
    xlim(as.Date("2020-01-01"), as.Date("2020-06-22")) + 
    labs(x = "Date", y = "Proportion of countries in Region with NPI") +
    # scale_color_brewer(palette = "Pastel",name = "Policy group", 
    #                    labels = c("Internal\nrestrictions", 
    #                               "Int'l travel \nrestrictions",
    #                               "Economic measures",
    #                               "Health systems\nactions"))+
    ggsci::scale_color_lancet(name = "Policy group", 
                              labels = c("Internal\nrestrictions\n",
                                         "Int'l travel \nrestrictions\n",
                                         "Economic \nmeasures\n",
                                         "Health systems\nactions"))+
    theme_bw()+
    theme(panel.grid = element_blank(),
          strip.background = element_rect(NA),
          axis.text = element_text(size = 20),
          axis.title = element_text(size = 25),
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 20),
          strip.text = element_text(size = 25))+
    geom_vline(data = start,
               aes(xintercept =  Date,
                   linetype = rk))+
    geom_vline(data = start,
               aes(xintercept =  tag_date,
                   linetype = tag)) +
    scale_linetype_manual(values = c(1,2), name  = "Epidemic\nprogression") +
    #lims(y = c(0,1)) +
    geom_label(data = region_labs,
              aes(x = as.Date("2020-01-30"),
                  y = 0.95,
                  label = lab),
              inherit.aes = F,
              size = 8)

ggsave(filename = "figs/fig1.png",
       plot = fig_counts,
       width = 30,
       height = 10)

# Stringency pot for CHN and SWE

plot_stringency <- function(iso3, country_name){
    joined$stringency %>% 
        filter(cnt %in% iso3) %>% 
        group_by(cnt) %>% 
        ggplot(aes(x=date,y=stringency)) + 
        geom_line(size = 0.5,color='darkblue') + 
        labs(x = "", y = "", title = paste0("Stringency - ",country_name))  + 
        ylim(0,100) + 
        xlim(as.Date("2020-01-01"),as.Date("2020-05-31")) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
}

plot_rt <- function(iso3, country_name){

    joined$mid %>% 
        filter(cnt %in% iso3) %>% 
        group_by(cnt) %>% 
        ggplot(aes(date)) +
        geom_ribbon(aes(ymin=lower_90,ymax=upper_90),fill='lightblue') +
        geom_line(aes(y=median),color='darkblue') +
        labs(x = "", y = "", 
             title = bquote("Median" ~ R[t] ~ " - " ~ .(country_name)))  + 
        ylim(0,3) + 
        xlim(as.Date("2020-01-01"),as.Date("2020-05-31")) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
        
}

sd_CHN <- plot_stringency("CHN", "China")
sd_SWE <- plot_stringency("SWE", "Sweden")
rt_CHN <- plot_rt("CHN", "China")
rt_SWE <- plot_rt("SWE", "Sweden")
gridExtra::grid.arrange(sd_CHN,sd_SWE,rt_CHN,rt_SWE)
