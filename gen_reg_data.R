gen_regress_data <- function(scen, max_date, reg){
    if(scen == "High") tmp <- joined$hi
    if(scen == "Low") tmp <- joined$lo
    if(scen == "Mid") tmp <- joined$mid
    tmp %>% 
        ungroup %>% 
        filter(date <= max_date) %>% 
        left_join(country_list %>% dplyr::select(-name), by = "cnt") -> tmp
    tmp <- pdata.frame(tmp %>% filter(region %in% reg), index=c("cnt","date"), drop.index=TRUE, row.names=TRUE)
    
    return(tmp)
}
