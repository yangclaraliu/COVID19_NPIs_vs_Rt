# correlation table
correlation_plot <- function(data, title){
    
    p.mat <- data %>% 
        select(policy_raw) %>% 
        mutate_all(as.numeric) %>% 
        corrplot::cor.mtest() %>% 
        .[["p"]]
    
    data %>% 
        dplyr::select(policy_raw) %>% 
        mutate_all(as.numeric) %>% 
        cor %>% 
        corrplot::corrplot(p.mat = p.mat, sig.level = 0.05, type = "upper", 
                           title = title, mar=c(0,0,2,0)) 
    
}