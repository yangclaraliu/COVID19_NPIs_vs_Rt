# COVID19_NPIs_vs_Rt
These files should be run in their order:  
-`0_loadData.R`: Load all data (if `data/joined.RDS` does not exist yet) and packages/ custom functions needed.  
-`1_Descriptives.R`: Descriptive plots. Fig 1 + data missingness plot in Appendix.  
-`2_Clustering.R`: Temporal clustering exercise using hierarchical clustering + statistical significant based on bootstrapping.  
-`3_Select_Var_Within.R`: Regression models. 
