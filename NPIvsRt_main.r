# @title: NPIvsRt_main.r
# @objective: Descriptives
# @author: Mark Jit, Yang Liu
# @created: 3/6/2020
# @created: 3/6/2020

# Load functions, data, prepare regression tables
source("v2/0_LoadData.R")
# source("ramp_data_fns.R")
# Plot descriptive figure
source("v2/1_Descriptives.R")
# Plot clustering results
source("v2/2_Clustering.R")
# backward variable selection
source("v2/3_Select_Var_Within.R")
# random effect model
source("v2/4_Random.R")