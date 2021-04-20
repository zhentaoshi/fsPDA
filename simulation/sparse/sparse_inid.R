rm(list=ls())
setwd("~/FS_2019aug/code_JY/simulation/super code/sparse")

source("main_function.R")

setwd("result")
dgp = "inid"
run_simulation(dgp)
