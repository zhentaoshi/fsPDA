rm(list=ls())
setwd("~/simulation/sparse")

source("main_function.R")

setwd("result")
dgp = "iid"
run_simulation(dgp)
