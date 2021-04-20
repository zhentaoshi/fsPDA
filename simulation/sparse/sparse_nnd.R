rm(list=ls())
setwd("~/simulation/sparse")

source("main_function.R")

setwd("result")
dgp = "nnd"
run_simulation(dgp)
