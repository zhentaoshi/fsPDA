rm(list=ls())
setwd("~/simulation/sparse")

source("main_function.R")

setwd("result")
dgp = "inid"
run_simulation(dgp)
