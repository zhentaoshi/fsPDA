# test data for the luxury watch import example


load("testData.Rda")
source("fsPDA.R")
fsPDA(treated, control, intervention_time = "201301")

