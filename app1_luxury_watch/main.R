# test data for the luxury watch import example
# this is the application in the main text

load("testData.Rda")
source("fsPDA.R")



fsPDA(treated, control, intervention_time = "201301")  # run this line to print the results

