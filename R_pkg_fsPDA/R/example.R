source("fsPDA.R")
load("testData.Rda")

result=fsPDA(treated,control,
             treatment_start=which(names(treated)==intervention_time),
             date=as.Date(paste(substr(names(treated),1,4),"-",substr(names(treated),5,6),"-01",sep="")))

print(result$plot+labs(x="Year",y="Monthly Growth Rate"))

