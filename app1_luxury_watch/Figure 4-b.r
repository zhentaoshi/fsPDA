library(fsPDA)

data("china_import")
date_import <- names(china_import$treated)

result <- est.fsPDA(
  treated = china_import$treated,
  control = china_import$control,
  treatment_start = which(date_import == china_import$intervention_time), date = as.Date(paste(substr(date_import, 1, 4), "-",
                                                                                               substr(date_import, 5, 6), "-01", sep = ""))
)

# back-out level data
obs <- exp(cumsum(c(result$in_sample$observation, result$out_of_sample$observation)))
fit <- exp(cumsum(c(result$in_sample$fit, result$out_of_sample$counterfactual)))

# plugin to result
result$in_sample$observation <- obs[1:length(result$in_sample$observation)]
result$in_sample$fit <- fit[1:length(result$in_sample$fit)]
result$out_of_sample$observation <- obs[(length(result$in_sample$observation)+1):length(obs)]
result$out_of_sample$counterfactual <- fit[(length(result$in_sample$fit)+1):length(fit)]

plot(result, tlab = "Year", ylab = "Import Value")

