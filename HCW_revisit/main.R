rm(list = ls())

library(ggplot2)
library(glmnet)
library(MASS)

source("fs.R")
source("lasso_ic.R")
source("random_draw.R")

# date cleaning
main = as.data.frame(read.table("hcw-data.txt", header = F)) # load data

index = c("Hong Kong","Australia","Austria","Canada","Denmark",
          "Finland","France","Germany","Italy","Japan","Korea","Mexico",
          "Netherlands","New Zealand","Norway","Switzerland", "United Kingdom",
          "United States","Singapore","Philippines","Indonesia","Malaysia",
          "Thailand","Taiwan","China")

n = length(1993:2008) # set the period of date
date = paste(rep(1993:2007, each = 4), rep(c("Q1","Q2","Q3","Q4"), 15), sep = "")
date = append(date, "2008Q1")

# end of training data: 2003Q4
t1 = 44
t2 = 17
t = t1 + t2

y = drop(main[,1])
Y = data.matrix(main[,-1])
N = ncol(Y)
country = index[-1]
colnames(Y) = country
pre.y = y[1:t1]
post.y = y[(t1+1):t]
PRE.Y = Y[1:t1,]
POST.Y = Y[(t1+1):t,]

#forward selection
fs = fs_a(N)
p_fs = length(fs$b) - 1

#lasso
las = lasso_ic(N)
b_las = las$b
p_las = length(las$b) - 1
rsq_las = las$rsq
rsq_post = las$rsq_post

#extend fs to select p_las variables, for plotting purpose only
p_max = p_las
fs_p_max = fs_p(N, p_max) # for plotting
rsq_fs = fs_p_max$rsq

# exhaustive search + random draw
rand = rd(N, p_max)
rsq_best = sapply(rand$Q, function(x)(x$rsq_max))
p_best = rand$p_wic

# line plot: compare all model
rsq_fs = cbind("Forward Selection", 1:p_max, rsq_fs)
rsq_las = cbind("Lasso", 1:p_max, rsq_las)
rsq_post = cbind("Post Lasso", 1:p_max, rsq_post)
rsq_best = cbind("Best", 1:p_max, rsq_best)
B = rbind(rsq_fs, rsq_las, rsq_post, rsq_best)
B[,3] = round(as.numeric(B[,3]), 3)
B = data.frame(B)
colnames(B) = c("model", "p", "R2")
B[,3] = as.numeric(as.character(B[,3]))

wp = ggplot(data = B, aes(x = p, y = R2, group = model))
wp = wp + geom_line( aes( linetype = model, color = model ))
wp = wp + geom_point( aes( color = model ), size = 0.5)
colors = unique(ggplot_build(wp)$data[[1]]["colour"])$colour
wp = wp + annotate("point", x = p_best, y = as.numeric(rsq_best[p_best, 3]), color = colors[1], shape = 8)
wp = wp + annotate("point", x = p_fs, y = as.numeric(rsq_fs[p_fs, 3]), color = colors[2], shape = 8)
wp = wp + annotate("point", x = p_las, y = as.numeric(rsq_las[p_las, 3]), color = colors[3], shape = 8)
wp = wp + annotate("point", x = p_las, y = as.numeric(rsq_post[p_las, 3]), color = colors[4], shape = 8)

wp = wp + geom_text ( aes( label = R2 ), size = 2, check_overlap = TRUE, vjust = "outward")
wp = wp + coord_cartesian(ylim=c(0, 1)) + scale_y_continuous(breaks=seq(0, 10, 0.25))

wp = wp + labs(y = "R-Square", x = "Number of Selected Variables(R)")
wp = wp + theme_bw()
wp = wp + theme(legend.title = element_blank())

print(wp)
ggsave(file = "hcw_r2.png", plot = wp, width = 10, height = 5.5)
