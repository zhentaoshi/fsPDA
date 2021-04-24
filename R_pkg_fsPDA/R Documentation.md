# Forward-Selected Panel Data Approach

#### Description

Function implementing forward-selected panel data approach in Zhentao Shi and Jingyi Huang (2021). 

#### Usage

```R
fsPDA(treated,control,treatment_start,date=NULL,lrvar_lag=NULL,plot=TRUE)
```

#### Arguments

| Argument          |                                                              |
| :---------------- | ------------------------------------------------------------ |
| `treated`         | Numeric. A $T$-dimensional time series vector of treated units. |
| `control`         | Numeric. A $T\times N$ panel matrix with each column being a control unit. |
| `treatment_start` | An integer specifying the period treatment starts. Should set to lie between $T/2$ and $T$ to ensure enough pre-treatment observations. |
| `date`            | Date or numeric. A $T$-dimensional vector of date class or any meaningful numerical sequence. By default, if set to be `NULL`, `1:length(treated)` is used. |
| `lrvar_lag`       | A non-negative integer specifying the maximum lag with Bartlett kernel for the Newey-West long-run variance estimator. By default, if set to be `NULL`, `floor((length(treated)-treatment_start+1)^(1/4))` is used. Cannot set to be larger than `floor(sqrt(length(treated)-treatment_start+1))`. |
| `plot`            | Logical. Should a `ggplot` object be included in the return list and printed? If set to be `TRUE`, `ggplot2` package is needed as dependency. |

#### Value

| Return          |                                                              |
| --------------- | ------------------------------------------------------------ |
| `select`        | A list containing feature selection results, where `dim` is the number of selected units, `control` is the vector indicates the selected units, `coef` contains the coefficient estimates, and `RSquared` is the in-sample $R^2$. |
| `in_sample`     | A data frame with in-sample fitted values.                   |
| `out_of_sample` | A data frame with out-of-sample counterfactual predicts and treatment effect estimates. |
| `ATE`           | A numeric vector containing estimate of average treatment effect (ATE), its long-run variance, t-statistic, and p-value to test if ATE is statistically 0. |
| `plot`          | A `ggplot` object. Some post-plot arguments of `ggplot` can be added additionally, for example, `labs`. |

#### Author(s)

Jingyi Huang, Zhan Gao, Zhentao Shi, Yishu Wang

#### References

Zhentao Shi and Jingyi Huang (2021): "Forward-Selected Panel Data Approach" forthcoming at the *Journal of Econometrics*, [arXiv: 1908.05894](https://arxiv.org/abs/1908.05894)

https://github.com/zhentaoshi/fsPDA

#### Examples

```R
load("testData.Rda")

result=fsPDA(treated,control,
             treatment_start=which(names(treated)==intervention_time),
             date=as.Date(paste(substr(names(treated),1,4),"-",substr(names(treated),5,6),"-01",sep="")))

print(result$plot+labs(x="Year",y="Monthly Growth Rate"))
```

