# fsPDA
This repo hosts the companion documents, functions and replication files for 

* Zhentao Shi and Jingyi Huang (2021): "[Forward-Selected Panel Data Approach for Program Evaluation](https://www.sciencedirect.com/science/article/pii/S0304407621001536?via%3Dihub)," the *Journal of Econometrics*, [arXiv: 1908.05894](https://arxiv.org/abs/1908.05894)

**Contributors**: Jingyi Huang, Zhan Gao, Zhentao Shi, Yishu Wang

**Status**: Replication files are complete. The R package is released with version v1.0.1.

---


## R Package

The folder`R_pkg_fsPDA` contains an R package. 

* To install the package, run `devtools::install_github("zhentaoshi/fsPDA/R_pkg_fsPDA")`.

* Click here for [Package documents](https://github.com/zhentaoshi/fsPDA/blob/master/R_pkg_fsPDA/doc/).


---


## Empirical Applications

#### Application 1

 `app1_luxury_watch\main.R` is an R script that replicates the empirical example in main text of the paper about China's luxury watch import.



#### Application 2

`app2_HCW_revist\main.R` replicates the empirical example of Hsiao, Ching and Wan (2012) as in the [online supplement](https://ars.els-cdn.com/content/image/1-s2.0-S0304407621001536-mmc1.pdf) of the paper.





## Simulations

Under the folder `simulation`: 

- Folder `nonsparse` includes code for simulation of non-sparse DGP, in which running `Run.simulation.dense.R` gives the simulation results. 
- Folder `sparse` includes code for simulation of sparse DGP, in which running `sparse_iid.R`, `sparse_inid.R` and `sparse_nnd.R` gives the simulation results. 

