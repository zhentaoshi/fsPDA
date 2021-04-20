# fsPDA
This repo hosts the companion function for 
Zhentao Shi and Jingyi Huang (2019): "[Forward-Selected Panel Data Approach](https://arxiv.org/abs/1908.05894)" arXiv: 1908.05894

The key function is provided, while this repo is under construction for documentation. We aim to develop it into an R package.

Under the root folder:
* `test.R` is an R script that replicates the empirical example in the paper about China's luxury watch import.
* `fsPDA.R` contains a documented function *fsPDA*. It reports the identity of the selected units, the treatment effects over time, and the t-statistic for the average treatment effect.

Under folder `simulation`: 

- Folder `nonsparse` includes codes for simulation of non-sparse DGP, in which running `Run.simulation.dense.R` gives the simulation results. 
-  Folder `sparse` includes codes for simulation of sparse DGP, in which running `sparse_iid.R`, `sparse_inid.R` and `sparse_nnd.R` give the simulation results. 

Any comments are welcome.
