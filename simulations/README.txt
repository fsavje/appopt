
The simulations are done in three parts:

1. Complexity.
	See folder "complexity". This simulation investigates the run time and memory requirements of the algorithms.
	
2. Performance.
	See folder "performance". This simulation investigates how well the algorithms blocks the sample, both wrt.
	to maximum and average within-block distances and variance of the treatment effect estimator.
	
3. Limit.
	See folder "limit". This simulation shows the limit for each implementation by running them with a 3 hours
	CPU and 48 GB memory limit.
	
See respective folder for additional details.
	
All three parts depend on the standard R software (as provided by http://www.r-project.org), and "devtools",
"blockTools", "nbpMatching" and "appopt" packages. These can be installed by running:

install.packages(c("devtools", "blockTools", "nbpMatching"))
devtools::install_github("fsavje/appopt")

Generating figures and tables also requires the "reshape2", "ggplot2", "grid" and "gridExtra" packages.

The session info of the R installation (with packages loaded) on the server where the simulations were run is:

R version 3.1.1 (2014-07-10)
Platform: x86_64-unknown-linux-gnu (64-bit)

locale:
[1] C

attached base packages:
[1] grid      stats     graphics  grDevices utils     datasets  methods
[8] base

other attached packages:
[1] nbpMatching_1.4.4 Hmisc_3.15-0      ggplot2_1.0.1     Formula_1.2-1
[5] survival_2.38-1   lattice_0.20-31   blockTools_0.6-2  appopt_0.0.0.9000

loaded via a namespace (and not attached):
 [1] MASS_7.3-40         RColorBrewer_1.1-2  Rcpp_0.11.5
 [4] acepack_1.3-3.3     cluster_2.0.1       colorspace_1.2-6
 [7] digest_0.6.8        foreign_0.8-63      gtable_0.1.2
[10] latticeExtra_0.6-26 munsell_0.4.2       nnet_7.3-9
[13] plyr_1.8.1          proto_0.3-10        reshape2_1.4.1
[16] rpart_4.1-9         scales_0.2.4        splines_3.1.1
[19] stringr_0.6.2

