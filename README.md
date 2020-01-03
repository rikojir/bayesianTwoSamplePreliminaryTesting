# bayesianTwoSamplePreliminaryTesting

Provides R scripts to replicate all simulation results and figures provided in the paper "Analysis of type I error rates of parametric and nonparametric Bayesian two-sample tests under preliminary assessment of normality for better reproducible biomedical research"

Scripts sim1.R, sim3.R and sim5.R procude the results for the parametric two-sample tests (Student's t-test and JZS-BF Bayesian t-test) displayed in table one.

Scripts sim2.R, sim4.R and sim6.R produce the results for the nonparametric two-sample tests (Mann-Whitney-U and Bayesian nonparametric t-test) displayed in table two.

Scripts sim7.R, sim8.R and sim9.R produce the results for the two-stage procedure, displayed in table three.

Scripts sim10.R, sim11.R and sim12.R provide the results for the type II error rates (beta levels) given in the paper.

To produce the figures, run script plots.R, which already includes the results of simulations one to nine, as these may run from multiple hours to multiple days depending on the machine.

It is heavily recommended to run these scripts on a high-performance computing cluster, as the code is already parallelized for four physical cores, and single scripts may take several days to run depending on the machine used.
