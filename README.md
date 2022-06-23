
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RCTrep: An R package for validation of methods for treatment effect estimation using real-world data

<!-- badges: start -->

**RCTrep** is an R package to validate methods for **conditional average
treatment effect (CATE)** estimation using **real-world data (RWD)**.
Validation of methods for treatment effect estimation using RWD is
challenging because we do not observe the true treatment effect for each
individual \(\boldsymbol{X}_{i}\) - a fundamental problem of **causal
inference** - hence we can only estimate treatment effect using observed
data. Randomized control trial (RCT) assigns individuals to treatment
\(Z=1\) or control groups \(Z=0\) with known probability
\(p(Z=1\mid \boldsymbol{X})\), hence two groups are balanced in terms of
observed and unobserved covariates. The difference in outcomes between
groups can be merely attributed to realization of treatment \(Z\), and
hence the treatment effect is the simple difference in means of outcomes
and is unbiased given identification assumption holds. However, in case
we don’t know the true probability \(p(Z=1\mid \boldsymbol{X})\) in RWD,
without knowing the ground truth, *how can we validate the methods for
treatment effect estimation?*

**RCTrep** is an R package to enable easy validation of various methods
for treatment effect estimation using RWD by comparison to RCT data. We
identify under which conditions the estimate from RCT can be regarded as
the ground truth for methods validation using RWD. We assume the RWD and
RCT data are two random samples from a, potentially different,
population, and hence allow for a valid comparison of estimates of
treatment effect between two samples after population composition is
controlled for. We refer users to [RCTrep
vignettes](https://github.com/duolajiang/RCTrep/blob/master/vignettes/RCTrep%20vignettes.pdf "RCTrep: A R package for evaluation of methods for treatment effect estimation using real world data")
for theoretical elaboration. We consider a set of candidate treatment
effect estimators \(\mathcal{F}=\{f_1,…,f_m \}\), where
\(f(x): \mathcal{X} \mapsto \mathbb{E}[Y(1)-Y(0)∣\boldsymbol{X}=\boldsymbol{x}]\),
hence \(f(x)\) is an estimator of conditional average treatment effect
of population with characteristics \(\boldsymbol{X}=\boldsymbol{x}\). We
provide the package that makes it easy to try out various estimators
\(f \in \mathcal{F}\) and select the best one using the following
evaluation metric:
\[f^*=\text{argmin}_{f\in\mathcal{F}} \mathbb{L}(\hat{\tau};f)=\text{argmin}_{f\in\mathcal{F}}\left(\hat{\tau} - \sum_{\boldsymbol{x}}w(\boldsymbol{x})f(\boldsymbol{x})\right)^2,s.t.  p(\boldsymbol{x}) = q(\boldsymbol{s})w(\boldsymbol{x}) \]
where \(\hat{tau}\) is an unbiased estimate of the average treatment
effect derived from the RCT data, \(p(\boldsymbol{x})\) and
\(q(\boldsymbol{x})\) are the empirical density of \(\boldsymbol{x}\) in
RCT data and RWD, \(w(\boldsymbol{x})\) is a weight for individuals in
RWD with characteristics \(\boldsymbol{X}=\boldsymbol{x}\). Hence the
weighted distribution of covariates in RWD and distribution of
covariates in the RCT data are balanced. We compute
\(\mathbb{L}(\hat{\tau};f)\) on **population** and **sub-population**
levels.

RCTrep provides two core classes, namely, TEstimator and SEstimator,
which are responsible for adjusting the treatment assignment mechanism
and the sampling mechanism respectively. TEstimator has three subclasses
for adjusting the treatment assignment mechanism, namely, G-computation,
inverse propensity score weighting, and doubly robust estimation 8,9.
SEstimator has three subclasses for adjusting the sampling mechanism,
namely, exact matching, inverse sampling score weighting, and
subclassification 10. Users can specify modeling approaches for sampling
score, propensity score, outcome regression, and distance measure, etc.
Two objects instantiated using RWD and experimental RCT data communicate
within the object of the class SEstimator, sharing either unit level
data or aggregated data for computing the weights w(x).

The package can also replicate a target study in which only meta data is
available (point estimate and interval estimate of average treatment
effect, conditional average treatment effect conditioning on univariate
variable, and univariate distribution).

The package use R6 Object-oriented programming system. We provide an
overview of implementation of RCTrep in the Figure
![schematic](man/figures/README-structure.png) The package new two
objects of superclass Estimator for source study and target study,
respectively, then

1.  each object estimate the treatment effect using G\_computation,
    inverse propensity score weighting, or doubly robust estimator to
    ensure internal validity;
2.  communication between two objects via implementing the public method
    RCTrep(), e.g., source.obj$RCTrep(target.obj). Then source.obj get a
    estimates as close to target.obj as possible. This step is to
    conduct external validation.

Below shows how the core function RCTREP() work:

![schematic](man/figures/README-function_call.png) <!-- badges: end -->

## Installation

<!-- You can install the released version of RCTrep from [CRAN](https://CRAN.R-project.org) with: -->

<!-- ``` r -->

<!-- install.packages("RCTrep") -->

<!-- ``` -->

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("duolajiang/RCTrep")
```

We will realease the package to CRAN soon.

## Example

This is a basic example which shows you how to solve a common problem:

``` r
# library(RCTrep)
# 
# source.data <- RCTrep::source.data
# target.data <- RCTrep::target.data
# 
# output <- RCTREP(source.data=source.data, target.data=target.data,
#                  vars_name=list(confounders_internal=c("Stage2","pT","age","BRAF"),
#                                 confounders_external=c("Stage2","pT","age","BRAF"),
#                                 treatment_name=c('combined_chemo'),
#                                 outcome_name=c('vitstat')),
#                  stratification=c("Stage2","pT"))
# 
# summary(source.obj = output$source.obj, target.obj = output$target.obj)
```

You can also summarize conditional average treatment effect by
specifying stratification and stratification\_joint:

``` r
#output$source.obj$plot_CATE(stratification=c("Stage2","pT","BRAF"),stratification_joint = TRUE)
```
