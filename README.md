
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RCTrep: An R package for validation of methods for treatment effect estimation using real-world data

<img src="https://zenodo.org/badge/437073752.svg">

**RCTrep** is an R package to validate methods for **conditional average
treatment effect (CATE)** estimation using **real-world data (RWD)**.
Validation of methods for treatment effect estimation using RWD is
challenging because we do not observe the true treatment effect for each
individual ![formula14](man/figures/CodeCogsEqn14.svg) - a fundamental
problem of **causal inference** - hence we can only estimate treatment
effect using observed data. Randomized control trial (RCT) assigns
individuals to treatment
<img src="https://latex.codecogs.com/svg.image?Z=1" /> or control groups
<img src="https://latex.codecogs.com/svg.image?Z=0"> with known
probability ![formula13](man/figures/CodeCogsEqn13.svg) , hence two
groups are balanced in terms of observed and unobserved covariates. The
difference in outcomes between groups can be merely attributed to
realization of treatment
<img src="https://latex.codecogs.com/svg.image?Z">, and hence the
treatment effect is the simple difference in means of outcomes and is
unbiased given identification assumption holds. However, in case we
don’t know the true probability
![formula13](man/figures/CodeCogsEqn13.svg) in RWD, without knowing the
ground truth, ***how can we validate the methods for treatment effect
estimation?***

## Method

**RCTrep** is an R package to enable easy validation of various methods
for treatment effect estimation using RWD by comparison to RCT data. We
identify under which conditions the estimate from RCT can be regarded as
the ground truth for methods validation using RWD. We assume the RWD and
RCT data are two random samples from a, potentially different,
population, and hence allow for a valid comparison of estimates of
treatment effect between two samples after population composition is
controlled for. We refer users to [RCTrep
vignettes](https://github.com/duolajiang/RCTrep/blob/master/vignettes/RCTrep%20vignettes.pdf "RCTrep: A R package for evaluation of methods for treatment effect estimation using real world data")
for theoretical elaboration, in which we illustrate why estimates from
RCT can be assumed as ground truth and how to use the estimates as the
surrogate of the ground truth of RWD from the view of **treatment
assignment mechanism** and **sampling mechansim**. We provide an diagram
to show how RCT data and RWD differ in two mechanisms in the following
figure:

![schematic](man/figures/DGM_RCT_RWD.png)

We consider a set of candidate treatment effect estimators
![formula1](man/figures/CodeCogsEqn1.svg) , where
![formula2](man/figures/CodeCogsEqn2.svg), hence
![formula3](man/figures/CodeCogsEqn3.svg) is an estimator of conditional
average treatment effect of population with characteristics
![formula4](man/figures/CodeCogsEqn4.svg). We provide the package that
makes it easy to try out various estimators
![formula5](man/figures/CodeCogsEqn5.svg) and select the best one using
the following evaluation metric:

![formula6](man/figures/CodeCogsEqn6.svg)

where ![formula7](man/figures/CodeCogsEqn7.svg) is an unbiased estimate
of the average treatment effect derived from the RCT data,
![formula8](man/figures/CodeCogsEqn8.svg) and
![formula9](man/figures/CodeCogsEqn9.svg) are the empirical density of
![formula10](man/figures/CodeCogsEqn10.svg) in RCT data and RWD,
![formula11](man/figures/CodeCogsEqn11.svg) is a weight for individuals
in RWD with characteristics ![formula4](man/figures/CodeCogsEqn4.svg).
Hence the weighted distribution of covariates in RWD and distribution of
covariates in the RCT data are balanced. We compute
![formula12](man/figures/CodeCogsEqn12.svg) on ***population*** and
***sub-population*** levels.

## Software overview

The package use R6 Object-oriented programming system. We provide an
overview of implementation of RCTrep in the following figure:

![schematic](man/figures/README-structure.png)

RCTrep provides two core classes, namely, **TEstimator** and
**SEstimator**, which are responsible for adjusting the **treatment
assignment mechanism** and the **sampling mechanism** respectively.

**TEstimator** has three subclasses for adjusting the treatment
assignment mechanism, namely,

-   G-computation,
-   Inverse propensity score weighting,
-   Doubly robust estimation

**SEstimator** has three subclasses for adjusting the sampling
mechanism, namely,

-   Exact matching,
-   Inverse sampling score weighting,
-   Subclassification

Users can specify modeling approaches for sampling score, propensity
score, outcome regression, and distance measure, etc. *Two objects*
instantiated using RWD and experimental RCT data *communicate within the
object of the class SEstimator*, sharing either **unit-level data”** or
**aggregated data** for computing the weights
![formula11](man/figures/CodeCogsEqn11.svg).

**Summary** R6 class Summary combines estimates from an object of class
TEstimate and/or an object of class SEstimate, and plots and evaluates
estimates of average treatment effect and heterogeneous treatment
effect. The number of objects of class TEstimator or SEstimator passed
to its constructor is not limited.

The package can also generate synthetic RCT data based on meta data from
publications (point estimate and interval estimate of average treatment
effect, conditional average treatment effect conditioning on univariate
variable, and univariate distribution).

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

## Quick Start

We demonstrate the simple usage of RCTrep to validate methods for
treatment effect estimation. We use G-computation to adjust for the
treatment assignment mechanism - the method using RWD to validate. We
use exact matching to balance covariates between RWD and RCT data, and
obtain the weighted estimates of treatment effect using RWD. Then we can
implement the fair comparison between weighted estimates using RWD and
unbiased estimate using experimental data. Variables that may confound
causal association between treatment and outcome, and variables that may
lead to supurious association between sampling and outcomes still need
careful investigation and identification.

### Step 1: Identification

This step is to identifiy the variable set
**confounders_treatment_name** that confound causal relation between
treatment and outcome, and the variable set
**confounders_sampling_name** that can induce supurious difference in
estimates between RWD and RCT data.

``` r
library(RCTrep)
source.data <- RCTrep::source.data
target.data <- RCTrep::target.data

vars_name <- list(confounders_treatment_name=c("x1","x2","x3","x4","x5","x6"),
                  treatment_name=c('z'),
                  outcome_name=c('y')
)
confounders_sampling_name <- c("x1","x2","x3","x4","x5","x6")
```

### Step 2: Estimation

#### step 2.1: Estimation of treatment effect of RWD and RCT data

``` r
source.obj <- TEstimator_wrapper(
  Estimator = "G_computation",
  data = source.data,
  name = "RWD",
  vars_name = vars_name,
  outcome_method = "glm",
  outcome_formula = y ~ x1 + x2 + x3 + z + z:x1 + z:x2 +z:x3+ z:x6,
  data.public = TRUE
)
#> y ~ x1 + x2 + x3 + z + z:x1 + z:x2 + z:x3 + z:x6

target.obj <- TEstimator_wrapper(
  Estimator = "Crude",
  data = target.data,
  name = "RCT",
  vars_name = vars_name,
  data.public = TRUE,
  isTrial = TRUE
)
```

#### Step 2.2: Estimation of weighted treatment effect of RWD

``` r
# step2.2: Estimation of weighted treatment effect
source.obj.rep <- SEstimator_wrapper(estimator="Exact",
                                target.obj=target.obj,
                                source.obj=source.obj,
                                confounders_sampling_name=confounders_sampling_name)
source.obj.rep$EstimateRep(stratification = c("x1","x3","x4","x5"))
```

### Step 3: Assumptions and model diagnosis

This step diagnoses model assumption for G-computation (covariats
balance for IPW), treatment overlap assumption, outcome overlap
assumption, and sampling overlap assumption.

``` r
source.obj$summary()
#> $residuals.overall
#> [1] 7.939112e-15
#> 
#> $residuals.subgroups
#>    x1 x2 x3 x4 x5 x6 sample.size      res.mean       res.se          msr
#> 1   0  0  0  0  0  0          10 -1.746381e-14 5.070722e-15 5.363946e-28
#> 2   0  0  0  0  0  1          39 -7.182289e-15 3.546474e-15 5.295295e-28
#> 3   0  0  0  0  1  0          18 -1.082467e-14 4.025566e-15 3.926617e-28
#> 4   0  0  0  0  1  1          68 -9.812412e-16 2.621409e-15 4.613725e-28
#> 5   0  0  0  1  0  0          33 -1.635729e-14 2.766306e-15 5.124391e-28
#> 6   0  0  0  1  0  1         132 -1.247235e-14 1.816200e-15 5.876736e-28
#> 7   0  0  0  1  1  1         263 -2.244086e-15 1.339672e-15 4.752527e-28
#> 8   0  0  1  0  0  0          28 -2.869728e-14 2.017141e-15 9.333931e-28
#> 9   0  0  1  0  0  1         145 -2.361904e-14 1.393494e-15 8.374820e-28
#> 10  0  0  1  0  1  0          78 -2.095332e-14 1.554759e-15 6.251720e-28
#> 11  0  0  1  1  0  0         117 -2.563144e-14 1.153694e-15 8.113682e-28
#> 12  0  0  1  1  1  0         261 -1.754152e-14 8.359062e-16 4.893773e-28
#> 13  0  1  0  0  0  0           2  7.577272e-15 2.248202e-15 6.246946e-29
#> 14  0  1  0  0  0  1           6  1.187939e-14 1.299008e-15 1.495569e-28
#> 15  0  1  0  0  1  0           5  8.026912e-15 1.101389e-15 6.928356e-29
#> 16  0  1  0  0  1  1          13  1.361731e-14 8.653648e-16 1.944175e-28
#> 17  0  1  0  1  0  0          10  8.476553e-15 6.868369e-16 7.609765e-29
#> 18  0  1  0  1  0  1          31  1.240943e-14 5.551225e-16 1.632387e-28
#> 19  0  1  0  1  1  0          15  8.326673e-15 5.664936e-16 7.382629e-29
#> 20  0  1  0  1  1  1          89  1.328712e-14 3.259036e-16 1.858944e-28
#> 21  0  1  1  0  0  1          29  3.629281e-15 9.706652e-18 1.317432e-29
#> 22  0  1  1  0  1  0          11 -2.523234e-16 1.638205e-15 2.690083e-29
#> 23  0  1  1  0  1  1          75  3.601563e-15 6.406410e-18 1.297430e-29
#> 24  0  1  1  1  0  0          33 -5.786617e-16 9.302072e-16 2.802398e-29
#> 25  0  1  1  1  0  1         124  3.623446e-15 4.813443e-18 1.313221e-29
#> 26  0  1  1  1  1  0          58 -2.277871e-15 7.093788e-16 3.387214e-29
#> 27  0  1  1  1  1  1         301  3.591442e-15 3.054962e-18 1.290126e-29
#> 28  1  0  0  0  0  0           2  5.406786e-14 1.521006e-14 3.154679e-27
#> 29  1  0  0  0  0  1           7  5.034068e-14 7.412166e-15 2.863826e-27
#> 30  1  0  0  0  1  0           4  5.406786e-14 8.781530e-15 3.154679e-27
#> 31  1  0  0  0  1  1          18  5.895284e-14 4.873762e-15 3.879248e-27
#> 32  1  0  0  1  0  0          10  5.102585e-14 4.967583e-15 2.825729e-27
#> 33  1  0  0  1  0  1          35  4.919240e-14 3.012446e-15 2.728436e-27
#> 34  1  0  0  1  1  0          23  5.869701e-14 3.088960e-15 3.655256e-27
#> 35  1  0  1  0  0  0           8  3.880229e-14 4.033804e-15 1.619519e-27
#> 36  1  0  1  0  0  1          24  3.990327e-14 2.952069e-15 1.792709e-27
#> 37  1  0  1  0  1  0          23  4.657144e-14 2.604928e-15 2.318184e-27
#> 38  1  0  1  0  1  1          71  5.817256e-14 1.844637e-15 3.622235e-27
#> 39  1  0  1  1  0  0          37  3.996803e-14 1.877558e-15 1.724351e-27
#> 40  1  0  1  1  0  1         121  4.445296e-14 1.506263e-15 2.248325e-27
#> 41  1  0  1  1  1  0          67  4.698729e-14 1.496331e-15 2.355580e-27
#> 42  1  1  0  1  0  1           9  7.704948e-14 3.510833e-16 5.937608e-27
#> 43  1  1  0  1  1  0           6  7.038814e-14 1.825633e-15 4.971155e-27
#> 44  1  1  0  1  1  1          24  7.766010e-14 1.804344e-16 6.031840e-27
#> 45  1  1  1  0  0  0           2  6.339373e-14 6.550316e-15 4.061672e-27
#> 46  1  1  1  0  0  1           5  6.896705e-14 5.982856e-16 4.757886e-27
#> 47  1  1  1  0  1  0           2  6.339373e-14 6.550316e-15 4.061672e-27
#> 48  1  1  1  0  1  1          13  6.825310e-14 3.254245e-16 4.659756e-27
#> 49  1  1  1  1  0  0           6  6.557717e-14 2.761856e-15 4.338505e-27
#> 50  1  1  1  1  0  1          26  6.928646e-14 2.166783e-16 4.801787e-27
#> 51  1  1  1  1  1  0          19  6.304898e-14 1.541784e-15 4.017962e-27
#> 52  1  1  1  1  1  1          62  6.825007e-14 1.441741e-16 4.659339e-27
#>            sesr
#> 1  1.097778e-28
#> 2  3.898004e-29
#> 3  8.715089e-29
#> 4  2.881245e-29
#> 5  5.988872e-29
#> 6  1.996223e-29
#> 7  1.472461e-29
#> 8  8.028521e-29
#> 9  4.061110e-29
#> 10 6.188173e-29
#> 11 4.591876e-29
#> 12 3.327032e-29
#> 13 3.407047e-29
#> 14 3.353089e-29
#> 15 1.669105e-29
#> 16 2.233739e-29
#> 17 1.040870e-29
#> 18 1.432920e-29
#> 19 8.584952e-30
#> 20 8.412447e-30
#> 21 7.004757e-32
#> 22 5.638197e-30
#> 23 4.623153e-32
#> 24 3.201486e-30
#> 25 3.473597e-32
#> 26 2.441463e-30
#> 27 2.204598e-32
#> 28 1.644750e-27
#> 29 8.739365e-28
#> 30 9.495970e-28
#> 31 5.746443e-28
#> 32 5.371732e-28
#> 33 3.551845e-28
#> 34 3.340269e-28
#> 35 3.627522e-28
#> 36 2.956264e-28
#> 37 2.342561e-28
#> 38 1.847259e-28
#> 39 1.688451e-28
#> 40 1.508404e-28
#> 41 1.345622e-28
#> 42 5.417953e-29
#> 43 2.622757e-28
#> 44 2.784482e-29
#> 45 8.304980e-28
#> 46 8.223173e-29
#> 47 8.304980e-28
#> 48 4.472816e-29
#> 49 3.501687e-28
#> 50 2.978148e-29
#> 51 1.954789e-28
#> 52 1.981610e-29
#> 
#> $plot.res.mse
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

``` r
source.obj$diagnosis_t_overlap()
#> function (..., list = character(), package = NULL, lib.loc = NULL, 
#>     verbose = getOption("verbose"), envir = .GlobalEnv, overwrite = TRUE) 
#> {
#>     fileExt <- function(x) {
#>         db <- grepl("\\.[^.]+\\.(gz|bz2|xz)$", x)
#>         ans <- sub(".*\\.", "", x)
#>         ans[db] <- sub(".*\\.([^.]+\\.)(gz|bz2|xz)$", "\\1\\2", 
#>             x[db])
#>         ans
#>     }
#>     names <- c(as.character(substitute(list(...))[-1L]), list)
#>     if (!is.null(package)) {
#>         if (!is.character(package)) 
#>             stop("'package' must be a character string or NULL")
#>         if (any(package %in% "base")) 
#>             warning("datasets have been moved from package 'base' to package 'datasets'")
#>         if (any(package %in% "stats")) 
#>             warning("datasets have been moved from package 'stats' to package 'datasets'")
#>         package[package %in% c("base", "stats")] <- "datasets"
#>     }
#>     paths <- find.package(package, lib.loc, verbose = verbose)
#>     if (is.null(lib.loc)) 
#>         paths <- c(path.package(package, TRUE), if (!length(package)) getwd(), 
#>             paths)
#>     paths <- unique(normalizePath(paths[file.exists(paths)]))
#>     paths <- paths[dir.exists(file.path(paths, "data"))]
#>     dataExts <- tools:::.make_file_exts("data")
#>     if (length(names) == 0L) {
#>         db <- matrix(character(), nrow = 0L, ncol = 4L)
#>         for (path in paths) {
#>             entries <- NULL
#>             packageName <- if (file_test("-f", file.path(path, 
#>                 "DESCRIPTION"))) 
#>                 basename(path)
#>             else "."
#>             if (file_test("-f", INDEX <- file.path(path, "Meta", 
#>                 "data.rds"))) {
#>                 entries <- readRDS(INDEX)
#>             }
#>             else {
#>                 dataDir <- file.path(path, "data")
#>                 entries <- tools::list_files_with_type(dataDir, 
#>                   "data")
#>                 if (length(entries)) {
#>                   entries <- unique(tools::file_path_sans_ext(basename(entries)))
#>                   entries <- cbind(entries, "")
#>                 }
#>             }
#>             if (NROW(entries)) {
#>                 if (is.matrix(entries) && ncol(entries) == 2L) 
#>                   db <- rbind(db, cbind(packageName, dirname(path), 
#>                     entries))
#>                 else warning(gettextf("data index for package %s is invalid and will be ignored", 
#>                   sQuote(packageName)), domain = NA, call. = FALSE)
#>             }
#>         }
#>         colnames(db) <- c("Package", "LibPath", "Item", "Title")
#>         footer <- if (missing(package)) 
#>             paste0("Use ", sQuote(paste("data(package =", ".packages(all.available = TRUE))")), 
#>                 "\n", "to list the data sets in all *available* packages.")
#>         else NULL
#>         y <- list(title = "Data sets", header = NULL, results = db, 
#>             footer = footer)
#>         class(y) <- "packageIQR"
#>         return(y)
#>     }
#>     paths <- file.path(paths, "data")
#>     for (name in names) {
#>         found <- FALSE
#>         for (p in paths) {
#>             tmp_env <- if (overwrite) 
#>                 envir
#>             else new.env()
#>             if (file_test("-f", file.path(p, "Rdata.rds"))) {
#>                 rds <- readRDS(file.path(p, "Rdata.rds"))
#>                 if (name %in% names(rds)) {
#>                   found <- TRUE
#>                   if (verbose) 
#>                     message(sprintf("name=%s:\t found in Rdata.rds", 
#>                       name), domain = NA)
#>                   thispkg <- sub(".*/([^/]*)/data$", "\\1", p)
#>                   thispkg <- sub("_.*$", "", thispkg)
#>                   thispkg <- paste0("package:", thispkg)
#>                   objs <- rds[[name]]
#>                   lazyLoad(file.path(p, "Rdata"), envir = tmp_env, 
#>                     filter = function(x) x %in% objs)
#>                   break
#>                 }
#>                 else if (verbose) 
#>                   message(sprintf("name=%s:\t NOT found in names() of Rdata.rds, i.e.,\n\t%s\n", 
#>                     name, paste(names(rds), collapse = ",")), 
#>                     domain = NA)
#>             }
#>             if (file_test("-f", file.path(p, "Rdata.zip"))) {
#>                 warning("zipped data found for package ", sQuote(basename(dirname(p))), 
#>                   ".\nThat is defunct, so please re-install the package.", 
#>                   domain = NA)
#>                 if (file_test("-f", fp <- file.path(p, "filelist"))) 
#>                   files <- file.path(p, scan(fp, what = "", quiet = TRUE))
#>                 else {
#>                   warning(gettextf("file 'filelist' is missing for directory %s", 
#>                     sQuote(p)), domain = NA)
#>                   next
#>                 }
#>             }
#>             else {
#>                 files <- list.files(p, full.names = TRUE)
#>             }
#>             files <- files[grep(name, files, fixed = TRUE)]
#>             if (length(files) > 1L) {
#>                 o <- match(fileExt(files), dataExts, nomatch = 100L)
#>                 paths0 <- dirname(files)
#>                 paths0 <- factor(paths0, levels = unique(paths0))
#>                 files <- files[order(paths0, o)]
#>             }
#>             if (length(files)) {
#>                 for (file in files) {
#>                   if (verbose) 
#>                     message("name=", name, ":\t file= ...", .Platform$file.sep, 
#>                       basename(file), "::\t", appendLF = FALSE, 
#>                       domain = NA)
#>                   ext <- fileExt(file)
#>                   if (basename(file) != paste0(name, ".", ext)) 
#>                     found <- FALSE
#>                   else {
#>                     found <- TRUE
#>                     zfile <- file
#>                     zipname <- file.path(dirname(file), "Rdata.zip")
#>                     if (file.exists(zipname)) {
#>                       Rdatadir <- tempfile("Rdata")
#>                       dir.create(Rdatadir, showWarnings = FALSE)
#>                       topic <- basename(file)
#>                       rc <- .External(C_unzip, zipname, topic, 
#>                         Rdatadir, FALSE, TRUE, FALSE, FALSE)
#>                       if (rc == 0L) 
#>                         zfile <- file.path(Rdatadir, topic)
#>                     }
#>                     if (zfile != file) 
#>                       on.exit(unlink(zfile))
#>                     switch(ext, R = , r = {
#>                       library("utils")
#>                       sys.source(zfile, chdir = TRUE, envir = tmp_env)
#>                     }, RData = , rdata = , rda = load(zfile, 
#>                       envir = tmp_env), TXT = , txt = , tab = , 
#>                       tab.gz = , tab.bz2 = , tab.xz = , txt.gz = , 
#>                       txt.bz2 = , txt.xz = assign(name, read.table(zfile, 
#>                         header = TRUE, as.is = FALSE), envir = tmp_env), 
#>                       CSV = , csv = , csv.gz = , csv.bz2 = , 
#>                       csv.xz = assign(name, read.table(zfile, 
#>                         header = TRUE, sep = ";", as.is = FALSE), 
#>                         envir = tmp_env), found <- FALSE)
#>                   }
#>                   if (found) 
#>                     break
#>                 }
#>                 if (verbose) 
#>                   message(if (!found) 
#>                     "*NOT* ", "found", domain = NA)
#>             }
#>             if (found) 
#>                 break
#>         }
#>         if (!found) {
#>             warning(gettextf("data set %s not found", sQuote(name)), 
#>                 domain = NA)
#>         }
#>         else if (!overwrite) {
#>             for (o in ls(envir = tmp_env, all.names = TRUE)) {
#>                 if (exists(o, envir = envir, inherits = FALSE)) 
#>                   warning(gettextf("an object named %s already exists and will not be overwritten", 
#>                     sQuote(o)))
#>                 else assign(o, get(o, envir = tmp_env, inherits = FALSE), 
#>                   envir = envir)
#>             }
#>             rm(tmp_env)
#>         }
#>     }
#>     invisible(names)
#> }
#> <bytecode: 0x000000001d37c0d8>
#> <environment: namespace:utils>
```

<img src="man/figures/README-unnamed-chunk-5-2.png" width="100%" />

``` r
target.obj$diagnosis_t_overlap()
#> function (..., list = character(), package = NULL, lib.loc = NULL, 
#>     verbose = getOption("verbose"), envir = .GlobalEnv, overwrite = TRUE) 
#> {
#>     fileExt <- function(x) {
#>         db <- grepl("\\.[^.]+\\.(gz|bz2|xz)$", x)
#>         ans <- sub(".*\\.", "", x)
#>         ans[db] <- sub(".*\\.([^.]+\\.)(gz|bz2|xz)$", "\\1\\2", 
#>             x[db])
#>         ans
#>     }
#>     names <- c(as.character(substitute(list(...))[-1L]), list)
#>     if (!is.null(package)) {
#>         if (!is.character(package)) 
#>             stop("'package' must be a character string or NULL")
#>         if (any(package %in% "base")) 
#>             warning("datasets have been moved from package 'base' to package 'datasets'")
#>         if (any(package %in% "stats")) 
#>             warning("datasets have been moved from package 'stats' to package 'datasets'")
#>         package[package %in% c("base", "stats")] <- "datasets"
#>     }
#>     paths <- find.package(package, lib.loc, verbose = verbose)
#>     if (is.null(lib.loc)) 
#>         paths <- c(path.package(package, TRUE), if (!length(package)) getwd(), 
#>             paths)
#>     paths <- unique(normalizePath(paths[file.exists(paths)]))
#>     paths <- paths[dir.exists(file.path(paths, "data"))]
#>     dataExts <- tools:::.make_file_exts("data")
#>     if (length(names) == 0L) {
#>         db <- matrix(character(), nrow = 0L, ncol = 4L)
#>         for (path in paths) {
#>             entries <- NULL
#>             packageName <- if (file_test("-f", file.path(path, 
#>                 "DESCRIPTION"))) 
#>                 basename(path)
#>             else "."
#>             if (file_test("-f", INDEX <- file.path(path, "Meta", 
#>                 "data.rds"))) {
#>                 entries <- readRDS(INDEX)
#>             }
#>             else {
#>                 dataDir <- file.path(path, "data")
#>                 entries <- tools::list_files_with_type(dataDir, 
#>                   "data")
#>                 if (length(entries)) {
#>                   entries <- unique(tools::file_path_sans_ext(basename(entries)))
#>                   entries <- cbind(entries, "")
#>                 }
#>             }
#>             if (NROW(entries)) {
#>                 if (is.matrix(entries) && ncol(entries) == 2L) 
#>                   db <- rbind(db, cbind(packageName, dirname(path), 
#>                     entries))
#>                 else warning(gettextf("data index for package %s is invalid and will be ignored", 
#>                   sQuote(packageName)), domain = NA, call. = FALSE)
#>             }
#>         }
#>         colnames(db) <- c("Package", "LibPath", "Item", "Title")
#>         footer <- if (missing(package)) 
#>             paste0("Use ", sQuote(paste("data(package =", ".packages(all.available = TRUE))")), 
#>                 "\n", "to list the data sets in all *available* packages.")
#>         else NULL
#>         y <- list(title = "Data sets", header = NULL, results = db, 
#>             footer = footer)
#>         class(y) <- "packageIQR"
#>         return(y)
#>     }
#>     paths <- file.path(paths, "data")
#>     for (name in names) {
#>         found <- FALSE
#>         for (p in paths) {
#>             tmp_env <- if (overwrite) 
#>                 envir
#>             else new.env()
#>             if (file_test("-f", file.path(p, "Rdata.rds"))) {
#>                 rds <- readRDS(file.path(p, "Rdata.rds"))
#>                 if (name %in% names(rds)) {
#>                   found <- TRUE
#>                   if (verbose) 
#>                     message(sprintf("name=%s:\t found in Rdata.rds", 
#>                       name), domain = NA)
#>                   thispkg <- sub(".*/([^/]*)/data$", "\\1", p)
#>                   thispkg <- sub("_.*$", "", thispkg)
#>                   thispkg <- paste0("package:", thispkg)
#>                   objs <- rds[[name]]
#>                   lazyLoad(file.path(p, "Rdata"), envir = tmp_env, 
#>                     filter = function(x) x %in% objs)
#>                   break
#>                 }
#>                 else if (verbose) 
#>                   message(sprintf("name=%s:\t NOT found in names() of Rdata.rds, i.e.,\n\t%s\n", 
#>                     name, paste(names(rds), collapse = ",")), 
#>                     domain = NA)
#>             }
#>             if (file_test("-f", file.path(p, "Rdata.zip"))) {
#>                 warning("zipped data found for package ", sQuote(basename(dirname(p))), 
#>                   ".\nThat is defunct, so please re-install the package.", 
#>                   domain = NA)
#>                 if (file_test("-f", fp <- file.path(p, "filelist"))) 
#>                   files <- file.path(p, scan(fp, what = "", quiet = TRUE))
#>                 else {
#>                   warning(gettextf("file 'filelist' is missing for directory %s", 
#>                     sQuote(p)), domain = NA)
#>                   next
#>                 }
#>             }
#>             else {
#>                 files <- list.files(p, full.names = TRUE)
#>             }
#>             files <- files[grep(name, files, fixed = TRUE)]
#>             if (length(files) > 1L) {
#>                 o <- match(fileExt(files), dataExts, nomatch = 100L)
#>                 paths0 <- dirname(files)
#>                 paths0 <- factor(paths0, levels = unique(paths0))
#>                 files <- files[order(paths0, o)]
#>             }
#>             if (length(files)) {
#>                 for (file in files) {
#>                   if (verbose) 
#>                     message("name=", name, ":\t file= ...", .Platform$file.sep, 
#>                       basename(file), "::\t", appendLF = FALSE, 
#>                       domain = NA)
#>                   ext <- fileExt(file)
#>                   if (basename(file) != paste0(name, ".", ext)) 
#>                     found <- FALSE
#>                   else {
#>                     found <- TRUE
#>                     zfile <- file
#>                     zipname <- file.path(dirname(file), "Rdata.zip")
#>                     if (file.exists(zipname)) {
#>                       Rdatadir <- tempfile("Rdata")
#>                       dir.create(Rdatadir, showWarnings = FALSE)
#>                       topic <- basename(file)
#>                       rc <- .External(C_unzip, zipname, topic, 
#>                         Rdatadir, FALSE, TRUE, FALSE, FALSE)
#>                       if (rc == 0L) 
#>                         zfile <- file.path(Rdatadir, topic)
#>                     }
#>                     if (zfile != file) 
#>                       on.exit(unlink(zfile))
#>                     switch(ext, R = , r = {
#>                       library("utils")
#>                       sys.source(zfile, chdir = TRUE, envir = tmp_env)
#>                     }, RData = , rdata = , rda = load(zfile, 
#>                       envir = tmp_env), TXT = , txt = , tab = , 
#>                       tab.gz = , tab.bz2 = , tab.xz = , txt.gz = , 
#>                       txt.bz2 = , txt.xz = assign(name, read.table(zfile, 
#>                         header = TRUE, as.is = FALSE), envir = tmp_env), 
#>                       CSV = , csv = , csv.gz = , csv.bz2 = , 
#>                       csv.xz = assign(name, read.table(zfile, 
#>                         header = TRUE, sep = ";", as.is = FALSE), 
#>                         envir = tmp_env), found <- FALSE)
#>                   }
#>                   if (found) 
#>                     break
#>                 }
#>                 if (verbose) 
#>                   message(if (!found) 
#>                     "*NOT* ", "found", domain = NA)
#>             }
#>             if (found) 
#>                 break
#>         }
#>         if (!found) {
#>             warning(gettextf("data set %s not found", sQuote(name)), 
#>                 domain = NA)
#>         }
#>         else if (!overwrite) {
#>             for (o in ls(envir = tmp_env, all.names = TRUE)) {
#>                 if (exists(o, envir = envir, inherits = FALSE)) 
#>                   warning(gettextf("an object named %s already exists and will not be overwritten", 
#>                     sQuote(o)))
#>                 else assign(o, get(o, envir = tmp_env, inherits = FALSE), 
#>                   envir = envir)
#>             }
#>             rm(tmp_env)
#>         }
#>     }
#>     invisible(names)
#> }
#> <bytecode: 0x000000001d37c0d8>
#> <environment: namespace:utils>
```

<img src="man/figures/README-unnamed-chunk-5-3.png" width="100%" />

``` r
source.obj.rep$diagnosis_s_overlap()
#>      x1 x2 x3 x4 x5 x6 study                    group_name
#> 1     0  0  0  0  0  0   RWD x1=0,x2=0,x3=0,x4=0,x5=0,x6=0
#> 2     0  0  0  0  0  0   RWD x1=0,x2=0,x3=0,x4=0,x5=0,x6=0
#> 3     0  0  0  0  0  0   RWD x1=0,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4     0  0  0  0  0  0   RWD x1=0,x2=0,x3=0,x4=0,x5=0,x6=0
#> 5     0  0  0  0  0  0   RWD x1=0,x2=0,x3=0,x4=0,x5=0,x6=0
#> 6     0  0  0  0  0  0   RWD x1=0,x2=0,x3=0,x4=0,x5=0,x6=0
#> 7     0  0  0  0  0  0   RWD x1=0,x2=0,x3=0,x4=0,x5=0,x6=0
#> 8     0  0  0  0  0  0   RWD x1=0,x2=0,x3=0,x4=0,x5=0,x6=0
#> 9     0  0  0  0  0  0   RWD x1=0,x2=0,x3=0,x4=0,x5=0,x6=0
#> 10    0  0  0  0  0  0   RWD x1=0,x2=0,x3=0,x4=0,x5=0,x6=0
#> 11    0  0  0  0  0  1   RWD x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 12    0  0  0  0  0  1   RWD x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 13    0  0  0  0  0  1   RWD x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 14    0  0  0  0  0  1   RWD x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 15    0  0  0  0  0  1   RWD x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 16    0  0  0  0  0  1   RWD x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 17    0  0  0  0  0  1   RWD x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 18    0  0  0  0  0  1   RWD x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 19    0  0  0  0  0  1   RWD x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 20    0  0  0  0  0  1   RWD x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 21    0  0  0  0  0  1   RWD x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 22    0  0  0  0  0  1   RWD x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 23    0  0  0  0  0  1   RWD x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 24    0  0  0  0  0  1   RWD x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 25    0  0  0  0  0  1   RWD x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 26    0  0  0  0  0  1   RWD x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 27    0  0  0  0  0  1   RWD x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 28    0  0  0  0  0  1   RWD x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 29    0  0  0  0  0  1   RWD x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 30    0  0  0  0  0  1   RWD x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 31    0  0  0  0  0  1   RWD x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 32    0  0  0  0  0  1   RWD x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 33    0  0  0  0  0  1   RWD x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 34    0  0  0  0  0  1   RWD x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 35    0  0  0  0  0  1   RWD x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 36    0  0  0  0  0  1   RWD x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 37    0  0  0  0  0  1   RWD x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 38    0  0  0  0  0  1   RWD x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 39    0  0  0  0  0  1   RWD x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 40    0  0  0  0  0  1   RWD x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 41    0  0  0  0  0  1   RWD x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 42    0  0  0  0  0  1   RWD x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 43    0  0  0  0  0  1   RWD x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 44    0  0  0  0  0  1   RWD x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 45    0  0  0  0  0  1   RWD x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 46    0  0  0  0  0  1   RWD x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 47    0  0  0  0  0  1   RWD x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 48    0  0  0  0  0  1   RWD x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 49    0  0  0  0  0  1   RWD x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 50    0  0  0  0  1  0   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=0
#> 51    0  0  0  0  1  0   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=0
#> 52    0  0  0  0  1  0   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=0
#> 53    0  0  0  0  1  0   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=0
#> 54    0  0  0  0  1  0   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=0
#> 55    0  0  0  0  1  0   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=0
#> 56    0  0  0  0  1  0   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=0
#> 57    0  0  0  0  1  0   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=0
#> 58    0  0  0  0  1  0   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=0
#> 59    0  0  0  0  1  0   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=0
#> 60    0  0  0  0  1  0   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=0
#> 61    0  0  0  0  1  0   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=0
#> 62    0  0  0  0  1  0   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=0
#> 63    0  0  0  0  1  0   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=0
#> 64    0  0  0  0  1  0   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=0
#> 65    0  0  0  0  1  0   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=0
#> 66    0  0  0  0  1  0   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=0
#> 67    0  0  0  0  1  0   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=0
#> 68    0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 69    0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 70    0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 71    0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 72    0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 73    0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 74    0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 75    0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 76    0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 77    0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 78    0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 79    0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 80    0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 81    0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 82    0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 83    0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 84    0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 85    0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 86    0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 87    0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 88    0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 89    0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 90    0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 91    0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 92    0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 93    0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 94    0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 95    0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 96    0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 97    0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 98    0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 99    0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 100   0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 101   0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 102   0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 103   0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 104   0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 105   0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 106   0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 107   0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 108   0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 109   0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 110   0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 111   0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 112   0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 113   0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 114   0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 115   0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 116   0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 117   0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 118   0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 119   0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 120   0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 121   0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 122   0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 123   0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 124   0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 125   0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 126   0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 127   0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 128   0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 129   0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 130   0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 131   0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 132   0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 133   0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 134   0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 135   0  0  0  0  1  1   RWD x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 136   0  0  0  1  0  0   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=0
#> 137   0  0  0  1  0  0   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=0
#> 138   0  0  0  1  0  0   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=0
#> 139   0  0  0  1  0  0   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=0
#> 140   0  0  0  1  0  0   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=0
#> 141   0  0  0  1  0  0   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=0
#> 142   0  0  0  1  0  0   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=0
#> 143   0  0  0  1  0  0   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=0
#> 144   0  0  0  1  0  0   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=0
#> 145   0  0  0  1  0  0   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=0
#> 146   0  0  0  1  0  0   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=0
#> 147   0  0  0  1  0  0   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=0
#> 148   0  0  0  1  0  0   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=0
#> 149   0  0  0  1  0  0   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=0
#> 150   0  0  0  1  0  0   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=0
#> 151   0  0  0  1  0  0   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=0
#> 152   0  0  0  1  0  0   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=0
#> 153   0  0  0  1  0  0   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=0
#> 154   0  0  0  1  0  0   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=0
#> 155   0  0  0  1  0  0   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=0
#> 156   0  0  0  1  0  0   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=0
#> 157   0  0  0  1  0  0   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=0
#> 158   0  0  0  1  0  0   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=0
#> 159   0  0  0  1  0  0   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=0
#> 160   0  0  0  1  0  0   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=0
#> 161   0  0  0  1  0  0   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=0
#> 162   0  0  0  1  0  0   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=0
#> 163   0  0  0  1  0  0   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=0
#> 164   0  0  0  1  0  0   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=0
#> 165   0  0  0  1  0  0   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=0
#> 166   0  0  0  1  0  0   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=0
#> 167   0  0  0  1  0  0   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=0
#> 168   0  0  0  1  0  0   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=0
#> 169   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 170   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 171   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 172   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 173   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 174   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 175   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 176   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 177   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 178   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 179   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 180   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 181   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 182   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 183   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 184   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 185   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 186   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 187   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 188   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 189   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 190   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 191   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 192   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 193   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 194   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 195   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 196   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 197   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 198   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 199   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 200   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 201   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 202   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 203   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 204   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 205   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 206   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 207   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 208   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 209   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 210   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 211   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 212   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 213   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 214   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 215   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 216   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 217   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 218   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 219   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 220   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 221   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 222   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 223   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 224   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 225   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 226   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 227   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 228   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 229   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 230   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 231   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 232   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 233   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 234   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 235   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 236   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 237   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 238   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 239   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 240   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 241   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 242   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 243   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 244   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 245   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 246   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 247   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 248   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 249   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 250   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 251   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 252   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 253   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 254   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 255   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 256   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 257   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 258   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 259   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 260   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 261   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 262   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 263   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 264   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 265   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 266   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 267   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 268   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 269   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 270   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 271   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 272   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 273   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 274   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 275   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 276   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 277   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 278   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 279   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 280   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 281   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 282   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 283   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 284   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 285   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 286   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 287   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 288   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 289   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 290   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 291   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 292   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 293   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 294   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 295   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 296   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 297   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 298   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 299   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 300   0  0  0  1  0  1   RWD x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 301   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 302   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 303   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 304   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 305   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 306   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 307   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 308   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 309   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 310   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 311   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 312   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 313   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 314   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 315   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 316   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 317   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 318   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 319   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 320   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 321   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 322   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 323   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 324   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 325   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 326   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 327   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 328   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 329   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 330   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 331   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 332   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 333   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 334   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 335   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 336   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 337   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 338   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 339   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 340   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 341   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 342   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 343   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 344   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 345   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 346   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 347   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 348   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 349   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 350   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 351   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 352   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 353   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 354   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 355   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 356   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 357   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 358   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 359   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 360   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 361   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 362   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 363   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 364   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 365   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 366   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 367   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 368   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 369   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 370   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 371   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 372   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 373   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 374   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 375   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 376   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 377   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 378   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 379   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 380   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 381   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 382   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 383   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 384   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 385   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 386   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 387   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 388   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 389   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 390   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 391   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 392   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 393   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 394   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 395   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 396   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 397   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 398   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 399   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 400   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 401   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 402   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 403   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 404   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 405   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 406   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 407   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 408   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 409   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 410   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 411   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 412   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 413   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 414   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 415   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 416   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 417   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 418   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 419   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 420   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 421   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 422   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 423   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 424   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 425   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 426   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 427   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 428   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 429   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 430   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 431   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 432   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 433   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 434   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 435   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 436   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 437   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 438   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 439   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 440   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 441   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 442   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 443   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 444   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 445   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 446   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 447   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 448   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 449   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 450   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 451   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 452   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 453   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 454   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 455   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 456   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 457   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 458   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 459   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 460   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 461   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 462   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 463   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 464   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 465   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 466   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 467   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 468   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 469   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 470   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 471   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 472   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 473   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 474   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 475   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 476   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 477   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 478   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 479   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 480   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 481   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 482   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 483   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 484   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 485   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 486   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 487   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 488   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 489   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 490   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 491   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 492   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 493   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 494   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 495   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 496   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 497   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 498   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 499   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 500   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 501   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 502   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 503   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 504   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 505   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 506   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 507   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 508   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 509   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 510   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 511   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 512   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 513   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 514   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 515   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 516   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 517   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 518   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 519   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 520   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 521   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 522   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 523   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 524   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 525   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 526   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 527   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 528   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 529   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 530   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 531   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 532   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 533   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 534   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 535   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 536   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 537   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 538   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 539   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 540   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 541   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 542   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 543   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 544   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 545   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 546   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 547   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 548   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 549   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 550   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 551   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 552   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 553   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 554   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 555   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 556   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 557   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 558   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 559   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 560   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 561   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 562   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 563   0  0  0  1  1  1   RWD x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 564   0  0  1  0  0  0   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=0
#> 565   0  0  1  0  0  0   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=0
#> 566   0  0  1  0  0  0   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=0
#> 567   0  0  1  0  0  0   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=0
#> 568   0  0  1  0  0  0   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=0
#> 569   0  0  1  0  0  0   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=0
#> 570   0  0  1  0  0  0   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=0
#> 571   0  0  1  0  0  0   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=0
#> 572   0  0  1  0  0  0   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=0
#> 573   0  0  1  0  0  0   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=0
#> 574   0  0  1  0  0  0   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=0
#> 575   0  0  1  0  0  0   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=0
#> 576   0  0  1  0  0  0   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=0
#> 577   0  0  1  0  0  0   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=0
#> 578   0  0  1  0  0  0   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=0
#> 579   0  0  1  0  0  0   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=0
#> 580   0  0  1  0  0  0   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=0
#> 581   0  0  1  0  0  0   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=0
#> 582   0  0  1  0  0  0   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=0
#> 583   0  0  1  0  0  0   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=0
#> 584   0  0  1  0  0  0   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=0
#> 585   0  0  1  0  0  0   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=0
#> 586   0  0  1  0  0  0   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=0
#> 587   0  0  1  0  0  0   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=0
#> 588   0  0  1  0  0  0   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=0
#> 589   0  0  1  0  0  0   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=0
#> 590   0  0  1  0  0  0   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=0
#> 591   0  0  1  0  0  0   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=0
#> 592   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 593   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 594   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 595   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 596   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 597   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 598   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 599   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 600   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 601   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 602   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 603   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 604   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 605   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 606   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 607   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 608   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 609   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 610   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 611   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 612   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 613   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 614   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 615   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 616   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 617   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 618   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 619   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 620   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 621   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 622   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 623   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 624   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 625   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 626   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 627   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 628   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 629   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 630   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 631   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 632   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 633   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 634   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 635   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 636   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 637   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 638   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 639   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 640   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 641   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 642   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 643   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 644   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 645   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 646   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 647   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 648   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 649   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 650   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 651   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 652   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 653   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 654   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 655   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 656   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 657   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 658   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 659   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 660   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 661   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 662   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 663   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 664   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 665   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 666   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 667   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 668   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 669   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 670   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 671   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 672   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 673   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 674   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 675   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 676   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 677   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 678   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 679   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 680   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 681   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 682   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 683   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 684   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 685   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 686   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 687   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 688   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 689   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 690   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 691   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 692   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 693   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 694   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 695   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 696   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 697   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 698   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 699   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 700   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 701   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 702   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 703   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 704   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 705   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 706   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 707   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 708   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 709   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 710   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 711   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 712   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 713   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 714   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 715   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 716   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 717   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 718   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 719   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 720   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 721   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 722   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 723   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 724   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 725   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 726   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 727   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 728   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 729   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 730   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 731   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 732   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 733   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 734   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 735   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 736   0  0  1  0  0  1   RWD x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 737   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 738   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 739   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 740   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 741   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 742   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 743   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 744   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 745   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 746   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 747   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 748   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 749   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 750   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 751   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 752   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 753   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 754   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 755   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 756   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 757   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 758   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 759   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 760   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 761   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 762   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 763   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 764   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 765   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 766   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 767   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 768   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 769   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 770   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 771   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 772   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 773   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 774   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 775   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 776   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 777   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 778   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 779   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 780   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 781   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 782   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 783   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 784   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 785   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 786   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 787   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 788   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 789   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 790   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 791   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 792   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 793   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 794   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 795   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 796   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 797   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 798   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 799   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 800   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 801   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 802   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 803   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 804   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 805   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 806   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 807   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 808   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 809   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 810   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 811   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 812   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 813   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 814   0  0  1  0  1  0   RWD x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 815   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 816   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 817   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 818   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 819   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 820   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 821   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 822   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 823   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 824   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 825   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 826   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 827   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 828   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 829   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 830   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 831   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 832   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 833   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 834   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 835   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 836   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 837   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 838   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 839   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 840   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 841   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 842   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 843   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 844   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 845   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 846   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 847   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 848   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 849   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 850   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 851   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 852   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 853   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 854   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 855   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 856   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 857   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 858   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 859   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 860   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 861   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 862   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 863   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 864   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 865   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 866   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 867   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 868   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 869   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 870   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 871   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 872   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 873   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 874   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 875   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 876   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 877   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 878   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 879   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 880   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 881   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 882   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 883   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 884   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 885   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 886   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 887   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 888   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 889   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 890   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 891   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 892   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 893   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 894   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 895   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 896   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 897   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 898   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 899   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 900   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 901   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 902   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 903   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 904   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 905   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 906   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 907   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 908   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 909   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 910   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 911   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 912   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 913   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 914   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 915   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 916   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 917   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 918   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 919   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 920   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 921   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 922   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 923   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 924   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 925   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 926   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 927   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 928   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 929   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 930   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 931   0  0  1  1  0  0   RWD x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 932   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 933   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 934   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 935   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 936   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 937   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 938   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 939   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 940   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 941   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 942   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 943   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 944   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 945   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 946   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 947   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 948   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 949   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 950   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 951   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 952   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 953   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 954   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 955   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 956   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 957   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 958   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 959   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 960   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 961   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 962   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 963   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 964   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 965   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 966   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 967   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 968   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 969   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 970   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 971   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 972   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 973   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 974   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 975   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 976   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 977   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 978   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 979   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 980   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 981   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 982   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 983   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 984   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 985   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 986   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 987   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 988   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 989   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 990   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 991   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 992   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 993   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 994   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 995   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 996   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 997   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 998   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 999   0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1000  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1001  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1002  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1003  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1004  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1005  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1006  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1007  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1008  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1009  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1010  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1011  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1012  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1013  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1014  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1015  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1016  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1017  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1018  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1019  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1020  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1021  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1022  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1023  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1024  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1025  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1026  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1027  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1028  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1029  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1030  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1031  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1032  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1033  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1034  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1035  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1036  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1037  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1038  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1039  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1040  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1041  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1042  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1043  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1044  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1045  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1046  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1047  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1048  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1049  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1050  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1051  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1052  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1053  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1054  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1055  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1056  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1057  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1058  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1059  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1060  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1061  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1062  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1063  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1064  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1065  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1066  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1067  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1068  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1069  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1070  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1071  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1072  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1073  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1074  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1075  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1076  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1077  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1078  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1079  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1080  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1081  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1082  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1083  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1084  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1085  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1086  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1087  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1088  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1089  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1090  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1091  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1092  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1093  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1094  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1095  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1096  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1097  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1098  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1099  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1100  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1101  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1102  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1103  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1104  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1105  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1106  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1107  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1108  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1109  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1110  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1111  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1112  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1113  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1114  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1115  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1116  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1117  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1118  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1119  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1120  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1121  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1122  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1123  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1124  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1125  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1126  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1127  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1128  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1129  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1130  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1131  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1132  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1133  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1134  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1135  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1136  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1137  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1138  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1139  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1140  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1141  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1142  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1143  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1144  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1145  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1146  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1147  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1148  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1149  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1150  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1151  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1152  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1153  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1154  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1155  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1156  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1157  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1158  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1159  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1160  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1161  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1162  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1163  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1164  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1165  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1166  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1167  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1168  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1169  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1170  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1171  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1172  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1173  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1174  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1175  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1176  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1177  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1178  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1179  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1180  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1181  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1182  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1183  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1184  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1185  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1186  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1187  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1188  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1189  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1190  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1191  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1192  0  0  1  1  1  0   RWD x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 1193  0  1  0  0  0  0   RWD x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 1194  0  1  0  0  0  0   RWD x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 1195  0  1  0  0  0  1   RWD x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 1196  0  1  0  0  0  1   RWD x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 1197  0  1  0  0  0  1   RWD x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 1198  0  1  0  0  0  1   RWD x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 1199  0  1  0  0  0  1   RWD x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 1200  0  1  0  0  0  1   RWD x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 1201  0  1  0  0  1  0   RWD x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 1202  0  1  0  0  1  0   RWD x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 1203  0  1  0  0  1  0   RWD x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 1204  0  1  0  0  1  0   RWD x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 1205  0  1  0  0  1  0   RWD x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 1206  0  1  0  0  1  1   RWD x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 1207  0  1  0  0  1  1   RWD x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 1208  0  1  0  0  1  1   RWD x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 1209  0  1  0  0  1  1   RWD x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 1210  0  1  0  0  1  1   RWD x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 1211  0  1  0  0  1  1   RWD x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 1212  0  1  0  0  1  1   RWD x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 1213  0  1  0  0  1  1   RWD x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 1214  0  1  0  0  1  1   RWD x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 1215  0  1  0  0  1  1   RWD x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 1216  0  1  0  0  1  1   RWD x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 1217  0  1  0  0  1  1   RWD x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 1218  0  1  0  0  1  1   RWD x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 1219  0  1  0  1  0  0   RWD x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 1220  0  1  0  1  0  0   RWD x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 1221  0  1  0  1  0  0   RWD x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 1222  0  1  0  1  0  0   RWD x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 1223  0  1  0  1  0  0   RWD x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 1224  0  1  0  1  0  0   RWD x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 1225  0  1  0  1  0  0   RWD x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 1226  0  1  0  1  0  0   RWD x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 1227  0  1  0  1  0  0   RWD x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 1228  0  1  0  1  0  0   RWD x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 1229  0  1  0  1  0  1   RWD x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 1230  0  1  0  1  0  1   RWD x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 1231  0  1  0  1  0  1   RWD x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 1232  0  1  0  1  0  1   RWD x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 1233  0  1  0  1  0  1   RWD x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 1234  0  1  0  1  0  1   RWD x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 1235  0  1  0  1  0  1   RWD x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 1236  0  1  0  1  0  1   RWD x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 1237  0  1  0  1  0  1   RWD x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 1238  0  1  0  1  0  1   RWD x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 1239  0  1  0  1  0  1   RWD x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 1240  0  1  0  1  0  1   RWD x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 1241  0  1  0  1  0  1   RWD x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 1242  0  1  0  1  0  1   RWD x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 1243  0  1  0  1  0  1   RWD x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 1244  0  1  0  1  0  1   RWD x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 1245  0  1  0  1  0  1   RWD x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 1246  0  1  0  1  0  1   RWD x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 1247  0  1  0  1  0  1   RWD x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 1248  0  1  0  1  0  1   RWD x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 1249  0  1  0  1  0  1   RWD x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 1250  0  1  0  1  0  1   RWD x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 1251  0  1  0  1  0  1   RWD x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 1252  0  1  0  1  0  1   RWD x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 1253  0  1  0  1  0  1   RWD x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 1254  0  1  0  1  0  1   RWD x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 1255  0  1  0  1  0  1   RWD x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 1256  0  1  0  1  0  1   RWD x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 1257  0  1  0  1  0  1   RWD x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 1258  0  1  0  1  0  1   RWD x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 1259  0  1  0  1  0  1   RWD x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 1260  0  1  0  1  1  0   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 1261  0  1  0  1  1  0   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 1262  0  1  0  1  1  0   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 1263  0  1  0  1  1  0   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 1264  0  1  0  1  1  0   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 1265  0  1  0  1  1  0   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 1266  0  1  0  1  1  0   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 1267  0  1  0  1  1  0   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 1268  0  1  0  1  1  0   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 1269  0  1  0  1  1  0   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 1270  0  1  0  1  1  0   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 1271  0  1  0  1  1  0   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 1272  0  1  0  1  1  0   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 1273  0  1  0  1  1  0   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 1274  0  1  0  1  1  0   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 1275  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1276  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1277  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1278  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1279  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1280  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1281  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1282  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1283  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1284  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1285  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1286  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1287  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1288  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1289  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1290  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1291  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1292  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1293  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1294  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1295  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1296  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1297  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1298  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1299  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1300  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1301  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1302  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1303  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1304  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1305  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1306  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1307  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1308  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1309  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1310  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1311  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1312  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1313  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1314  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1315  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1316  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1317  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1318  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1319  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1320  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1321  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1322  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1323  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1324  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1325  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1326  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1327  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1328  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1329  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1330  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1331  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1332  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1333  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1334  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1335  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1336  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1337  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1338  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1339  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1340  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1341  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1342  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1343  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1344  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1345  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1346  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1347  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1348  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1349  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1350  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1351  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1352  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1353  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1354  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1355  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1356  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1357  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1358  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1359  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1360  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1361  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1362  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1363  0  1  0  1  1  1   RWD x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 1364  0  1  1  0  0  1   RWD x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 1365  0  1  1  0  0  1   RWD x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 1366  0  1  1  0  0  1   RWD x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 1367  0  1  1  0  0  1   RWD x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 1368  0  1  1  0  0  1   RWD x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 1369  0  1  1  0  0  1   RWD x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 1370  0  1  1  0  0  1   RWD x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 1371  0  1  1  0  0  1   RWD x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 1372  0  1  1  0  0  1   RWD x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 1373  0  1  1  0  0  1   RWD x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 1374  0  1  1  0  0  1   RWD x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 1375  0  1  1  0  0  1   RWD x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 1376  0  1  1  0  0  1   RWD x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 1377  0  1  1  0  0  1   RWD x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 1378  0  1  1  0  0  1   RWD x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 1379  0  1  1  0  0  1   RWD x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 1380  0  1  1  0  0  1   RWD x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 1381  0  1  1  0  0  1   RWD x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 1382  0  1  1  0  0  1   RWD x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 1383  0  1  1  0  0  1   RWD x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 1384  0  1  1  0  0  1   RWD x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 1385  0  1  1  0  0  1   RWD x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 1386  0  1  1  0  0  1   RWD x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 1387  0  1  1  0  0  1   RWD x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 1388  0  1  1  0  0  1   RWD x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 1389  0  1  1  0  0  1   RWD x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 1390  0  1  1  0  0  1   RWD x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 1391  0  1  1  0  0  1   RWD x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 1392  0  1  1  0  0  1   RWD x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 1393  0  1  1  0  1  0   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=0
#> 1394  0  1  1  0  1  0   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=0
#> 1395  0  1  1  0  1  0   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=0
#> 1396  0  1  1  0  1  0   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=0
#> 1397  0  1  1  0  1  0   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=0
#> 1398  0  1  1  0  1  0   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=0
#> 1399  0  1  1  0  1  0   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=0
#> 1400  0  1  1  0  1  0   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=0
#> 1401  0  1  1  0  1  0   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=0
#> 1402  0  1  1  0  1  0   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=0
#> 1403  0  1  1  0  1  0   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=0
#> 1404  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1405  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1406  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1407  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1408  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1409  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1410  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1411  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1412  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1413  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1414  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1415  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1416  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1417  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1418  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1419  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1420  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1421  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1422  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1423  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1424  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1425  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1426  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1427  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1428  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1429  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1430  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1431  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1432  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1433  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1434  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1435  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1436  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1437  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1438  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1439  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1440  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1441  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1442  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1443  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1444  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1445  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1446  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1447  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1448  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1449  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1450  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1451  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1452  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1453  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1454  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1455  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1456  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1457  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1458  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1459  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1460  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1461  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1462  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1463  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1464  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1465  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1466  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1467  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1468  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1469  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1470  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1471  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1472  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1473  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1474  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1475  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1476  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1477  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1478  0  1  1  0  1  1   RWD x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 1479  0  1  1  1  0  0   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 1480  0  1  1  1  0  0   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 1481  0  1  1  1  0  0   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 1482  0  1  1  1  0  0   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 1483  0  1  1  1  0  0   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 1484  0  1  1  1  0  0   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 1485  0  1  1  1  0  0   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 1486  0  1  1  1  0  0   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 1487  0  1  1  1  0  0   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 1488  0  1  1  1  0  0   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 1489  0  1  1  1  0  0   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 1490  0  1  1  1  0  0   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 1491  0  1  1  1  0  0   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 1492  0  1  1  1  0  0   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 1493  0  1  1  1  0  0   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 1494  0  1  1  1  0  0   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 1495  0  1  1  1  0  0   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 1496  0  1  1  1  0  0   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 1497  0  1  1  1  0  0   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 1498  0  1  1  1  0  0   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 1499  0  1  1  1  0  0   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 1500  0  1  1  1  0  0   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 1501  0  1  1  1  0  0   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 1502  0  1  1  1  0  0   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 1503  0  1  1  1  0  0   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 1504  0  1  1  1  0  0   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 1505  0  1  1  1  0  0   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 1506  0  1  1  1  0  0   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 1507  0  1  1  1  0  0   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 1508  0  1  1  1  0  0   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 1509  0  1  1  1  0  0   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 1510  0  1  1  1  0  0   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 1511  0  1  1  1  0  0   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 1512  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1513  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1514  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1515  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1516  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1517  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1518  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1519  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1520  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1521  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1522  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1523  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1524  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1525  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1526  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1527  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1528  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1529  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1530  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1531  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1532  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1533  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1534  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1535  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1536  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1537  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1538  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1539  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1540  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1541  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1542  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1543  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1544  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1545  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1546  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1547  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1548  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1549  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1550  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1551  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1552  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1553  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1554  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1555  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1556  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1557  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1558  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1559  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1560  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1561  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1562  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1563  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1564  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1565  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1566  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1567  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1568  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1569  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1570  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1571  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1572  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1573  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1574  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1575  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1576  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1577  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1578  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1579  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1580  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1581  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1582  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1583  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1584  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1585  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1586  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1587  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1588  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1589  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1590  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1591  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1592  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1593  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1594  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1595  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1596  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1597  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1598  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1599  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1600  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1601  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1602  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1603  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1604  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1605  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1606  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1607  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1608  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1609  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1610  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1611  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1612  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1613  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1614  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1615  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1616  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1617  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1618  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1619  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1620  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1621  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1622  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1623  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1624  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1625  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1626  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1627  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1628  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1629  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1630  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1631  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1632  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1633  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1634  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1635  0  1  1  1  0  1   RWD x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 1636  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1637  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1638  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1639  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1640  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1641  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1642  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1643  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1644  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1645  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1646  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1647  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1648  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1649  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1650  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1651  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1652  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1653  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1654  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1655  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1656  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1657  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1658  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1659  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1660  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1661  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1662  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1663  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1664  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1665  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1666  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1667  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1668  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1669  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1670  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1671  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1672  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1673  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1674  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1675  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1676  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1677  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1678  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1679  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1680  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1681  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1682  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1683  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1684  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1685  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1686  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1687  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1688  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1689  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1690  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1691  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1692  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1693  0  1  1  1  1  0   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 1694  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1695  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1696  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1697  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1698  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1699  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1700  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1701  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1702  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1703  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1704  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1705  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1706  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1707  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1708  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1709  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1710  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1711  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1712  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1713  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1714  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1715  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1716  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1717  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1718  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1719  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1720  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1721  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1722  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1723  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1724  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1725  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1726  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1727  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1728  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1729  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1730  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1731  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1732  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1733  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1734  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1735  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1736  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1737  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1738  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1739  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1740  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1741  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1742  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1743  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1744  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1745  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1746  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1747  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1748  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1749  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1750  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1751  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1752  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1753  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1754  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1755  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1756  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1757  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1758  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1759  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1760  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1761  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1762  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1763  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1764  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1765  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1766  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1767  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1768  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1769  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1770  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1771  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1772  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1773  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1774  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1775  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1776  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1777  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1778  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1779  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1780  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1781  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1782  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1783  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1784  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1785  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1786  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1787  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1788  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1789  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1790  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1791  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1792  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1793  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1794  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1795  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1796  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1797  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1798  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1799  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1800  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1801  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1802  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1803  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1804  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1805  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1806  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1807  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1808  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1809  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1810  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1811  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1812  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1813  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1814  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1815  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1816  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1817  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1818  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1819  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1820  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1821  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1822  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1823  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1824  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1825  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1826  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1827  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1828  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1829  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1830  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1831  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1832  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1833  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1834  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1835  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1836  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1837  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1838  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1839  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1840  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1841  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1842  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1843  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1844  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1845  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1846  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1847  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1848  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1849  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1850  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1851  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1852  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1853  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1854  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1855  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1856  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1857  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1858  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1859  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1860  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1861  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1862  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1863  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1864  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1865  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1866  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1867  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1868  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1869  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1870  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1871  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1872  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1873  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1874  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1875  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1876  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1877  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1878  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1879  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1880  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1881  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1882  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1883  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1884  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1885  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1886  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1887  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1888  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1889  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1890  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1891  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1892  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1893  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1894  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1895  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1896  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1897  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1898  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1899  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1900  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1901  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1902  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1903  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1904  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1905  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1906  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1907  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1908  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1909  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1910  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1911  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1912  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1913  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1914  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1915  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1916  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1917  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1918  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1919  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1920  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1921  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1922  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1923  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1924  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1925  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1926  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1927  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1928  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1929  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1930  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1931  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1932  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1933  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1934  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1935  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1936  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1937  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1938  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1939  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1940  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1941  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1942  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1943  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1944  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1945  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1946  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1947  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1948  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1949  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1950  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1951  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1952  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1953  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1954  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1955  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1956  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1957  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1958  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1959  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1960  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1961  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1962  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1963  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1964  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1965  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1966  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1967  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1968  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1969  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1970  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1971  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1972  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1973  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1974  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1975  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1976  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1977  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1978  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1979  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1980  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1981  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1982  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1983  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1984  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1985  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1986  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1987  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1988  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1989  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1990  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1991  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1992  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1993  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1994  0  1  1  1  1  1   RWD x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 1995  1  0  0  0  0  0   RWD x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 1996  1  0  0  0  0  0   RWD x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 1997  1  0  0  0  0  1   RWD x1=1,x2=0,x3=0,x4=0,x5=0,x6=1
#> 1998  1  0  0  0  0  1   RWD x1=1,x2=0,x3=0,x4=0,x5=0,x6=1
#> 1999  1  0  0  0  0  1   RWD x1=1,x2=0,x3=0,x4=0,x5=0,x6=1
#> 2000  1  0  0  0  0  1   RWD x1=1,x2=0,x3=0,x4=0,x5=0,x6=1
#> 2001  1  0  0  0  0  1   RWD x1=1,x2=0,x3=0,x4=0,x5=0,x6=1
#> 2002  1  0  0  0  0  1   RWD x1=1,x2=0,x3=0,x4=0,x5=0,x6=1
#> 2003  1  0  0  0  0  1   RWD x1=1,x2=0,x3=0,x4=0,x5=0,x6=1
#> 2004  1  0  0  0  1  0   RWD x1=1,x2=0,x3=0,x4=0,x5=1,x6=0
#> 2005  1  0  0  0  1  0   RWD x1=1,x2=0,x3=0,x4=0,x5=1,x6=0
#> 2006  1  0  0  0  1  0   RWD x1=1,x2=0,x3=0,x4=0,x5=1,x6=0
#> 2007  1  0  0  0  1  0   RWD x1=1,x2=0,x3=0,x4=0,x5=1,x6=0
#> 2008  1  0  0  0  1  1   RWD x1=1,x2=0,x3=0,x4=0,x5=1,x6=1
#> 2009  1  0  0  0  1  1   RWD x1=1,x2=0,x3=0,x4=0,x5=1,x6=1
#> 2010  1  0  0  0  1  1   RWD x1=1,x2=0,x3=0,x4=0,x5=1,x6=1
#> 2011  1  0  0  0  1  1   RWD x1=1,x2=0,x3=0,x4=0,x5=1,x6=1
#> 2012  1  0  0  0  1  1   RWD x1=1,x2=0,x3=0,x4=0,x5=1,x6=1
#> 2013  1  0  0  0  1  1   RWD x1=1,x2=0,x3=0,x4=0,x5=1,x6=1
#> 2014  1  0  0  0  1  1   RWD x1=1,x2=0,x3=0,x4=0,x5=1,x6=1
#> 2015  1  0  0  0  1  1   RWD x1=1,x2=0,x3=0,x4=0,x5=1,x6=1
#> 2016  1  0  0  0  1  1   RWD x1=1,x2=0,x3=0,x4=0,x5=1,x6=1
#> 2017  1  0  0  0  1  1   RWD x1=1,x2=0,x3=0,x4=0,x5=1,x6=1
#> 2018  1  0  0  0  1  1   RWD x1=1,x2=0,x3=0,x4=0,x5=1,x6=1
#> 2019  1  0  0  0  1  1   RWD x1=1,x2=0,x3=0,x4=0,x5=1,x6=1
#> 2020  1  0  0  0  1  1   RWD x1=1,x2=0,x3=0,x4=0,x5=1,x6=1
#> 2021  1  0  0  0  1  1   RWD x1=1,x2=0,x3=0,x4=0,x5=1,x6=1
#> 2022  1  0  0  0  1  1   RWD x1=1,x2=0,x3=0,x4=0,x5=1,x6=1
#> 2023  1  0  0  0  1  1   RWD x1=1,x2=0,x3=0,x4=0,x5=1,x6=1
#> 2024  1  0  0  0  1  1   RWD x1=1,x2=0,x3=0,x4=0,x5=1,x6=1
#> 2025  1  0  0  0  1  1   RWD x1=1,x2=0,x3=0,x4=0,x5=1,x6=1
#> 2026  1  0  0  1  0  0   RWD x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 2027  1  0  0  1  0  0   RWD x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 2028  1  0  0  1  0  0   RWD x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 2029  1  0  0  1  0  0   RWD x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 2030  1  0  0  1  0  0   RWD x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 2031  1  0  0  1  0  0   RWD x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 2032  1  0  0  1  0  0   RWD x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 2033  1  0  0  1  0  0   RWD x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 2034  1  0  0  1  0  0   RWD x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 2035  1  0  0  1  0  0   RWD x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 2036  1  0  0  1  0  1   RWD x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 2037  1  0  0  1  0  1   RWD x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 2038  1  0  0  1  0  1   RWD x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 2039  1  0  0  1  0  1   RWD x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 2040  1  0  0  1  0  1   RWD x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 2041  1  0  0  1  0  1   RWD x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 2042  1  0  0  1  0  1   RWD x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 2043  1  0  0  1  0  1   RWD x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 2044  1  0  0  1  0  1   RWD x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 2045  1  0  0  1  0  1   RWD x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 2046  1  0  0  1  0  1   RWD x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 2047  1  0  0  1  0  1   RWD x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 2048  1  0  0  1  0  1   RWD x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 2049  1  0  0  1  0  1   RWD x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 2050  1  0  0  1  0  1   RWD x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 2051  1  0  0  1  0  1   RWD x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 2052  1  0  0  1  0  1   RWD x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 2053  1  0  0  1  0  1   RWD x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 2054  1  0  0  1  0  1   RWD x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 2055  1  0  0  1  0  1   RWD x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 2056  1  0  0  1  0  1   RWD x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 2057  1  0  0  1  0  1   RWD x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 2058  1  0  0  1  0  1   RWD x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 2059  1  0  0  1  0  1   RWD x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 2060  1  0  0  1  0  1   RWD x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 2061  1  0  0  1  0  1   RWD x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 2062  1  0  0  1  0  1   RWD x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 2063  1  0  0  1  0  1   RWD x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 2064  1  0  0  1  0  1   RWD x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 2065  1  0  0  1  0  1   RWD x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 2066  1  0  0  1  0  1   RWD x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 2067  1  0  0  1  0  1   RWD x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 2068  1  0  0  1  0  1   RWD x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 2069  1  0  0  1  0  1   RWD x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 2070  1  0  0  1  0  1   RWD x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 2071  1  0  0  1  1  0   RWD x1=1,x2=0,x3=0,x4=1,x5=1,x6=0
#> 2072  1  0  0  1  1  0   RWD x1=1,x2=0,x3=0,x4=1,x5=1,x6=0
#> 2073  1  0  0  1  1  0   RWD x1=1,x2=0,x3=0,x4=1,x5=1,x6=0
#> 2074  1  0  0  1  1  0   RWD x1=1,x2=0,x3=0,x4=1,x5=1,x6=0
#> 2075  1  0  0  1  1  0   RWD x1=1,x2=0,x3=0,x4=1,x5=1,x6=0
#> 2076  1  0  0  1  1  0   RWD x1=1,x2=0,x3=0,x4=1,x5=1,x6=0
#> 2077  1  0  0  1  1  0   RWD x1=1,x2=0,x3=0,x4=1,x5=1,x6=0
#> 2078  1  0  0  1  1  0   RWD x1=1,x2=0,x3=0,x4=1,x5=1,x6=0
#> 2079  1  0  0  1  1  0   RWD x1=1,x2=0,x3=0,x4=1,x5=1,x6=0
#> 2080  1  0  0  1  1  0   RWD x1=1,x2=0,x3=0,x4=1,x5=1,x6=0
#> 2081  1  0  0  1  1  0   RWD x1=1,x2=0,x3=0,x4=1,x5=1,x6=0
#> 2082  1  0  0  1  1  0   RWD x1=1,x2=0,x3=0,x4=1,x5=1,x6=0
#> 2083  1  0  0  1  1  0   RWD x1=1,x2=0,x3=0,x4=1,x5=1,x6=0
#> 2084  1  0  0  1  1  0   RWD x1=1,x2=0,x3=0,x4=1,x5=1,x6=0
#> 2085  1  0  0  1  1  0   RWD x1=1,x2=0,x3=0,x4=1,x5=1,x6=0
#> 2086  1  0  0  1  1  0   RWD x1=1,x2=0,x3=0,x4=1,x5=1,x6=0
#> 2087  1  0  0  1  1  0   RWD x1=1,x2=0,x3=0,x4=1,x5=1,x6=0
#> 2088  1  0  0  1  1  0   RWD x1=1,x2=0,x3=0,x4=1,x5=1,x6=0
#> 2089  1  0  0  1  1  0   RWD x1=1,x2=0,x3=0,x4=1,x5=1,x6=0
#> 2090  1  0  0  1  1  0   RWD x1=1,x2=0,x3=0,x4=1,x5=1,x6=0
#> 2091  1  0  0  1  1  0   RWD x1=1,x2=0,x3=0,x4=1,x5=1,x6=0
#> 2092  1  0  0  1  1  0   RWD x1=1,x2=0,x3=0,x4=1,x5=1,x6=0
#> 2093  1  0  0  1  1  0   RWD x1=1,x2=0,x3=0,x4=1,x5=1,x6=0
#> 2094  1  0  1  0  0  0   RWD x1=1,x2=0,x3=1,x4=0,x5=0,x6=0
#> 2095  1  0  1  0  0  0   RWD x1=1,x2=0,x3=1,x4=0,x5=0,x6=0
#> 2096  1  0  1  0  0  0   RWD x1=1,x2=0,x3=1,x4=0,x5=0,x6=0
#> 2097  1  0  1  0  0  0   RWD x1=1,x2=0,x3=1,x4=0,x5=0,x6=0
#> 2098  1  0  1  0  0  0   RWD x1=1,x2=0,x3=1,x4=0,x5=0,x6=0
#> 2099  1  0  1  0  0  0   RWD x1=1,x2=0,x3=1,x4=0,x5=0,x6=0
#> 2100  1  0  1  0  0  0   RWD x1=1,x2=0,x3=1,x4=0,x5=0,x6=0
#> 2101  1  0  1  0  0  0   RWD x1=1,x2=0,x3=1,x4=0,x5=0,x6=0
#> 2102  1  0  1  0  0  1   RWD x1=1,x2=0,x3=1,x4=0,x5=0,x6=1
#> 2103  1  0  1  0  0  1   RWD x1=1,x2=0,x3=1,x4=0,x5=0,x6=1
#> 2104  1  0  1  0  0  1   RWD x1=1,x2=0,x3=1,x4=0,x5=0,x6=1
#> 2105  1  0  1  0  0  1   RWD x1=1,x2=0,x3=1,x4=0,x5=0,x6=1
#> 2106  1  0  1  0  0  1   RWD x1=1,x2=0,x3=1,x4=0,x5=0,x6=1
#> 2107  1  0  1  0  0  1   RWD x1=1,x2=0,x3=1,x4=0,x5=0,x6=1
#> 2108  1  0  1  0  0  1   RWD x1=1,x2=0,x3=1,x4=0,x5=0,x6=1
#> 2109  1  0  1  0  0  1   RWD x1=1,x2=0,x3=1,x4=0,x5=0,x6=1
#> 2110  1  0  1  0  0  1   RWD x1=1,x2=0,x3=1,x4=0,x5=0,x6=1
#> 2111  1  0  1  0  0  1   RWD x1=1,x2=0,x3=1,x4=0,x5=0,x6=1
#> 2112  1  0  1  0  0  1   RWD x1=1,x2=0,x3=1,x4=0,x5=0,x6=1
#> 2113  1  0  1  0  0  1   RWD x1=1,x2=0,x3=1,x4=0,x5=0,x6=1
#> 2114  1  0  1  0  0  1   RWD x1=1,x2=0,x3=1,x4=0,x5=0,x6=1
#> 2115  1  0  1  0  0  1   RWD x1=1,x2=0,x3=1,x4=0,x5=0,x6=1
#> 2116  1  0  1  0  0  1   RWD x1=1,x2=0,x3=1,x4=0,x5=0,x6=1
#> 2117  1  0  1  0  0  1   RWD x1=1,x2=0,x3=1,x4=0,x5=0,x6=1
#> 2118  1  0  1  0  0  1   RWD x1=1,x2=0,x3=1,x4=0,x5=0,x6=1
#> 2119  1  0  1  0  0  1   RWD x1=1,x2=0,x3=1,x4=0,x5=0,x6=1
#> 2120  1  0  1  0  0  1   RWD x1=1,x2=0,x3=1,x4=0,x5=0,x6=1
#> 2121  1  0  1  0  0  1   RWD x1=1,x2=0,x3=1,x4=0,x5=0,x6=1
#> 2122  1  0  1  0  0  1   RWD x1=1,x2=0,x3=1,x4=0,x5=0,x6=1
#> 2123  1  0  1  0  0  1   RWD x1=1,x2=0,x3=1,x4=0,x5=0,x6=1
#> 2124  1  0  1  0  0  1   RWD x1=1,x2=0,x3=1,x4=0,x5=0,x6=1
#> 2125  1  0  1  0  0  1   RWD x1=1,x2=0,x3=1,x4=0,x5=0,x6=1
#> 2126  1  0  1  0  1  0   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=0
#> 2127  1  0  1  0  1  0   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=0
#> 2128  1  0  1  0  1  0   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=0
#> 2129  1  0  1  0  1  0   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=0
#> 2130  1  0  1  0  1  0   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=0
#> 2131  1  0  1  0  1  0   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=0
#> 2132  1  0  1  0  1  0   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=0
#> 2133  1  0  1  0  1  0   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=0
#> 2134  1  0  1  0  1  0   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=0
#> 2135  1  0  1  0  1  0   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=0
#> 2136  1  0  1  0  1  0   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=0
#> 2137  1  0  1  0  1  0   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=0
#> 2138  1  0  1  0  1  0   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=0
#> 2139  1  0  1  0  1  0   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=0
#> 2140  1  0  1  0  1  0   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=0
#> 2141  1  0  1  0  1  0   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=0
#> 2142  1  0  1  0  1  0   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=0
#> 2143  1  0  1  0  1  0   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=0
#> 2144  1  0  1  0  1  0   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=0
#> 2145  1  0  1  0  1  0   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=0
#> 2146  1  0  1  0  1  0   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=0
#> 2147  1  0  1  0  1  0   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=0
#> 2148  1  0  1  0  1  0   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=0
#> 2149  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2150  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2151  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2152  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2153  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2154  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2155  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2156  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2157  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2158  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2159  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2160  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2161  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2162  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2163  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2164  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2165  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2166  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2167  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2168  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2169  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2170  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2171  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2172  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2173  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2174  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2175  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2176  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2177  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2178  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2179  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2180  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2181  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2182  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2183  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2184  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2185  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2186  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2187  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2188  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2189  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2190  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2191  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2192  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2193  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2194  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2195  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2196  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2197  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2198  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2199  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2200  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2201  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2202  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2203  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2204  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2205  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2206  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2207  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2208  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2209  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2210  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2211  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2212  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2213  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2214  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2215  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2216  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2217  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2218  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2219  1  0  1  0  1  1   RWD x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 2220  1  0  1  1  0  0   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 2221  1  0  1  1  0  0   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 2222  1  0  1  1  0  0   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 2223  1  0  1  1  0  0   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 2224  1  0  1  1  0  0   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 2225  1  0  1  1  0  0   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 2226  1  0  1  1  0  0   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 2227  1  0  1  1  0  0   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 2228  1  0  1  1  0  0   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 2229  1  0  1  1  0  0   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 2230  1  0  1  1  0  0   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 2231  1  0  1  1  0  0   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 2232  1  0  1  1  0  0   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 2233  1  0  1  1  0  0   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 2234  1  0  1  1  0  0   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 2235  1  0  1  1  0  0   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 2236  1  0  1  1  0  0   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 2237  1  0  1  1  0  0   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 2238  1  0  1  1  0  0   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 2239  1  0  1  1  0  0   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 2240  1  0  1  1  0  0   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 2241  1  0  1  1  0  0   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 2242  1  0  1  1  0  0   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 2243  1  0  1  1  0  0   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 2244  1  0  1  1  0  0   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 2245  1  0  1  1  0  0   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 2246  1  0  1  1  0  0   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 2247  1  0  1  1  0  0   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 2248  1  0  1  1  0  0   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 2249  1  0  1  1  0  0   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 2250  1  0  1  1  0  0   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 2251  1  0  1  1  0  0   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 2252  1  0  1  1  0  0   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 2253  1  0  1  1  0  0   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 2254  1  0  1  1  0  0   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 2255  1  0  1  1  0  0   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 2256  1  0  1  1  0  0   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 2257  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2258  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2259  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2260  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2261  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2262  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2263  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2264  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2265  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2266  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2267  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2268  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2269  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2270  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2271  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2272  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2273  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2274  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2275  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2276  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2277  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2278  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2279  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2280  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2281  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2282  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2283  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2284  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2285  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2286  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2287  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2288  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2289  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2290  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2291  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2292  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2293  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2294  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2295  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2296  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2297  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2298  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2299  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2300  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2301  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2302  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2303  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2304  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2305  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2306  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2307  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2308  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2309  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2310  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2311  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2312  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2313  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2314  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2315  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2316  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2317  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2318  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2319  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2320  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2321  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2322  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2323  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2324  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2325  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2326  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2327  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2328  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2329  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2330  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2331  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2332  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2333  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2334  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2335  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2336  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2337  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2338  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2339  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2340  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2341  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2342  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2343  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2344  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2345  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2346  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2347  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2348  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2349  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2350  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2351  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2352  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2353  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2354  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2355  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2356  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2357  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2358  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2359  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2360  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2361  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2362  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2363  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2364  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2365  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2366  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2367  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2368  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2369  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2370  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2371  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2372  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2373  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2374  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2375  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2376  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2377  1  0  1  1  0  1   RWD x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 2378  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2379  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2380  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2381  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2382  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2383  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2384  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2385  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2386  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2387  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2388  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2389  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2390  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2391  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2392  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2393  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2394  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2395  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2396  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2397  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2398  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2399  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2400  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2401  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2402  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2403  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2404  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2405  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2406  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2407  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2408  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2409  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2410  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2411  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2412  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2413  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2414  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2415  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2416  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2417  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2418  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2419  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2420  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2421  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2422  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2423  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2424  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2425  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2426  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2427  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2428  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2429  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2430  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2431  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2432  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2433  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2434  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2435  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2436  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2437  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2438  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2439  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2440  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2441  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2442  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2443  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2444  1  0  1  1  1  0   RWD x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2445  1  1  0  1  0  1   RWD x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 2446  1  1  0  1  0  1   RWD x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 2447  1  1  0  1  0  1   RWD x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 2448  1  1  0  1  0  1   RWD x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 2449  1  1  0  1  0  1   RWD x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 2450  1  1  0  1  0  1   RWD x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 2451  1  1  0  1  0  1   RWD x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 2452  1  1  0  1  0  1   RWD x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 2453  1  1  0  1  0  1   RWD x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 2454  1  1  0  1  1  0   RWD x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 2455  1  1  0  1  1  0   RWD x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 2456  1  1  0  1  1  0   RWD x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 2457  1  1  0  1  1  0   RWD x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 2458  1  1  0  1  1  0   RWD x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 2459  1  1  0  1  1  0   RWD x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 2460  1  1  0  1  1  1   RWD x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 2461  1  1  0  1  1  1   RWD x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 2462  1  1  0  1  1  1   RWD x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 2463  1  1  0  1  1  1   RWD x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 2464  1  1  0  1  1  1   RWD x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 2465  1  1  0  1  1  1   RWD x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 2466  1  1  0  1  1  1   RWD x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 2467  1  1  0  1  1  1   RWD x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 2468  1  1  0  1  1  1   RWD x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 2469  1  1  0  1  1  1   RWD x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 2470  1  1  0  1  1  1   RWD x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 2471  1  1  0  1  1  1   RWD x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 2472  1  1  0  1  1  1   RWD x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 2473  1  1  0  1  1  1   RWD x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 2474  1  1  0  1  1  1   RWD x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 2475  1  1  0  1  1  1   RWD x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 2476  1  1  0  1  1  1   RWD x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 2477  1  1  0  1  1  1   RWD x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 2478  1  1  0  1  1  1   RWD x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 2479  1  1  0  1  1  1   RWD x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 2480  1  1  0  1  1  1   RWD x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 2481  1  1  0  1  1  1   RWD x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 2482  1  1  0  1  1  1   RWD x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 2483  1  1  0  1  1  1   RWD x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 2484  1  1  1  0  0  0   RWD x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 2485  1  1  1  0  0  0   RWD x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 2486  1  1  1  0  0  1   RWD x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 2487  1  1  1  0  0  1   RWD x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 2488  1  1  1  0  0  1   RWD x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 2489  1  1  1  0  0  1   RWD x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 2490  1  1  1  0  0  1   RWD x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 2491  1  1  1  0  1  0   RWD x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 2492  1  1  1  0  1  0   RWD x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 2493  1  1  1  0  1  1   RWD x1=1,x2=1,x3=1,x4=0,x5=1,x6=1
#> 2494  1  1  1  0  1  1   RWD x1=1,x2=1,x3=1,x4=0,x5=1,x6=1
#> 2495  1  1  1  0  1  1   RWD x1=1,x2=1,x3=1,x4=0,x5=1,x6=1
#> 2496  1  1  1  0  1  1   RWD x1=1,x2=1,x3=1,x4=0,x5=1,x6=1
#> 2497  1  1  1  0  1  1   RWD x1=1,x2=1,x3=1,x4=0,x5=1,x6=1
#> 2498  1  1  1  0  1  1   RWD x1=1,x2=1,x3=1,x4=0,x5=1,x6=1
#> 2499  1  1  1  0  1  1   RWD x1=1,x2=1,x3=1,x4=0,x5=1,x6=1
#> 2500  1  1  1  0  1  1   RWD x1=1,x2=1,x3=1,x4=0,x5=1,x6=1
#> 2501  1  1  1  0  1  1   RWD x1=1,x2=1,x3=1,x4=0,x5=1,x6=1
#> 2502  1  1  1  0  1  1   RWD x1=1,x2=1,x3=1,x4=0,x5=1,x6=1
#> 2503  1  1  1  0  1  1   RWD x1=1,x2=1,x3=1,x4=0,x5=1,x6=1
#> 2504  1  1  1  0  1  1   RWD x1=1,x2=1,x3=1,x4=0,x5=1,x6=1
#> 2505  1  1  1  0  1  1   RWD x1=1,x2=1,x3=1,x4=0,x5=1,x6=1
#> 2506  1  1  1  1  0  0   RWD x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 2507  1  1  1  1  0  0   RWD x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 2508  1  1  1  1  0  0   RWD x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 2509  1  1  1  1  0  0   RWD x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 2510  1  1  1  1  0  0   RWD x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 2511  1  1  1  1  0  0   RWD x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 2512  1  1  1  1  0  1   RWD x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 2513  1  1  1  1  0  1   RWD x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 2514  1  1  1  1  0  1   RWD x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 2515  1  1  1  1  0  1   RWD x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 2516  1  1  1  1  0  1   RWD x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 2517  1  1  1  1  0  1   RWD x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 2518  1  1  1  1  0  1   RWD x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 2519  1  1  1  1  0  1   RWD x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 2520  1  1  1  1  0  1   RWD x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 2521  1  1  1  1  0  1   RWD x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 2522  1  1  1  1  0  1   RWD x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 2523  1  1  1  1  0  1   RWD x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 2524  1  1  1  1  0  1   RWD x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 2525  1  1  1  1  0  1   RWD x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 2526  1  1  1  1  0  1   RWD x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 2527  1  1  1  1  0  1   RWD x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 2528  1  1  1  1  0  1   RWD x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 2529  1  1  1  1  0  1   RWD x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 2530  1  1  1  1  0  1   RWD x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 2531  1  1  1  1  0  1   RWD x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 2532  1  1  1  1  0  1   RWD x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 2533  1  1  1  1  0  1   RWD x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 2534  1  1  1  1  0  1   RWD x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 2535  1  1  1  1  0  1   RWD x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 2536  1  1  1  1  0  1   RWD x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 2537  1  1  1  1  0  1   RWD x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 2538  1  1  1  1  1  0   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=0
#> 2539  1  1  1  1  1  0   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=0
#> 2540  1  1  1  1  1  0   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=0
#> 2541  1  1  1  1  1  0   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=0
#> 2542  1  1  1  1  1  0   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=0
#> 2543  1  1  1  1  1  0   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=0
#> 2544  1  1  1  1  1  0   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=0
#> 2545  1  1  1  1  1  0   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=0
#> 2546  1  1  1  1  1  0   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=0
#> 2547  1  1  1  1  1  0   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=0
#> 2548  1  1  1  1  1  0   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=0
#> 2549  1  1  1  1  1  0   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=0
#> 2550  1  1  1  1  1  0   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=0
#> 2551  1  1  1  1  1  0   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=0
#> 2552  1  1  1  1  1  0   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=0
#> 2553  1  1  1  1  1  0   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=0
#> 2554  1  1  1  1  1  0   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=0
#> 2555  1  1  1  1  1  0   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=0
#> 2556  1  1  1  1  1  0   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=0
#> 2557  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2558  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2559  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2560  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2561  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2562  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2563  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2564  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2565  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2566  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2567  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2568  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2569  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2570  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2571  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2572  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2573  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2574  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2575  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2576  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2577  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2578  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2579  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2580  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2581  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2582  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2583  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2584  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2585  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2586  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2587  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2588  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2589  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2590  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2591  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2592  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2593  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2594  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2595  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2596  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2597  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2598  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2599  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2600  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2601  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2602  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2603  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2604  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2605  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2606  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2607  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2608  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2609  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2610  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2611  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2612  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2613  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2614  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2615  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2616  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2617  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2618  1  1  1  1  1  1   RWD x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 2619  0  0  0  0  0  0   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=0
#> 2620  0  0  0  0  0  0   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=0
#> 2621  0  0  0  0  0  0   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=0
#> 2622  0  0  0  0  0  0   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=0
#> 2623  0  0  0  0  0  0   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=0
#> 2624  0  0  0  0  0  0   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=0
#> 2625  0  0  0  0  0  0   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=0
#> 2626  0  0  0  0  0  0   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=0
#> 2627  0  0  0  0  0  0   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=0
#> 2628  0  0  0  0  0  0   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=0
#> 2629  0  0  0  0  0  0   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=0
#> 2630  0  0  0  0  0  0   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=0
#> 2631  0  0  0  0  0  0   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=0
#> 2632  0  0  0  0  0  0   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=0
#> 2633  0  0  0  0  0  0   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=0
#> 2634  0  0  0  0  0  0   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=0
#> 2635  0  0  0  0  0  0   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=0
#> 2636  0  0  0  0  0  0   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=0
#> 2637  0  0  0  0  0  0   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=0
#> 2638  0  0  0  0  0  0   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=0
#> 2639  0  0  0  0  0  0   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=0
#> 2640  0  0  0  0  0  0   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=0
#> 2641  0  0  0  0  0  0   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=0
#> 2642  0  0  0  0  0  0   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=0
#> 2643  0  0  0  0  0  0   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=0
#> 2644  0  0  0  0  0  0   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=0
#> 2645  0  0  0  0  0  0   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=0
#> 2646  0  0  0  0  0  0   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=0
#> 2647  0  0  0  0  0  0   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=0
#> 2648  0  0  0  0  0  0   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=0
#> 2649  0  0  0  0  0  0   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=0
#> 2650  0  0  0  0  0  0   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=0
#> 2651  0  0  0  0  0  0   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=0
#> 2652  0  0  0  0  0  0   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=0
#> 2653  0  0  0  0  0  0   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=0
#> 2654  0  0  0  0  0  0   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=0
#> 2655  0  0  0  0  0  0   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=0
#> 2656  0  0  0  0  0  1   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 2657  0  0  0  0  0  1   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 2658  0  0  0  0  0  1   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 2659  0  0  0  0  0  1   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 2660  0  0  0  0  0  1   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 2661  0  0  0  0  0  1   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 2662  0  0  0  0  0  1   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 2663  0  0  0  0  0  1   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 2664  0  0  0  0  0  1   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 2665  0  0  0  0  0  1   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 2666  0  0  0  0  0  1   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 2667  0  0  0  0  0  1   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 2668  0  0  0  0  0  1   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 2669  0  0  0  0  0  1   RCT x1=0,x2=0,x3=0,x4=0,x5=0,x6=1
#> 2670  0  0  0  0  1  0   RCT x1=0,x2=0,x3=0,x4=0,x5=1,x6=0
#> 2671  0  0  0  0  1  0   RCT x1=0,x2=0,x3=0,x4=0,x5=1,x6=0
#> 2672  0  0  0  0  1  0   RCT x1=0,x2=0,x3=0,x4=0,x5=1,x6=0
#> 2673  0  0  0  0  1  0   RCT x1=0,x2=0,x3=0,x4=0,x5=1,x6=0
#> 2674  0  0  0  0  1  0   RCT x1=0,x2=0,x3=0,x4=0,x5=1,x6=0
#> 2675  0  0  0  0  1  0   RCT x1=0,x2=0,x3=0,x4=0,x5=1,x6=0
#> 2676  0  0  0  0  1  0   RCT x1=0,x2=0,x3=0,x4=0,x5=1,x6=0
#> 2677  0  0  0  0  1  0   RCT x1=0,x2=0,x3=0,x4=0,x5=1,x6=0
#> 2678  0  0  0  0  1  0   RCT x1=0,x2=0,x3=0,x4=0,x5=1,x6=0
#> 2679  0  0  0  0  1  0   RCT x1=0,x2=0,x3=0,x4=0,x5=1,x6=0
#> 2680  0  0  0  0  1  1   RCT x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 2681  0  0  0  0  1  1   RCT x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 2682  0  0  0  0  1  1   RCT x1=0,x2=0,x3=0,x4=0,x5=1,x6=1
#> 2683  0  0  0  1  0  0   RCT x1=0,x2=0,x3=0,x4=1,x5=0,x6=0
#> 2684  0  0  0  1  0  0   RCT x1=0,x2=0,x3=0,x4=1,x5=0,x6=0
#> 2685  0  0  0  1  0  0   RCT x1=0,x2=0,x3=0,x4=1,x5=0,x6=0
#> 2686  0  0  0  1  0  0   RCT x1=0,x2=0,x3=0,x4=1,x5=0,x6=0
#> 2687  0  0  0  1  0  0   RCT x1=0,x2=0,x3=0,x4=1,x5=0,x6=0
#> 2688  0  0  0  1  0  0   RCT x1=0,x2=0,x3=0,x4=1,x5=0,x6=0
#> 2689  0  0  0  1  0  0   RCT x1=0,x2=0,x3=0,x4=1,x5=0,x6=0
#> 2690  0  0  0  1  0  0   RCT x1=0,x2=0,x3=0,x4=1,x5=0,x6=0
#> 2691  0  0  0  1  0  0   RCT x1=0,x2=0,x3=0,x4=1,x5=0,x6=0
#> 2692  0  0  0  1  0  0   RCT x1=0,x2=0,x3=0,x4=1,x5=0,x6=0
#> 2693  0  0  0  1  0  0   RCT x1=0,x2=0,x3=0,x4=1,x5=0,x6=0
#> 2694  0  0  0  1  0  0   RCT x1=0,x2=0,x3=0,x4=1,x5=0,x6=0
#> 2695  0  0  0  1  0  0   RCT x1=0,x2=0,x3=0,x4=1,x5=0,x6=0
#> 2696  0  0  0  1  0  0   RCT x1=0,x2=0,x3=0,x4=1,x5=0,x6=0
#> 2697  0  0  0  1  0  0   RCT x1=0,x2=0,x3=0,x4=1,x5=0,x6=0
#> 2698  0  0  0  1  0  1   RCT x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 2699  0  0  0  1  0  1   RCT x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 2700  0  0  0  1  0  1   RCT x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 2701  0  0  0  1  0  1   RCT x1=0,x2=0,x3=0,x4=1,x5=0,x6=1
#> 2702  0  0  0  1  1  1   RCT x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 2703  0  0  0  1  1  1   RCT x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 2704  0  0  0  1  1  1   RCT x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 2705  0  0  0  1  1  1   RCT x1=0,x2=0,x3=0,x4=1,x5=1,x6=1
#> 2706  0  0  1  0  0  0   RCT x1=0,x2=0,x3=1,x4=0,x5=0,x6=0
#> 2707  0  0  1  0  0  0   RCT x1=0,x2=0,x3=1,x4=0,x5=0,x6=0
#> 2708  0  0  1  0  0  0   RCT x1=0,x2=0,x3=1,x4=0,x5=0,x6=0
#> 2709  0  0  1  0  0  0   RCT x1=0,x2=0,x3=1,x4=0,x5=0,x6=0
#> 2710  0  0  1  0  0  0   RCT x1=0,x2=0,x3=1,x4=0,x5=0,x6=0
#> 2711  0  0  1  0  0  0   RCT x1=0,x2=0,x3=1,x4=0,x5=0,x6=0
#> 2712  0  0  1  0  0  0   RCT x1=0,x2=0,x3=1,x4=0,x5=0,x6=0
#> 2713  0  0  1  0  0  0   RCT x1=0,x2=0,x3=1,x4=0,x5=0,x6=0
#> 2714  0  0  1  0  0  0   RCT x1=0,x2=0,x3=1,x4=0,x5=0,x6=0
#> 2715  0  0  1  0  0  0   RCT x1=0,x2=0,x3=1,x4=0,x5=0,x6=0
#> 2716  0  0  1  0  0  0   RCT x1=0,x2=0,x3=1,x4=0,x5=0,x6=0
#> 2717  0  0  1  0  0  0   RCT x1=0,x2=0,x3=1,x4=0,x5=0,x6=0
#> 2718  0  0  1  0  0  0   RCT x1=0,x2=0,x3=1,x4=0,x5=0,x6=0
#> 2719  0  0  1  0  0  0   RCT x1=0,x2=0,x3=1,x4=0,x5=0,x6=0
#> 2720  0  0  1  0  0  0   RCT x1=0,x2=0,x3=1,x4=0,x5=0,x6=0
#> 2721  0  0  1  0  0  0   RCT x1=0,x2=0,x3=1,x4=0,x5=0,x6=0
#> 2722  0  0  1  0  0  0   RCT x1=0,x2=0,x3=1,x4=0,x5=0,x6=0
#> 2723  0  0  1  0  0  0   RCT x1=0,x2=0,x3=1,x4=0,x5=0,x6=0
#> 2724  0  0  1  0  0  1   RCT x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 2725  0  0  1  0  0  1   RCT x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 2726  0  0  1  0  0  1   RCT x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 2727  0  0  1  0  0  1   RCT x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 2728  0  0  1  0  0  1   RCT x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 2729  0  0  1  0  0  1   RCT x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 2730  0  0  1  0  0  1   RCT x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 2731  0  0  1  0  0  1   RCT x1=0,x2=0,x3=1,x4=0,x5=0,x6=1
#> 2732  0  0  1  0  1  0   RCT x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 2733  0  0  1  0  1  0   RCT x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 2734  0  0  1  0  1  0   RCT x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 2735  0  0  1  0  1  0   RCT x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 2736  0  0  1  0  1  0   RCT x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 2737  0  0  1  0  1  0   RCT x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 2738  0  0  1  0  1  0   RCT x1=0,x2=0,x3=1,x4=0,x5=1,x6=0
#> 2739  0  0  1  1  0  0   RCT x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 2740  0  0  1  1  0  0   RCT x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 2741  0  0  1  1  0  0   RCT x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 2742  0  0  1  1  0  0   RCT x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 2743  0  0  1  1  0  0   RCT x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 2744  0  0  1  1  0  0   RCT x1=0,x2=0,x3=1,x4=1,x5=0,x6=0
#> 2745  0  0  1  1  1  0   RCT x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2746  0  0  1  1  1  0   RCT x1=0,x2=0,x3=1,x4=1,x5=1,x6=0
#> 2747  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2748  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2749  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2750  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2751  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2752  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2753  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2754  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2755  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2756  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2757  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2758  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2759  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2760  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2761  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2762  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2763  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2764  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2765  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2766  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2767  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2768  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2769  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2770  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2771  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2772  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2773  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2774  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2775  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2776  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2777  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2778  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2779  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2780  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2781  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2782  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2783  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2784  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2785  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2786  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2787  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2788  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2789  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2790  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2791  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2792  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2793  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2794  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2795  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2796  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2797  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2798  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2799  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2800  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2801  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2802  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2803  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2804  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2805  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2806  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2807  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2808  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2809  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2810  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2811  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2812  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2813  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2814  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2815  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2816  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2817  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2818  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2819  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2820  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2821  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2822  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2823  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2824  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2825  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2826  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2827  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2828  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2829  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2830  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2831  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2832  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2833  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2834  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2835  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2836  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2837  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2838  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2839  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2840  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2841  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2842  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2843  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2844  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2845  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2846  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2847  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2848  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2849  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2850  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2851  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2852  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2853  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2854  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2855  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2856  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2857  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2858  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2859  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2860  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2861  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2862  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2863  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2864  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2865  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2866  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2867  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2868  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2869  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2870  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2871  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2872  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2873  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2874  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2875  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2876  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2877  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2878  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2879  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2880  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2881  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2882  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2883  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2884  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2885  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2886  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2887  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2888  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2889  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2890  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2891  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2892  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2893  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2894  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2895  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2896  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2897  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2898  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2899  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2900  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2901  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2902  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2903  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2904  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2905  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2906  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2907  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2908  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2909  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2910  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2911  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2912  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2913  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2914  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2915  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2916  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2917  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2918  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2919  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2920  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2921  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2922  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2923  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2924  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2925  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2926  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2927  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2928  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2929  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2930  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2931  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2932  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2933  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2934  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2935  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2936  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2937  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2938  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2939  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2940  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2941  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2942  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2943  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2944  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2945  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2946  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2947  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2948  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2949  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2950  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2951  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2952  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2953  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2954  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2955  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2956  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2957  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2958  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2959  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2960  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2961  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2962  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2963  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2964  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2965  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2966  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2967  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2968  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2969  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2970  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2971  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2972  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2973  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2974  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2975  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2976  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2977  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2978  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2979  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2980  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2981  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2982  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2983  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2984  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2985  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2986  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2987  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2988  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2989  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2990  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2991  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2992  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2993  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2994  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2995  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2996  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2997  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2998  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 2999  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3000  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3001  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3002  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3003  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3004  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3005  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3006  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3007  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3008  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3009  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3010  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3011  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3012  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3013  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3014  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3015  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3016  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3017  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3018  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3019  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3020  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3021  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3022  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3023  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3024  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3025  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3026  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3027  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3028  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3029  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3030  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3031  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3032  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3033  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3034  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3035  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3036  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3037  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3038  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3039  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3040  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3041  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3042  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3043  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3044  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3045  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3046  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3047  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3048  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3049  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3050  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3051  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3052  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3053  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3054  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3055  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3056  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3057  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3058  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3059  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3060  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3061  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3062  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3063  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3064  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3065  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3066  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3067  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3068  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3069  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3070  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3071  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3072  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3073  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3074  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3075  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3076  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3077  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3078  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3079  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3080  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3081  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3082  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3083  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3084  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3085  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3086  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3087  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3088  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3089  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3090  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3091  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3092  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3093  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3094  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3095  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3096  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3097  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3098  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3099  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3100  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3101  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3102  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3103  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3104  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3105  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3106  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3107  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3108  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3109  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3110  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3111  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3112  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3113  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3114  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3115  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3116  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3117  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3118  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3119  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3120  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3121  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3122  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3123  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3124  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3125  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3126  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3127  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3128  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3129  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3130  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3131  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3132  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3133  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3134  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3135  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3136  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3137  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3138  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3139  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3140  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3141  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3142  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3143  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3144  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3145  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3146  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3147  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3148  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3149  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3150  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3151  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3152  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3153  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3154  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3155  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3156  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3157  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3158  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3159  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3160  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3161  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3162  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3163  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3164  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3165  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3166  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3167  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3168  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3169  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3170  0  1  0  0  0  0   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=0
#> 3171  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3172  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3173  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3174  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3175  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3176  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3177  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3178  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3179  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3180  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3181  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3182  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3183  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3184  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3185  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3186  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3187  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3188  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3189  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3190  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3191  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3192  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3193  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3194  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3195  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3196  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3197  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3198  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3199  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3200  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3201  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3202  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3203  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3204  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3205  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3206  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3207  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3208  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3209  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3210  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3211  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3212  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3213  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3214  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3215  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3216  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3217  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3218  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3219  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3220  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3221  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3222  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3223  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3224  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3225  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3226  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3227  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3228  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3229  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3230  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3231  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3232  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3233  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3234  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3235  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3236  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3237  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3238  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3239  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3240  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3241  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3242  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3243  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3244  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3245  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3246  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3247  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3248  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3249  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3250  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3251  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3252  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3253  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3254  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3255  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3256  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3257  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3258  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3259  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3260  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3261  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3262  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3263  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3264  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3265  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3266  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3267  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3268  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3269  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3270  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3271  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3272  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3273  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3274  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3275  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3276  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3277  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3278  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3279  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3280  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3281  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3282  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3283  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3284  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3285  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3286  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3287  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3288  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3289  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3290  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3291  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3292  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3293  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3294  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3295  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3296  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3297  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3298  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3299  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3300  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3301  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3302  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3303  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3304  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3305  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3306  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3307  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3308  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3309  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3310  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3311  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3312  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3313  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3314  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3315  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3316  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3317  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3318  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3319  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3320  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3321  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3322  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3323  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3324  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3325  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3326  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3327  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3328  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3329  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3330  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3331  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3332  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3333  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3334  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3335  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3336  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3337  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3338  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3339  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3340  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3341  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3342  0  1  0  0  0  1   RCT x1=0,x2=1,x3=0,x4=0,x5=0,x6=1
#> 3343  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3344  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3345  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3346  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3347  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3348  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3349  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3350  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3351  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3352  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3353  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3354  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3355  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3356  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3357  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3358  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3359  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3360  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3361  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3362  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3363  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3364  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3365  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3366  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3367  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3368  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3369  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3370  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3371  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3372  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3373  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3374  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3375  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3376  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3377  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3378  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3379  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3380  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3381  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3382  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3383  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3384  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3385  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3386  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3387  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3388  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3389  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3390  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3391  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3392  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3393  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3394  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3395  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3396  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3397  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3398  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3399  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3400  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3401  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3402  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3403  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3404  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3405  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3406  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3407  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3408  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3409  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3410  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3411  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3412  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3413  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3414  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3415  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3416  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3417  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3418  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3419  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3420  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3421  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3422  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3423  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3424  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3425  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3426  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3427  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3428  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3429  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3430  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3431  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3432  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3433  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3434  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3435  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3436  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3437  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3438  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3439  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3440  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3441  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3442  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3443  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3444  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3445  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3446  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3447  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3448  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3449  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3450  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3451  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3452  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3453  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3454  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3455  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3456  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3457  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3458  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3459  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3460  0  1  0  0  1  0   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=0
#> 3461  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3462  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3463  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3464  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3465  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3466  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3467  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3468  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3469  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3470  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3471  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3472  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3473  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3474  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3475  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3476  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3477  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3478  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3479  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3480  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3481  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3482  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3483  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3484  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3485  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3486  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3487  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3488  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3489  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3490  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3491  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3492  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3493  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3494  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3495  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3496  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3497  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3498  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3499  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3500  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3501  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3502  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3503  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3504  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3505  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3506  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3507  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3508  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3509  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3510  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3511  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3512  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3513  0  1  0  0  1  1   RCT x1=0,x2=1,x3=0,x4=0,x5=1,x6=1
#> 3514  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3515  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3516  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3517  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3518  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3519  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3520  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3521  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3522  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3523  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3524  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3525  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3526  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3527  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3528  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3529  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3530  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3531  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3532  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3533  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3534  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3535  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3536  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3537  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3538  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3539  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3540  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3541  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3542  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3543  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3544  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3545  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3546  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3547  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3548  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3549  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3550  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3551  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3552  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3553  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3554  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3555  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3556  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3557  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3558  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3559  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3560  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3561  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3562  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3563  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3564  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3565  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3566  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3567  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3568  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3569  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3570  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3571  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3572  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3573  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3574  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3575  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3576  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3577  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3578  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3579  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3580  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3581  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3582  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3583  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3584  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3585  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3586  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3587  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3588  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3589  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3590  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3591  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3592  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3593  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3594  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3595  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3596  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3597  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3598  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3599  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3600  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3601  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3602  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3603  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3604  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3605  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3606  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3607  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3608  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3609  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3610  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3611  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3612  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3613  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3614  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3615  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3616  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3617  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3618  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3619  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3620  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3621  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3622  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3623  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3624  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3625  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3626  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3627  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3628  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3629  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3630  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3631  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3632  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3633  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3634  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3635  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3636  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3637  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3638  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3639  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3640  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3641  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3642  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3643  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3644  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3645  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3646  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3647  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3648  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3649  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3650  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3651  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3652  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3653  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3654  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3655  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3656  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3657  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3658  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3659  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3660  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3661  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3662  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3663  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3664  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3665  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3666  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3667  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3668  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3669  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3670  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3671  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3672  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3673  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3674  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3675  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3676  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3677  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3678  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3679  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3680  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3681  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3682  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3683  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3684  0  1  0  1  0  0   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=0
#> 3685  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3686  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3687  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3688  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3689  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3690  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3691  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3692  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3693  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3694  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3695  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3696  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3697  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3698  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3699  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3700  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3701  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3702  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3703  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3704  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3705  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3706  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3707  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3708  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3709  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3710  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3711  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3712  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3713  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3714  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3715  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3716  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3717  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3718  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3719  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3720  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3721  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3722  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3723  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3724  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3725  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3726  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3727  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3728  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3729  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3730  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3731  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3732  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3733  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3734  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3735  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3736  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3737  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3738  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3739  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3740  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3741  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3742  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3743  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3744  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3745  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3746  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3747  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3748  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3749  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3750  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3751  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3752  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3753  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3754  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3755  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3756  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3757  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3758  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3759  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3760  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3761  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3762  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3763  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3764  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3765  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3766  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3767  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3768  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3769  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3770  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3771  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3772  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3773  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3774  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3775  0  1  0  1  0  1   RCT x1=0,x2=1,x3=0,x4=1,x5=0,x6=1
#> 3776  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3777  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3778  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3779  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3780  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3781  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3782  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3783  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3784  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3785  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3786  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3787  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3788  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3789  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3790  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3791  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3792  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3793  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3794  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3795  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3796  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3797  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3798  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3799  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3800  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3801  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3802  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3803  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3804  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3805  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3806  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3807  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3808  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3809  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3810  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3811  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3812  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3813  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3814  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3815  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3816  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3817  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3818  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3819  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3820  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3821  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3822  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3823  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3824  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3825  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3826  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3827  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3828  0  1  0  1  1  0   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=0
#> 3829  0  1  0  1  1  1   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 3830  0  1  0  1  1  1   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 3831  0  1  0  1  1  1   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 3832  0  1  0  1  1  1   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 3833  0  1  0  1  1  1   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 3834  0  1  0  1  1  1   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 3835  0  1  0  1  1  1   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 3836  0  1  0  1  1  1   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 3837  0  1  0  1  1  1   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 3838  0  1  0  1  1  1   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 3839  0  1  0  1  1  1   RCT x1=0,x2=1,x3=0,x4=1,x5=1,x6=1
#> 3840  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3841  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3842  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3843  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3844  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3845  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3846  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3847  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3848  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3849  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3850  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3851  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3852  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3853  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3854  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3855  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3856  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3857  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3858  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3859  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3860  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3861  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3862  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3863  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3864  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3865  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3866  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3867  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3868  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3869  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3870  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3871  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3872  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3873  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3874  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3875  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3876  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3877  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3878  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3879  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3880  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3881  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3882  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3883  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3884  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3885  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3886  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3887  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3888  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3889  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3890  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3891  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3892  0  1  1  0  0  1   RCT x1=0,x2=1,x3=1,x4=0,x5=0,x6=1
#> 3893  0  1  1  0  1  0   RCT x1=0,x2=1,x3=1,x4=0,x5=1,x6=0
#> 3894  0  1  1  0  1  0   RCT x1=0,x2=1,x3=1,x4=0,x5=1,x6=0
#> 3895  0  1  1  0  1  0   RCT x1=0,x2=1,x3=1,x4=0,x5=1,x6=0
#> 3896  0  1  1  0  1  0   RCT x1=0,x2=1,x3=1,x4=0,x5=1,x6=0
#> 3897  0  1  1  0  1  0   RCT x1=0,x2=1,x3=1,x4=0,x5=1,x6=0
#> 3898  0  1  1  0  1  0   RCT x1=0,x2=1,x3=1,x4=0,x5=1,x6=0
#> 3899  0  1  1  0  1  0   RCT x1=0,x2=1,x3=1,x4=0,x5=1,x6=0
#> 3900  0  1  1  0  1  0   RCT x1=0,x2=1,x3=1,x4=0,x5=1,x6=0
#> 3901  0  1  1  0  1  0   RCT x1=0,x2=1,x3=1,x4=0,x5=1,x6=0
#> 3902  0  1  1  0  1  0   RCT x1=0,x2=1,x3=1,x4=0,x5=1,x6=0
#> 3903  0  1  1  0  1  0   RCT x1=0,x2=1,x3=1,x4=0,x5=1,x6=0
#> 3904  0  1  1  0  1  0   RCT x1=0,x2=1,x3=1,x4=0,x5=1,x6=0
#> 3905  0  1  1  0  1  0   RCT x1=0,x2=1,x3=1,x4=0,x5=1,x6=0
#> 3906  0  1  1  0  1  0   RCT x1=0,x2=1,x3=1,x4=0,x5=1,x6=0
#> 3907  0  1  1  0  1  0   RCT x1=0,x2=1,x3=1,x4=0,x5=1,x6=0
#> 3908  0  1  1  0  1  0   RCT x1=0,x2=1,x3=1,x4=0,x5=1,x6=0
#> 3909  0  1  1  0  1  0   RCT x1=0,x2=1,x3=1,x4=0,x5=1,x6=0
#> 3910  0  1  1  0  1  0   RCT x1=0,x2=1,x3=1,x4=0,x5=1,x6=0
#> 3911  0  1  1  0  1  0   RCT x1=0,x2=1,x3=1,x4=0,x5=1,x6=0
#> 3912  0  1  1  0  1  0   RCT x1=0,x2=1,x3=1,x4=0,x5=1,x6=0
#> 3913  0  1  1  0  1  0   RCT x1=0,x2=1,x3=1,x4=0,x5=1,x6=0
#> 3914  0  1  1  0  1  1   RCT x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 3915  0  1  1  0  1  1   RCT x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 3916  0  1  1  0  1  1   RCT x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 3917  0  1  1  0  1  1   RCT x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 3918  0  1  1  0  1  1   RCT x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 3919  0  1  1  0  1  1   RCT x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 3920  0  1  1  0  1  1   RCT x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 3921  0  1  1  0  1  1   RCT x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 3922  0  1  1  0  1  1   RCT x1=0,x2=1,x3=1,x4=0,x5=1,x6=1
#> 3923  0  1  1  1  0  0   RCT x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 3924  0  1  1  1  0  0   RCT x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 3925  0  1  1  1  0  0   RCT x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 3926  0  1  1  1  0  0   RCT x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 3927  0  1  1  1  0  0   RCT x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 3928  0  1  1  1  0  0   RCT x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 3929  0  1  1  1  0  0   RCT x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 3930  0  1  1  1  0  0   RCT x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 3931  0  1  1  1  0  0   RCT x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 3932  0  1  1  1  0  0   RCT x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 3933  0  1  1  1  0  0   RCT x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 3934  0  1  1  1  0  0   RCT x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 3935  0  1  1  1  0  0   RCT x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 3936  0  1  1  1  0  0   RCT x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 3937  0  1  1  1  0  0   RCT x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 3938  0  1  1  1  0  0   RCT x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 3939  0  1  1  1  0  0   RCT x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 3940  0  1  1  1  0  0   RCT x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 3941  0  1  1  1  0  0   RCT x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 3942  0  1  1  1  0  0   RCT x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 3943  0  1  1  1  0  0   RCT x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 3944  0  1  1  1  0  0   RCT x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 3945  0  1  1  1  0  0   RCT x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 3946  0  1  1  1  0  0   RCT x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 3947  0  1  1  1  0  0   RCT x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 3948  0  1  1  1  0  0   RCT x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 3949  0  1  1  1  0  0   RCT x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 3950  0  1  1  1  0  0   RCT x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 3951  0  1  1  1  0  0   RCT x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 3952  0  1  1  1  0  0   RCT x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 3953  0  1  1  1  0  0   RCT x1=0,x2=1,x3=1,x4=1,x5=0,x6=0
#> 3954  0  1  1  1  0  1   RCT x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 3955  0  1  1  1  0  1   RCT x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 3956  0  1  1  1  0  1   RCT x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 3957  0  1  1  1  0  1   RCT x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 3958  0  1  1  1  0  1   RCT x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 3959  0  1  1  1  0  1   RCT x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 3960  0  1  1  1  0  1   RCT x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 3961  0  1  1  1  0  1   RCT x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 3962  0  1  1  1  0  1   RCT x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 3963  0  1  1  1  0  1   RCT x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 3964  0  1  1  1  0  1   RCT x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 3965  0  1  1  1  0  1   RCT x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 3966  0  1  1  1  0  1   RCT x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 3967  0  1  1  1  0  1   RCT x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 3968  0  1  1  1  0  1   RCT x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 3969  0  1  1  1  0  1   RCT x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 3970  0  1  1  1  0  1   RCT x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 3971  0  1  1  1  0  1   RCT x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 3972  0  1  1  1  0  1   RCT x1=0,x2=1,x3=1,x4=1,x5=0,x6=1
#> 3973  0  1  1  1  1  0   RCT x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 3974  0  1  1  1  1  0   RCT x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 3975  0  1  1  1  1  0   RCT x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 3976  0  1  1  1  1  0   RCT x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 3977  0  1  1  1  1  0   RCT x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 3978  0  1  1  1  1  0   RCT x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 3979  0  1  1  1  1  0   RCT x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 3980  0  1  1  1  1  0   RCT x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 3981  0  1  1  1  1  0   RCT x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 3982  0  1  1  1  1  0   RCT x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 3983  0  1  1  1  1  0   RCT x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 3984  0  1  1  1  1  0   RCT x1=0,x2=1,x3=1,x4=1,x5=1,x6=0
#> 3985  0  1  1  1  1  1   RCT x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 3986  0  1  1  1  1  1   RCT x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 3987  0  1  1  1  1  1   RCT x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 3988  0  1  1  1  1  1   RCT x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 3989  0  1  1  1  1  1   RCT x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 3990  0  1  1  1  1  1   RCT x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 3991  0  1  1  1  1  1   RCT x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 3992  0  1  1  1  1  1   RCT x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 3993  0  1  1  1  1  1   RCT x1=0,x2=1,x3=1,x4=1,x5=1,x6=1
#> 3994  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 3995  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 3996  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 3997  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 3998  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 3999  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4000  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4001  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4002  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4003  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4004  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4005  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4006  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4007  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4008  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4009  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4010  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4011  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4012  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4013  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4014  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4015  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4016  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4017  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4018  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4019  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4020  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4021  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4022  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4023  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4024  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4025  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4026  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4027  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4028  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4029  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4030  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4031  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4032  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4033  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4034  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4035  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4036  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4037  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4038  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4039  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4040  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4041  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4042  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4043  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4044  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4045  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4046  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4047  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4048  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4049  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4050  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4051  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4052  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4053  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4054  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4055  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4056  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4057  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4058  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4059  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4060  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4061  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4062  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4063  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4064  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4065  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4066  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4067  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4068  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4069  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4070  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4071  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4072  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4073  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4074  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4075  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4076  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4077  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4078  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4079  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4080  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4081  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4082  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4083  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4084  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4085  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4086  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4087  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4088  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4089  1  0  0  0  0  0   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=0
#> 4090  1  0  0  0  0  1   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=1
#> 4091  1  0  0  0  0  1   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=1
#> 4092  1  0  0  0  0  1   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=1
#> 4093  1  0  0  0  0  1   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=1
#> 4094  1  0  0  0  0  1   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=1
#> 4095  1  0  0  0  0  1   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=1
#> 4096  1  0  0  0  0  1   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=1
#> 4097  1  0  0  0  0  1   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=1
#> 4098  1  0  0  0  0  1   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=1
#> 4099  1  0  0  0  0  1   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=1
#> 4100  1  0  0  0  0  1   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=1
#> 4101  1  0  0  0  0  1   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=1
#> 4102  1  0  0  0  0  1   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=1
#> 4103  1  0  0  0  0  1   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=1
#> 4104  1  0  0  0  0  1   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=1
#> 4105  1  0  0  0  0  1   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=1
#> 4106  1  0  0  0  0  1   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=1
#> 4107  1  0  0  0  0  1   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=1
#> 4108  1  0  0  0  0  1   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=1
#> 4109  1  0  0  0  0  1   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=1
#> 4110  1  0  0  0  0  1   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=1
#> 4111  1  0  0  0  0  1   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=1
#> 4112  1  0  0  0  0  1   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=1
#> 4113  1  0  0  0  0  1   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=1
#> 4114  1  0  0  0  0  1   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=1
#> 4115  1  0  0  0  0  1   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=1
#> 4116  1  0  0  0  0  1   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=1
#> 4117  1  0  0  0  0  1   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=1
#> 4118  1  0  0  0  0  1   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=1
#> 4119  1  0  0  0  0  1   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=1
#> 4120  1  0  0  0  0  1   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=1
#> 4121  1  0  0  0  0  1   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=1
#> 4122  1  0  0  0  0  1   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=1
#> 4123  1  0  0  0  0  1   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=1
#> 4124  1  0  0  0  0  1   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=1
#> 4125  1  0  0  0  0  1   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=1
#> 4126  1  0  0  0  0  1   RCT x1=1,x2=0,x3=0,x4=0,x5=0,x6=1
#> 4127  1  0  0  0  1  0   RCT x1=1,x2=0,x3=0,x4=0,x5=1,x6=0
#> 4128  1  0  0  0  1  0   RCT x1=1,x2=0,x3=0,x4=0,x5=1,x6=0
#> 4129  1  0  0  0  1  0   RCT x1=1,x2=0,x3=0,x4=0,x5=1,x6=0
#> 4130  1  0  0  0  1  0   RCT x1=1,x2=0,x3=0,x4=0,x5=1,x6=0
#> 4131  1  0  0  0  1  0   RCT x1=1,x2=0,x3=0,x4=0,x5=1,x6=0
#> 4132  1  0  0  0  1  0   RCT x1=1,x2=0,x3=0,x4=0,x5=1,x6=0
#> 4133  1  0  0  0  1  0   RCT x1=1,x2=0,x3=0,x4=0,x5=1,x6=0
#> 4134  1  0  0  0  1  0   RCT x1=1,x2=0,x3=0,x4=0,x5=1,x6=0
#> 4135  1  0  0  0  1  0   RCT x1=1,x2=0,x3=0,x4=0,x5=1,x6=0
#> 4136  1  0  0  0  1  0   RCT x1=1,x2=0,x3=0,x4=0,x5=1,x6=0
#> 4137  1  0  0  0  1  0   RCT x1=1,x2=0,x3=0,x4=0,x5=1,x6=0
#> 4138  1  0  0  0  1  0   RCT x1=1,x2=0,x3=0,x4=0,x5=1,x6=0
#> 4139  1  0  0  0  1  0   RCT x1=1,x2=0,x3=0,x4=0,x5=1,x6=0
#> 4140  1  0  0  0  1  0   RCT x1=1,x2=0,x3=0,x4=0,x5=1,x6=0
#> 4141  1  0  0  0  1  0   RCT x1=1,x2=0,x3=0,x4=0,x5=1,x6=0
#> 4142  1  0  0  0  1  0   RCT x1=1,x2=0,x3=0,x4=0,x5=1,x6=0
#> 4143  1  0  0  0  1  0   RCT x1=1,x2=0,x3=0,x4=0,x5=1,x6=0
#> 4144  1  0  0  0  1  0   RCT x1=1,x2=0,x3=0,x4=0,x5=1,x6=0
#> 4145  1  0  0  0  1  0   RCT x1=1,x2=0,x3=0,x4=0,x5=1,x6=0
#> 4146  1  0  0  0  1  0   RCT x1=1,x2=0,x3=0,x4=0,x5=1,x6=0
#> 4147  1  0  0  0  1  0   RCT x1=1,x2=0,x3=0,x4=0,x5=1,x6=0
#> 4148  1  0  0  0  1  0   RCT x1=1,x2=0,x3=0,x4=0,x5=1,x6=0
#> 4149  1  0  0  0  1  0   RCT x1=1,x2=0,x3=0,x4=0,x5=1,x6=0
#> 4150  1  0  0  0  1  0   RCT x1=1,x2=0,x3=0,x4=0,x5=1,x6=0
#> 4151  1  0  0  0  1  0   RCT x1=1,x2=0,x3=0,x4=0,x5=1,x6=0
#> 4152  1  0  0  0  1  0   RCT x1=1,x2=0,x3=0,x4=0,x5=1,x6=0
#> 4153  1  0  0  0  1  0   RCT x1=1,x2=0,x3=0,x4=0,x5=1,x6=0
#> 4154  1  0  0  0  1  0   RCT x1=1,x2=0,x3=0,x4=0,x5=1,x6=0
#> 4155  1  0  0  0  1  0   RCT x1=1,x2=0,x3=0,x4=0,x5=1,x6=0
#> 4156  1  0  0  0  1  0   RCT x1=1,x2=0,x3=0,x4=0,x5=1,x6=0
#> 4157  1  0  0  0  1  0   RCT x1=1,x2=0,x3=0,x4=0,x5=1,x6=0
#> 4158  1  0  0  0  1  0   RCT x1=1,x2=0,x3=0,x4=0,x5=1,x6=0
#> 4159  1  0  0  0  1  0   RCT x1=1,x2=0,x3=0,x4=0,x5=1,x6=0
#> 4160  1  0  0  0  1  0   RCT x1=1,x2=0,x3=0,x4=0,x5=1,x6=0
#> 4161  1  0  0  0  1  0   RCT x1=1,x2=0,x3=0,x4=0,x5=1,x6=0
#> 4162  1  0  0  0  1  0   RCT x1=1,x2=0,x3=0,x4=0,x5=1,x6=0
#> 4163  1  0  0  0  1  1   RCT x1=1,x2=0,x3=0,x4=0,x5=1,x6=1
#> 4164  1  0  0  0  1  1   RCT x1=1,x2=0,x3=0,x4=0,x5=1,x6=1
#> 4165  1  0  0  0  1  1   RCT x1=1,x2=0,x3=0,x4=0,x5=1,x6=1
#> 4166  1  0  0  0  1  1   RCT x1=1,x2=0,x3=0,x4=0,x5=1,x6=1
#> 4167  1  0  0  0  1  1   RCT x1=1,x2=0,x3=0,x4=0,x5=1,x6=1
#> 4168  1  0  0  0  1  1   RCT x1=1,x2=0,x3=0,x4=0,x5=1,x6=1
#> 4169  1  0  0  0  1  1   RCT x1=1,x2=0,x3=0,x4=0,x5=1,x6=1
#> 4170  1  0  0  0  1  1   RCT x1=1,x2=0,x3=0,x4=0,x5=1,x6=1
#> 4171  1  0  0  0  1  1   RCT x1=1,x2=0,x3=0,x4=0,x5=1,x6=1
#> 4172  1  0  0  1  0  0   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 4173  1  0  0  1  0  0   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 4174  1  0  0  1  0  0   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 4175  1  0  0  1  0  0   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 4176  1  0  0  1  0  0   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 4177  1  0  0  1  0  0   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 4178  1  0  0  1  0  0   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 4179  1  0  0  1  0  0   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 4180  1  0  0  1  0  0   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 4181  1  0  0  1  0  0   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 4182  1  0  0  1  0  0   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 4183  1  0  0  1  0  0   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 4184  1  0  0  1  0  0   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 4185  1  0  0  1  0  0   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 4186  1  0  0  1  0  0   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 4187  1  0  0  1  0  0   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 4188  1  0  0  1  0  0   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 4189  1  0  0  1  0  0   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 4190  1  0  0  1  0  0   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 4191  1  0  0  1  0  0   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 4192  1  0  0  1  0  0   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 4193  1  0  0  1  0  0   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 4194  1  0  0  1  0  0   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 4195  1  0  0  1  0  0   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 4196  1  0  0  1  0  0   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 4197  1  0  0  1  0  0   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 4198  1  0  0  1  0  0   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 4199  1  0  0  1  0  0   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 4200  1  0  0  1  0  0   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 4201  1  0  0  1  0  0   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 4202  1  0  0  1  0  0   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 4203  1  0  0  1  0  0   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 4204  1  0  0  1  0  0   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 4205  1  0  0  1  0  0   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 4206  1  0  0  1  0  0   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 4207  1  0  0  1  0  0   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 4208  1  0  0  1  0  0   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 4209  1  0  0  1  0  0   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 4210  1  0  0  1  0  0   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 4211  1  0  0  1  0  0   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 4212  1  0  0  1  0  0   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 4213  1  0  0  1  0  0   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 4214  1  0  0  1  0  0   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 4215  1  0  0  1  0  0   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 4216  1  0  0  1  0  0   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 4217  1  0  0  1  0  0   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 4218  1  0  0  1  0  0   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 4219  1  0  0  1  0  0   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=0
#> 4220  1  0  0  1  0  1   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 4221  1  0  0  1  0  1   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 4222  1  0  0  1  0  1   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 4223  1  0  0  1  0  1   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 4224  1  0  0  1  0  1   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 4225  1  0  0  1  0  1   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 4226  1  0  0  1  0  1   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 4227  1  0  0  1  0  1   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 4228  1  0  0  1  0  1   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 4229  1  0  0  1  0  1   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 4230  1  0  0  1  0  1   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 4231  1  0  0  1  0  1   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 4232  1  0  0  1  0  1   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 4233  1  0  0  1  0  1   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 4234  1  0  0  1  0  1   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 4235  1  0  0  1  0  1   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 4236  1  0  0  1  0  1   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 4237  1  0  0  1  0  1   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 4238  1  0  0  1  0  1   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 4239  1  0  0  1  0  1   RCT x1=1,x2=0,x3=0,x4=1,x5=0,x6=1
#> 4240  1  0  0  1  1  0   RCT x1=1,x2=0,x3=0,x4=1,x5=1,x6=0
#> 4241  1  0  0  1  1  0   RCT x1=1,x2=0,x3=0,x4=1,x5=1,x6=0
#> 4242  1  0  0  1  1  0   RCT x1=1,x2=0,x3=0,x4=1,x5=1,x6=0
#> 4243  1  0  0  1  1  0   RCT x1=1,x2=0,x3=0,x4=1,x5=1,x6=0
#> 4244  1  0  0  1  1  0   RCT x1=1,x2=0,x3=0,x4=1,x5=1,x6=0
#> 4245  1  0  0  1  1  0   RCT x1=1,x2=0,x3=0,x4=1,x5=1,x6=0
#> 4246  1  0  0  1  1  0   RCT x1=1,x2=0,x3=0,x4=1,x5=1,x6=0
#> 4247  1  0  0  1  1  0   RCT x1=1,x2=0,x3=0,x4=1,x5=1,x6=0
#> 4248  1  0  0  1  1  0   RCT x1=1,x2=0,x3=0,x4=1,x5=1,x6=0
#> 4249  1  0  0  1  1  0   RCT x1=1,x2=0,x3=0,x4=1,x5=1,x6=0
#> 4250  1  0  0  1  1  0   RCT x1=1,x2=0,x3=0,x4=1,x5=1,x6=0
#> 4251  1  0  1  0  0  0   RCT x1=1,x2=0,x3=1,x4=0,x5=0,x6=0
#> 4252  1  0  1  0  0  0   RCT x1=1,x2=0,x3=1,x4=0,x5=0,x6=0
#> 4253  1  0  1  0  0  0   RCT x1=1,x2=0,x3=1,x4=0,x5=0,x6=0
#> 4254  1  0  1  0  0  0   RCT x1=1,x2=0,x3=1,x4=0,x5=0,x6=0
#> 4255  1  0  1  0  0  0   RCT x1=1,x2=0,x3=1,x4=0,x5=0,x6=0
#> 4256  1  0  1  0  0  0   RCT x1=1,x2=0,x3=1,x4=0,x5=0,x6=0
#> 4257  1  0  1  0  0  0   RCT x1=1,x2=0,x3=1,x4=0,x5=0,x6=0
#> 4258  1  0  1  0  0  0   RCT x1=1,x2=0,x3=1,x4=0,x5=0,x6=0
#> 4259  1  0  1  0  0  0   RCT x1=1,x2=0,x3=1,x4=0,x5=0,x6=0
#> 4260  1  0  1  0  0  0   RCT x1=1,x2=0,x3=1,x4=0,x5=0,x6=0
#> 4261  1  0  1  0  0  0   RCT x1=1,x2=0,x3=1,x4=0,x5=0,x6=0
#> 4262  1  0  1  0  0  0   RCT x1=1,x2=0,x3=1,x4=0,x5=0,x6=0
#> 4263  1  0  1  0  0  0   RCT x1=1,x2=0,x3=1,x4=0,x5=0,x6=0
#> 4264  1  0  1  0  0  0   RCT x1=1,x2=0,x3=1,x4=0,x5=0,x6=0
#> 4265  1  0  1  0  0  0   RCT x1=1,x2=0,x3=1,x4=0,x5=0,x6=0
#> 4266  1  0  1  0  0  0   RCT x1=1,x2=0,x3=1,x4=0,x5=0,x6=0
#> 4267  1  0  1  0  0  0   RCT x1=1,x2=0,x3=1,x4=0,x5=0,x6=0
#> 4268  1  0  1  0  0  0   RCT x1=1,x2=0,x3=1,x4=0,x5=0,x6=0
#> 4269  1  0  1  0  0  0   RCT x1=1,x2=0,x3=1,x4=0,x5=0,x6=0
#> 4270  1  0  1  0  0  0   RCT x1=1,x2=0,x3=1,x4=0,x5=0,x6=0
#> 4271  1  0  1  0  0  0   RCT x1=1,x2=0,x3=1,x4=0,x5=0,x6=0
#> 4272  1  0  1  0  0  0   RCT x1=1,x2=0,x3=1,x4=0,x5=0,x6=0
#> 4273  1  0  1  0  0  0   RCT x1=1,x2=0,x3=1,x4=0,x5=0,x6=0
#> 4274  1  0  1  0  0  0   RCT x1=1,x2=0,x3=1,x4=0,x5=0,x6=0
#> 4275  1  0  1  0  0  0   RCT x1=1,x2=0,x3=1,x4=0,x5=0,x6=0
#> 4276  1  0  1  0  0  0   RCT x1=1,x2=0,x3=1,x4=0,x5=0,x6=0
#> 4277  1  0  1  0  0  0   RCT x1=1,x2=0,x3=1,x4=0,x5=0,x6=0
#> 4278  1  0  1  0  0  0   RCT x1=1,x2=0,x3=1,x4=0,x5=0,x6=0
#> 4279  1  0  1  0  0  0   RCT x1=1,x2=0,x3=1,x4=0,x5=0,x6=0
#> 4280  1  0  1  0  0  0   RCT x1=1,x2=0,x3=1,x4=0,x5=0,x6=0
#> 4281  1  0  1  0  0  0   RCT x1=1,x2=0,x3=1,x4=0,x5=0,x6=0
#> 4282  1  0  1  0  0  1   RCT x1=1,x2=0,x3=1,x4=0,x5=0,x6=1
#> 4283  1  0  1  0  0  1   RCT x1=1,x2=0,x3=1,x4=0,x5=0,x6=1
#> 4284  1  0  1  0  0  1   RCT x1=1,x2=0,x3=1,x4=0,x5=0,x6=1
#> 4285  1  0  1  0  0  1   RCT x1=1,x2=0,x3=1,x4=0,x5=0,x6=1
#> 4286  1  0  1  0  0  1   RCT x1=1,x2=0,x3=1,x4=0,x5=0,x6=1
#> 4287  1  0  1  0  0  1   RCT x1=1,x2=0,x3=1,x4=0,x5=0,x6=1
#> 4288  1  0  1  0  0  1   RCT x1=1,x2=0,x3=1,x4=0,x5=0,x6=1
#> 4289  1  0  1  0  0  1   RCT x1=1,x2=0,x3=1,x4=0,x5=0,x6=1
#> 4290  1  0  1  0  0  1   RCT x1=1,x2=0,x3=1,x4=0,x5=0,x6=1
#> 4291  1  0  1  0  0  1   RCT x1=1,x2=0,x3=1,x4=0,x5=0,x6=1
#> 4292  1  0  1  0  0  1   RCT x1=1,x2=0,x3=1,x4=0,x5=0,x6=1
#> 4293  1  0  1  0  0  1   RCT x1=1,x2=0,x3=1,x4=0,x5=0,x6=1
#> 4294  1  0  1  0  0  1   RCT x1=1,x2=0,x3=1,x4=0,x5=0,x6=1
#> 4295  1  0  1  0  1  0   RCT x1=1,x2=0,x3=1,x4=0,x5=1,x6=0
#> 4296  1  0  1  0  1  0   RCT x1=1,x2=0,x3=1,x4=0,x5=1,x6=0
#> 4297  1  0  1  0  1  0   RCT x1=1,x2=0,x3=1,x4=0,x5=1,x6=0
#> 4298  1  0  1  0  1  0   RCT x1=1,x2=0,x3=1,x4=0,x5=1,x6=0
#> 4299  1  0  1  0  1  0   RCT x1=1,x2=0,x3=1,x4=0,x5=1,x6=0
#> 4300  1  0  1  0  1  0   RCT x1=1,x2=0,x3=1,x4=0,x5=1,x6=0
#> 4301  1  0  1  0  1  0   RCT x1=1,x2=0,x3=1,x4=0,x5=1,x6=0
#> 4302  1  0  1  0  1  1   RCT x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 4303  1  0  1  0  1  1   RCT x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 4304  1  0  1  0  1  1   RCT x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 4305  1  0  1  0  1  1   RCT x1=1,x2=0,x3=1,x4=0,x5=1,x6=1
#> 4306  1  0  1  1  0  0   RCT x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 4307  1  0  1  1  0  0   RCT x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 4308  1  0  1  1  0  0   RCT x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 4309  1  0  1  1  0  0   RCT x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 4310  1  0  1  1  0  0   RCT x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 4311  1  0  1  1  0  0   RCT x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 4312  1  0  1  1  0  0   RCT x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 4313  1  0  1  1  0  0   RCT x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 4314  1  0  1  1  0  0   RCT x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 4315  1  0  1  1  0  0   RCT x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 4316  1  0  1  1  0  0   RCT x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 4317  1  0  1  1  0  0   RCT x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 4318  1  0  1  1  0  0   RCT x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 4319  1  0  1  1  0  0   RCT x1=1,x2=0,x3=1,x4=1,x5=0,x6=0
#> 4320  1  0  1  1  0  1   RCT x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 4321  1  0  1  1  0  1   RCT x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 4322  1  0  1  1  0  1   RCT x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 4323  1  0  1  1  0  1   RCT x1=1,x2=0,x3=1,x4=1,x5=0,x6=1
#> 4324  1  0  1  1  1  0   RCT x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 4325  1  0  1  1  1  0   RCT x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 4326  1  0  1  1  1  0   RCT x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 4327  1  0  1  1  1  0   RCT x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 4328  1  0  1  1  1  0   RCT x1=1,x2=0,x3=1,x4=1,x5=1,x6=0
#> 4329  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4330  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4331  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4332  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4333  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4334  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4335  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4336  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4337  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4338  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4339  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4340  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4341  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4342  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4343  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4344  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4345  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4346  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4347  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4348  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4349  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4350  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4351  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4352  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4353  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4354  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4355  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4356  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4357  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4358  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4359  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4360  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4361  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4362  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4363  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4364  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4365  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4366  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4367  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4368  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4369  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4370  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4371  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4372  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4373  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4374  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4375  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4376  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4377  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4378  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4379  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4380  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4381  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4382  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4383  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4384  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4385  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4386  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4387  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4388  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4389  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4390  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4391  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4392  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4393  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4394  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4395  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4396  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4397  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4398  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4399  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4400  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4401  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4402  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4403  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4404  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4405  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4406  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4407  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4408  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4409  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4410  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4411  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4412  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4413  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4414  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4415  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4416  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4417  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4418  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4419  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4420  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4421  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4422  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4423  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4424  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4425  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4426  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4427  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4428  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4429  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4430  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4431  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4432  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4433  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4434  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4435  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4436  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4437  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4438  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4439  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4440  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4441  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4442  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4443  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4444  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4445  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4446  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4447  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4448  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4449  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4450  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4451  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4452  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4453  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4454  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4455  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4456  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4457  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4458  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4459  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4460  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4461  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4462  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4463  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4464  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4465  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4466  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4467  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4468  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4469  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4470  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4471  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4472  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4473  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4474  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4475  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4476  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4477  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4478  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4479  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4480  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4481  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4482  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4483  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4484  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4485  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4486  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4487  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4488  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4489  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4490  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4491  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4492  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4493  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4494  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4495  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4496  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4497  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4498  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4499  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4500  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4501  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4502  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4503  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4504  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4505  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4506  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4507  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4508  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4509  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4510  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4511  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4512  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4513  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4514  1  1  0  1  0  1   RCT x1=1,x2=1,x3=0,x4=1,x5=0,x6=1
#> 4515  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4516  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4517  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4518  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4519  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4520  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4521  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4522  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4523  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4524  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4525  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4526  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4527  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4528  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4529  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4530  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4531  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4532  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4533  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4534  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4535  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4536  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4537  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4538  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4539  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4540  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4541  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4542  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4543  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4544  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4545  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4546  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4547  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4548  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4549  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4550  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4551  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4552  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4553  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4554  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4555  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4556  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4557  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4558  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4559  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4560  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4561  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4562  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4563  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4564  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4565  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4566  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4567  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4568  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4569  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4570  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4571  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4572  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4573  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4574  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4575  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4576  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4577  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4578  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4579  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4580  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4581  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4582  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4583  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4584  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4585  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4586  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4587  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4588  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4589  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4590  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4591  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4592  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4593  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4594  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4595  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4596  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4597  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4598  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4599  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4600  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4601  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4602  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4603  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4604  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4605  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4606  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4607  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4608  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4609  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4610  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4611  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4612  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4613  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4614  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4615  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4616  1  1  0  1  1  0   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=0
#> 4617  1  1  0  1  1  1   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 4618  1  1  0  1  1  1   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 4619  1  1  0  1  1  1   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 4620  1  1  0  1  1  1   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 4621  1  1  0  1  1  1   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 4622  1  1  0  1  1  1   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 4623  1  1  0  1  1  1   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 4624  1  1  0  1  1  1   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 4625  1  1  0  1  1  1   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 4626  1  1  0  1  1  1   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 4627  1  1  0  1  1  1   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 4628  1  1  0  1  1  1   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 4629  1  1  0  1  1  1   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 4630  1  1  0  1  1  1   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 4631  1  1  0  1  1  1   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 4632  1  1  0  1  1  1   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 4633  1  1  0  1  1  1   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 4634  1  1  0  1  1  1   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 4635  1  1  0  1  1  1   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 4636  1  1  0  1  1  1   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 4637  1  1  0  1  1  1   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 4638  1  1  0  1  1  1   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 4639  1  1  0  1  1  1   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 4640  1  1  0  1  1  1   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 4641  1  1  0  1  1  1   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 4642  1  1  0  1  1  1   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 4643  1  1  0  1  1  1   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 4644  1  1  0  1  1  1   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 4645  1  1  0  1  1  1   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 4646  1  1  0  1  1  1   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 4647  1  1  0  1  1  1   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 4648  1  1  0  1  1  1   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 4649  1  1  0  1  1  1   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 4650  1  1  0  1  1  1   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 4651  1  1  0  1  1  1   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 4652  1  1  0  1  1  1   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 4653  1  1  0  1  1  1   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 4654  1  1  0  1  1  1   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 4655  1  1  0  1  1  1   RCT x1=1,x2=1,x3=0,x4=1,x5=1,x6=1
#> 4656  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4657  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4658  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4659  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4660  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4661  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4662  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4663  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4664  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4665  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4666  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4667  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4668  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4669  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4670  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4671  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4672  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4673  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4674  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4675  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4676  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4677  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4678  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4679  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4680  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4681  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4682  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4683  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4684  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4685  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4686  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4687  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4688  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4689  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4690  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4691  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4692  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4693  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4694  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4695  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4696  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4697  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4698  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4699  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4700  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4701  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4702  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4703  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4704  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4705  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4706  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4707  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4708  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4709  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4710  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4711  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4712  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4713  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4714  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4715  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4716  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4717  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4718  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4719  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4720  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4721  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4722  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4723  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4724  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4725  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4726  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4727  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4728  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4729  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4730  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4731  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4732  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4733  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4734  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4735  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4736  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4737  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4738  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4739  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4740  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4741  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4742  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4743  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4744  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4745  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4746  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4747  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4748  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4749  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4750  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4751  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4752  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4753  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4754  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4755  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4756  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4757  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4758  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4759  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4760  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4761  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4762  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4763  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4764  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4765  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4766  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4767  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4768  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4769  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4770  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4771  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4772  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4773  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4774  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4775  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4776  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4777  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4778  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4779  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4780  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4781  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4782  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4783  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4784  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4785  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4786  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4787  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4788  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4789  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4790  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4791  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4792  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4793  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4794  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4795  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4796  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4797  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4798  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4799  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4800  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4801  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4802  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4803  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4804  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4805  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4806  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4807  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4808  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4809  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4810  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4811  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4812  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4813  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4814  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4815  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4816  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4817  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4818  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4819  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4820  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4821  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4822  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4823  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4824  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4825  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4826  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4827  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4828  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4829  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4830  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4831  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4832  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4833  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4834  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4835  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4836  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4837  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4838  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4839  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4840  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4841  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4842  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4843  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4844  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4845  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4846  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4847  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4848  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4849  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4850  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4851  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4852  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4853  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4854  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4855  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4856  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4857  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4858  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4859  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4860  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4861  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4862  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4863  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4864  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4865  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4866  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4867  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4868  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4869  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4870  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4871  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4872  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4873  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4874  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4875  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4876  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4877  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4878  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4879  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4880  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4881  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4882  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4883  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4884  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4885  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4886  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4887  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4888  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4889  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4890  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4891  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4892  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4893  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4894  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4895  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4896  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4897  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4898  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4899  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4900  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4901  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4902  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4903  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4904  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4905  1  1  1  0  0  0   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=0
#> 4906  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4907  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4908  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4909  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4910  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4911  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4912  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4913  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4914  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4915  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4916  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4917  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4918  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4919  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4920  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4921  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4922  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4923  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4924  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4925  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4926  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4927  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4928  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4929  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4930  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4931  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4932  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4933  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4934  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4935  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4936  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4937  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4938  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4939  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4940  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4941  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4942  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4943  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4944  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4945  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4946  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4947  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4948  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4949  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4950  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4951  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4952  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4953  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4954  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4955  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4956  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4957  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4958  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4959  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4960  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4961  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4962  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4963  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4964  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4965  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4966  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4967  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4968  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4969  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4970  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4971  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4972  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4973  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4974  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4975  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4976  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4977  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4978  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4979  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4980  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4981  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4982  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4983  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4984  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4985  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4986  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4987  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4988  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4989  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4990  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4991  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4992  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4993  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4994  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4995  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4996  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4997  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4998  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 4999  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 5000  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 5001  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 5002  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 5003  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 5004  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 5005  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 5006  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 5007  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 5008  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 5009  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 5010  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 5011  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 5012  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 5013  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 5014  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 5015  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 5016  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 5017  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 5018  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 5019  1  1  1  0  0  1   RCT x1=1,x2=1,x3=1,x4=0,x5=0,x6=1
#> 5020  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5021  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5022  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5023  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5024  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5025  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5026  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5027  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5028  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5029  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5030  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5031  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5032  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5033  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5034  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5035  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5036  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5037  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5038  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5039  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5040  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5041  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5042  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5043  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5044  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5045  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5046  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5047  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5048  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5049  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5050  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5051  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5052  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5053  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5054  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5055  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5056  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5057  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5058  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5059  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5060  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5061  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5062  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5063  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5064  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5065  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5066  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5067  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5068  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5069  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5070  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5071  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5072  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5073  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5074  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5075  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5076  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5077  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5078  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5079  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5080  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5081  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5082  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5083  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5084  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5085  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5086  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5087  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5088  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5089  1  1  1  0  1  0   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=0
#> 5090  1  1  1  0  1  1   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=1
#> 5091  1  1  1  0  1  1   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=1
#> 5092  1  1  1  0  1  1   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=1
#> 5093  1  1  1  0  1  1   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=1
#> 5094  1  1  1  0  1  1   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=1
#> 5095  1  1  1  0  1  1   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=1
#> 5096  1  1  1  0  1  1   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=1
#> 5097  1  1  1  0  1  1   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=1
#> 5098  1  1  1  0  1  1   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=1
#> 5099  1  1  1  0  1  1   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=1
#> 5100  1  1  1  0  1  1   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=1
#> 5101  1  1  1  0  1  1   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=1
#> 5102  1  1  1  0  1  1   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=1
#> 5103  1  1  1  0  1  1   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=1
#> 5104  1  1  1  0  1  1   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=1
#> 5105  1  1  1  0  1  1   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=1
#> 5106  1  1  1  0  1  1   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=1
#> 5107  1  1  1  0  1  1   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=1
#> 5108  1  1  1  0  1  1   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=1
#> 5109  1  1  1  0  1  1   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=1
#> 5110  1  1  1  0  1  1   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=1
#> 5111  1  1  1  0  1  1   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=1
#> 5112  1  1  1  0  1  1   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=1
#> 5113  1  1  1  0  1  1   RCT x1=1,x2=1,x3=1,x4=0,x5=1,x6=1
#> 5114  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5115  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5116  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5117  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5118  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5119  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5120  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5121  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5122  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5123  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5124  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5125  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5126  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5127  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5128  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5129  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5130  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5131  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5132  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5133  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5134  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5135  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5136  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5137  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5138  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5139  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5140  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5141  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5142  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5143  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5144  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5145  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5146  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5147  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5148  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5149  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5150  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5151  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5152  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5153  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5154  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5155  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5156  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5157  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5158  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5159  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5160  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5161  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5162  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5163  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5164  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5165  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5166  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5167  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5168  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5169  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5170  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5171  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5172  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5173  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5174  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5175  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5176  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5177  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5178  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5179  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5180  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5181  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5182  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5183  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5184  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5185  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5186  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5187  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5188  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5189  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5190  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5191  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5192  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5193  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5194  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5195  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5196  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5197  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5198  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5199  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5200  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5201  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5202  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5203  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5204  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5205  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5206  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5207  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5208  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5209  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5210  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5211  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5212  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5213  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5214  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5215  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5216  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5217  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5218  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5219  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5220  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5221  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5222  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5223  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5224  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5225  1  1  1  1  0  0   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=0
#> 5226  1  1  1  1  0  1   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 5227  1  1  1  1  0  1   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 5228  1  1  1  1  0  1   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 5229  1  1  1  1  0  1   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 5230  1  1  1  1  0  1   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 5231  1  1  1  1  0  1   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 5232  1  1  1  1  0  1   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 5233  1  1  1  1  0  1   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 5234  1  1  1  1  0  1   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 5235  1  1  1  1  0  1   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 5236  1  1  1  1  0  1   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 5237  1  1  1  1  0  1   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 5238  1  1  1  1  0  1   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 5239  1  1  1  1  0  1   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 5240  1  1  1  1  0  1   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 5241  1  1  1  1  0  1   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 5242  1  1  1  1  0  1   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 5243  1  1  1  1  0  1   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 5244  1  1  1  1  0  1   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 5245  1  1  1  1  0  1   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 5246  1  1  1  1  0  1   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 5247  1  1  1  1  0  1   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 5248  1  1  1  1  0  1   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 5249  1  1  1  1  0  1   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 5250  1  1  1  1  0  1   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 5251  1  1  1  1  0  1   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 5252  1  1  1  1  0  1   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 5253  1  1  1  1  0  1   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 5254  1  1  1  1  0  1   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 5255  1  1  1  1  0  1   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 5256  1  1  1  1  0  1   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 5257  1  1  1  1  0  1   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 5258  1  1  1  1  0  1   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 5259  1  1  1  1  0  1   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 5260  1  1  1  1  0  1   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 5261  1  1  1  1  0  1   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 5262  1  1  1  1  0  1   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 5263  1  1  1  1  0  1   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 5264  1  1  1  1  0  1   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 5265  1  1  1  1  0  1   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 5266  1  1  1  1  0  1   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 5267  1  1  1  1  0  1   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 5268  1  1  1  1  0  1   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 5269  1  1  1  1  0  1   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 5270  1  1  1  1  0  1   RCT x1=1,x2=1,x3=1,x4=1,x5=0,x6=1
#> 5271  1  1  1  1  1  0   RCT x1=1,x2=1,x3=1,x4=1,x5=1,x6=0
#> 5272  1  1  1  1  1  0   RCT x1=1,x2=1,x3=1,x4=1,x5=1,x6=0
#> 5273  1  1  1  1  1  0   RCT x1=1,x2=1,x3=1,x4=1,x5=1,x6=0
#> 5274  1  1  1  1  1  0   RCT x1=1,x2=1,x3=1,x4=1,x5=1,x6=0
#> 5275  1  1  1  1  1  0   RCT x1=1,x2=1,x3=1,x4=1,x5=1,x6=0
#> 5276  1  1  1  1  1  0   RCT x1=1,x2=1,x3=1,x4=1,x5=1,x6=0
#> 5277  1  1  1  1  1  0   RCT x1=1,x2=1,x3=1,x4=1,x5=1,x6=0
#> 5278  1  1  1  1  1  0   RCT x1=1,x2=1,x3=1,x4=1,x5=1,x6=0
#> 5279  1  1  1  1  1  0   RCT x1=1,x2=1,x3=1,x4=1,x5=1,x6=0
#> 5280  1  1  1  1  1  0   RCT x1=1,x2=1,x3=1,x4=1,x5=1,x6=0
#> 5281  1  1  1  1  1  0   RCT x1=1,x2=1,x3=1,x4=1,x5=1,x6=0
#> 5282  1  1  1  1  1  0   RCT x1=1,x2=1,x3=1,x4=1,x5=1,x6=0
#> 5283  1  1  1  1  1  0   RCT x1=1,x2=1,x3=1,x4=1,x5=1,x6=0
#> 5284  1  1  1  1  1  0   RCT x1=1,x2=1,x3=1,x4=1,x5=1,x6=0
#> 5285  1  1  1  1  1  0   RCT x1=1,x2=1,x3=1,x4=1,x5=1,x6=0
#> 5286  1  1  1  1  1  0   RCT x1=1,x2=1,x3=1,x4=1,x5=1,x6=0
#> 5287  1  1  1  1  1  0   RCT x1=1,x2=1,x3=1,x4=1,x5=1,x6=0
#> 5288  1  1  1  1  1  0   RCT x1=1,x2=1,x3=1,x4=1,x5=1,x6=0
#> 5289  1  1  1  1  1  0   RCT x1=1,x2=1,x3=1,x4=1,x5=1,x6=0
#> 5290  1  1  1  1  1  0   RCT x1=1,x2=1,x3=1,x4=1,x5=1,x6=0
#> 5291  1  1  1  1  1  0   RCT x1=1,x2=1,x3=1,x4=1,x5=1,x6=0
#> 5292  1  1  1  1  1  0   RCT x1=1,x2=1,x3=1,x4=1,x5=1,x6=0
#> 5293  1  1  1  1  1  0   RCT x1=1,x2=1,x3=1,x4=1,x5=1,x6=0
#> 5294  1  1  1  1  1  0   RCT x1=1,x2=1,x3=1,x4=1,x5=1,x6=0
#> 5295  1  1  1  1  1  0   RCT x1=1,x2=1,x3=1,x4=1,x5=1,x6=0
#> 5296  1  1  1  1  1  1   RCT x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 5297  1  1  1  1  1  1   RCT x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 5298  1  1  1  1  1  1   RCT x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 5299  1  1  1  1  1  1   RCT x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 5300  1  1  1  1  1  1   RCT x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 5301  1  1  1  1  1  1   RCT x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 5302  1  1  1  1  1  1   RCT x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 5303  1  1  1  1  1  1   RCT x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
#> 5304  1  1  1  1  1  1   RCT x1=1,x2=1,x3=1,x4=1,x5=1,x6=1
```

<img src="man/figures/README-unnamed-chunk-5-4.png" width="100%" />

### Step 4: Model Validation

This step compare the estimates from RWD with those from RCT data, and
validate difference methods using RWD.

``` r
fusion <- Summary$new(target.obj,
                      source.obj,
                      source.obj.rep)
fusion$evaluate()
#> # A tibble: 34 x 7
#> # Groups:   group_name [17]
#>    group_name          estimator             size     mse len_ci agg.est agg.reg
#>    <chr>               <chr>                <dbl>   <dbl>  <dbl> <lgl>   <lgl>  
#>  1 pop                 G_computation/glm/E~  2618 0        4.52  TRUE    TRUE   
#>  2 pop                 G_computation/glm     2618 5.88e+2  0.237 FALSE   TRUE   
#>  3 x1=0,x3=0,x4=0,x5=0 G_computation/glm/E~    57 1.03e+0 14.6   TRUE    TRUE   
#>  4 x1=0,x3=0,x4=0,x5=0 G_computation/glm       57 1.84e+3  1.11  FALSE   TRUE   
#>  5 x1=0,x3=0,x4=0,x5=1 G_computation/glm/E~   104 1.7 e-2  9.02  TRUE    TRUE   
#>  6 x1=0,x3=0,x4=0,x5=1 G_computation/glm      104 1.66e+3  0.886 FALSE   TRUE   
#>  7 x1=0,x3=0,x4=1,x5=0 G_computation/glm/E~   206 1.61e+0  6.06  TRUE    TRUE   
#>  8 x1=0,x3=0,x4=1,x5=0 G_computation/glm      206 1.46e+3  0.666 FALSE   TRUE   
#>  9 x1=0,x3=0,x4=1,x5=1 G_computation/glm/E~   367 1.97e+0  6.20  TRUE    TRUE   
#> 10 x1=0,x3=0,x4=1,x5=1 G_computation/glm      367 9.41e+2  0.547 FALSE   TRUE   
#> # ... with 24 more rows
#> # i Use `print(n = ...)` to see more rows
fusion$plot()
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />
