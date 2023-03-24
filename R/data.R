#' A data set of simulated observational data, where outcome is continuous variable, treatment is a binary variable.
#'
#' @format ## `source.data`
#' A data frame with 5000 rows and 8 variables:
#' \describe{
#' \item{x1}{binary variable, x1 ~ rbinom(5000,1,0.2)}
#' \item{x2}{binary variable, x2 ~ rbinom(5000,1,0.2)}
#' \item{x3}{binary variable, x3 ~ rbinom(5000,1,0.8)}
#' \item{x4}{binary variable, x4 ~ rbinom(5000,1,0.8)}
#' \item{x5}{binary variable, x5 ~ rbinom(5000,1,0.7)}
#' \item{x6}{binary variable, x6 ~ rbinom(5000,1,0.8)}
#' \item{z}{binary variable indicating treatment and control. pp = x1*x2+x3*x4+5*x5+x6, p(z=1) = p = 1/1+e^{-(pp-mean(pp))/sd(pp)*sqrt(3)/pi}, z ~ rbinom(5000,1,p)}
#' \item{y}{continuous variable indicating outcome, y ~ x1 + 6*x2+x6+2*z + rnorm(5000,0,1)}
#' }
"source.data"


#' A data set of simulated RCT data, where outcome is continuous variable, treatment is a binary variable.
#'
#' @format ## `target.data`
#' A data frame with 5000 rows and 8 variables:
#' \describe{
#' \item{x1}{binary variable, x1 ~ rbinom(5000,1,0.7)}
#' \item{x2}{binary variable, x2 ~ rbinom(5000,1,0.9)}
#' \item{x3}{binary variable, x3 ~ rbinom(5000,1,0.2)}
#' \item{x4}{binary variable, x4 ~ rbinom(5000,1,0.3)}
#' \item{x5}{binary variable, x5 ~ rbinom(5000,1,0.2)}
#' \item{x6}{binary variable, x6 ~ rbinom(5000,1,0.3)}
#' \item{z}{binary variable indicating treatment and control, z ~ rbinom(5000,1,0.5)}
#' \item{y}{continuous variable indicating outcome, y ~ x1 + 6*x2+x6+2*z + rnorm(5000,0,1)}
#' }
"target.data"


#' A synthetic QUASAR trial dataset, where outcome is a binary variable, treatment is a binary variable.
#'
#' @format ## 'quasar.synthetic'
#' A data frame with 5934 rows and 3 variables:
#' \describe{
#' \item{Stage2}{binary variable, 1 indicating stage 2 and 0 indicating stage 3}
#' \item{male}{binary variable, 1 indicating male and 0 indicating female}
#' \item{age}{categorical variable, 1 indicating [23,50], 2 indicating [50,59], 3 indicating [60,69], 4 indicating [70,86]}
#' }
'quasar.synthetic'

#' An object of class TEstimator_Synthetic using quasar.synthetic
'quasar.obj'

#' Aggregated data derived from paper of QUASAR trial
'quasar.agg'

#' A dataset of simulated observational data, where outcome is binary variable. The data is filtered after compared to target.binary.data
#' @format
#' A data frame with 2624 rows and 9 variables.
#' \describe{
#' \item{x1}{binary variable, x1 ~ rbinom(5000,1,0.2)}
#' \item{x2}{binary variable, x2 ~ rbinom(5000,1,0.2)}
#' \item{x3}{binary variable, x3 ~ rbinom(5000,1,0.8)}
#' \item{x4}{binary variable, x4 ~ rbinom(5000,1,0.8)}
#' \item{x5}{binary variable, x5 ~ rbinom(5000,1,0.7)}
#' \item{x6}{binary variable, x6 ~ rbinom(5000,1,0.8)}
#' \item{z}{binary variable. pp = x1*x2+x3*x4+5*x5+x6, p(z=1) = p = 1/1+e^{-(pp-mean(pp))/sd(pp)*sqrt(3)/pi}, z ~ rbinom(5000,1,p)}
#' \item{y}{binary variable. pp = x1 + (6*x2+x6+2)*z, p(y=1) = p = 1/1+e^{-(pp-mean(pp))/sd(pp)*sqrt(3)/pi}, y ~ rbinom(5000,1,p)}
#' \item{pt}{a continuous variable within 0 and 1, specifying the probability of p(z=1) given x1,x2,x3,x4,x5,x6}
#' }
'source.binary.data'


#' A dataset of simulated RCT data, where outcome is binary variable. The data is filtered after compared to source.binary.data
#' @format
#' A data frame with 3194 rows and 9 variables.
#' \describe{
#' \item{x1}{binary variable, x1 ~ rbinom(5000,1,0.7)}
#' \item{x2}{binary variable, x2 ~ rbinom(5000,1,0.9)}
#' \item{x3}{binary variable, x3 ~ rbinom(5000,1,0.2)}
#' \item{x4}{binary variable, x4 ~ rbinom(5000,1,0.3)}
#' \item{x5}{binary variable, x5 ~ rbinom(5000,1,0.2)}
#' \item{x6}{binary variable, x6 ~ rbinom(5000,1,0.3)}
#' \item{z}{binary variable. pp = x1*x2+x3*x4+5*x5+x6, p(z=1) = p = 1/1+exp^{-(pp-mean(pp))/sd(pp)*sqrt(3)/pi}, z ~ rbinom(5000,1,p)}
#' \item{y}{binary variable. pp = x1 + (6*x2+x6+2)*z, p(y=1) = p = 1/1+exp^{-(pp-mean(pp))/sd(pp)*sqrt(3)/pi}, y ~ rbinom(5000,1,p)}
#' \item{pt}{a continuous variable within 0 and 1, specifying the probability of p(z=1) given x1,x2,x3,x4,x5,x6}
#' }
'target.binary.data'







