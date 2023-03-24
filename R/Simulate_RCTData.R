#' @title Generating the synthetic RCT data given marginal distribution of each covariate
#'
#' @param margin_dis a character indicating the distribution of each variable, allowable options is \code{"bernoulli_categorical"} and \code{"bernoulli"}
#' @param N a numeric value indicating the sample size for the simulated data
#' @param margin a list containing the marginal distribution of variables; if margin_dis="bernoulli_categorical", then margin should be list(x1=c("x1",nlevels(x1),level1, level2,...,leveln, plevel1, plevel2,...,plevel3), x2=c(...)); if margin_dis="bernoulli", margin=list(p(x1=1),p(x2=1),...,p(xn=1))
#' @param var_name a vector indicating the name of variables, the order should be in line with margin
#' @param pw.cor a vector containing the pairwise correlation of these variables, default is NULL; when margin_dis="bernoulli", then pw.cor must be specified
#'
#' @returns a data frame
#'
#' @export
#' @example
#' RCT.univariate.p <- list(Stage2=c("Stage2",2,0,1,1-0.91,0.91),
#'                          male=c("male",2,0,1,1-0.62,0.62),
#'                          age=c("age",4,1,2,3,4,0.11,0.26,0.42,0.21))
#'
#'
#' quasar.synthetic <- RCTrep::GenerateSyntheticData(margin_dis="bernoulli_categorical",
#'                                                   N=1000,
#'                                                   margin=RCT.univariate.p,
#'                                                   var_name=c("Stage2","male","age"))
#'
GenerateSyntheticData <- function(margin_dis,N,margin,var_name, pw.cor=0) {
  if(margin_dis == "bernoulli_categorical") {
    synthetic.data <- GenerateSyntheticData_bernoullicategorical(N, margin, var_name)
  } else if (margin_dis == "bernoulli"){
    synthetic.data <- GenerateSyntheticData_multibernoulli(N, margin, var_name, pw.cor)
  } else {
    message("to be continued")
  }
  return(synthetic.data)
}

#' @param N a numeric value indicating the sample size for the simulated data
#' @param margin a list containing the marginal distribution of binary variable
#' @param var_name a vector incating the name of variables
#' @param pw.cor a vector containing the pairwise correlation of these variables
#' @importFrom copula normalCopula mvdc rMvdc
GenerateSyntheticData_multibernoulli <- function(N, margin, var_name, pw.cor){
  nvar <- length(var_name)
  myCop <- normalCopula(param=pw.cor,
                        dim = nvar, dispstr = "un")

  paramMargin <- function(x) {list(1,unlist(x))}
  myMvd <- mvdc(copula=myCop,
                margins = rep("binom",nvar),
                paramMargins=lapply(margin, paramMargin))

  synthetic.data <- rMvdc(N, myMvd)
  synthetic.data <- as.data.frame(synthetic.data)
  colnames(synthetic.data) <- var_name
  return(synthetic.data)
}


#' @param N a numeric value indicating the sample size for the simulated data
#' @param margin a list containing the marginal distribution of binary and categorical variable, margin should be list(x1=c("x1",nlevels(x1),level1, level2,...,leveln, plevel1, plevel2,...,plevel3), x2=c(...)),
#' @param var_name a vector indicating the name of variables; must be in line with the first element in each list in parameter of margin.
GenerateSyntheticData_bernoullicategorical <- function(N, margin, var_name){
  #browser()
  pattern_distribution <- PatternDistribution(margin, var_name)
  target <- generateSyntheticData(N, pattern_distribution)
  return(target)
}


