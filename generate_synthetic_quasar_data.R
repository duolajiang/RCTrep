#' the function compute two-year and five year treatment effect
AteRct <- function(year){
  if(year==5){
    Nd1 <- 154; Nc1 <- 1325-773-Nd1; Nt1 <- 1325-Nc1/2; Pd1 <- Nd1/Nt1; ps1 <- (1-98/1522.5)*(1-Pd1); n1 <- Nt1; p1 <- 1-ps1
    Nd0 <- 158; Nc0 <- 1288-735-Nd0; Nt0 <- 1288-Nc0/2; Pd0 <- Nd0/Nt0; ps0 <- (1-129/1517)*(1-Pd0); n0 <- Nt0; p0 <- 1-ps0
    aterct <- p1-p0
  } else{
    Nd1 <- 98;  Nc1 <- 1622-1325-Nd1; Nt1 <- 1622-Nc1/2; Pd1 <- Nd1/Nt1; ps1 <- 1-Pd1; n1 <- Nt1; p1 <- 1-ps1
    Nd0 <- 129; Nc0 <- 1617-1288-Nd0; Nt0 <- 1617-Nc0/2; Pd0 <- Nd0/Nt0; ps0 <- 1-Pd0; n0 <- Nt0; p0 <- 1-ps0
    aterct <- p1-p0
  }
  return(aterct)
}

#' the resulting data frame should have the following format:
#' data.frame(name,
#'            value,
#'            y1.hat,
#'            y0.hat,
#'            cate,
#'            se,
#'            size)
#' the same as the TEstimator$CATE
CateRctByOneStrata <- function(name,value,p1,p0,samplesize1,samplesize0){
  #browser()
  dd <- cbind(p1,samplesize1,p0,samplesize0)
  CATE_mean_se <- data.frame(name=name,
                             value=value,
                             y1.hat = p1,
                             y0.hat = p0,
                             cate= p1-p0,
                             se = apply(dd,1, function(x) sqrt(x[1]*(1-x[1])/x[2]+x[3]*(1-x[3])/x[4])),
                             size = samplesize1+samplesize0,
                             stringsAsFactors = FALSE)
  return(CATE_mean_se)
}

CATE_mean_se <- CateRctByOneStrata(name = c("Stage2","Stage2",
                                            "male","male",
                                            "age","age",
                                            "age","age"),
                                   value = c(1,0,1,0,1,2,3,4),
                                   p1 = c(0.161,0.45,0.2,0.17,0.108,0.147,0.196,0.287),
                                   p0 = c(0.185,0.58,0.24,0.211,0.124,0.194,0.254,0.28),
                                   samplesize1 = c(1073,131,1006,616,185,428,678,331),
                                   samplesize0 = c(1073,129,973,644,185,427,673,332))
RCT.univariate.p <- list(Stage2=c("Stage2",2,0,1,1-0.91,0.91),
                         male=c("male",2,0,1,1-0.62,0.62),
                         age=c("age",4,1,2,3,4,0.11,0.26,0.42,0.21))
quasar.agg <- list(ATE_mean = AteRct(year=5),
                   ATE_se = 0.01,
                   CATE_mean_se = CATE_mean_se,
                   univariate_p = RCT.univariate.p,
                   n = 2406)

quasar.synthetic <- RCTrep::GenerateSyntheticData(margin_dis="bernoulli_categorical",
                                                  N=dim(source.data)[1],
                                                  margin=RCT.univariate.p,
                                                  var_name=c("Stage2","male","age"))

quasar.synthetic$age <- as.factor(quasar.synthetic$age)

quasar.obj <- TEstimator_Synthetic$new(df = quasar.synthetic,
                                       estimates=quasar.agg,
                                       vars_name = c("Stage2","male","age"),
                                       name = "RCT",
                                       isTrial = TRUE,
                                       data.public = TRUE)

usethis::use_data(quasar.agg,quasar.obj,overwrite = TRUE)
















