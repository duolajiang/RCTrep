# ===================================
n_rct <- 5000; n_rwd <- 5000
var_name <- c("x1","x2","x3","x4","x5","x6")
p_success_rct <- c(0.7,0.9,0.2,0.3,0.2,0.3)
p_success_rwd <- c(0.2,0.2,0.8,0.8,0.7,0.8)
tau_rct <- "6*x2+x6+2"
tau_rwd <- "3*x2+2*x3"
y0_rct <- "x1"
y0_rwd <- "x5"
log.ps <- "x1*x2+x3*x4+5*x5+x6"
rho1 <- c("x1","x2",0)
rho2 <- c("x2","x3",0)

target.data.2 <- RCTrep::DGM(trial=TRUE,  n_rct, var_name, p_success_rct, tau_rct, y0_rct, log.ps=0, binary = FALSE, noise=1, rho1, rho2)
source.data.2 <- RCTrep::DGM(trial=FALSE, n_rwd, var_name, p_success_rwd, tau_rwd, y0_rwd, log.ps,   binary = FALSE, noise=1, rho1, rho2)
usethis::use_data(source.data.2,target.data.2,overwrite = TRUE)
