dr_estFUN <- function(data, models){

  Z <- data$Z
  Y <- data$Y

  Xe <- grab_design_matrix(
    data, rhs_formula = grab_fixed_formula(models$e))
  Xm0 <- grab_design_matrix(
    data, rhs_formula = grab_fixed_formula(models$m0))
  Xm1 <- grab_design_matrix(
    data, rhs_formula = grab_fixed_formula(models$m1))

  e_pos  <- 1:ncol(Xe)
  m0_pos <- (max(e_pos) + 1):(max(e_pos) + ncol(Xm0))
  m1_pos <- (max(m0_pos) + 1):(max(m0_pos) + ncol(Xm1))

  e_scores  <- grab_psiFUN(models$e, data)
  m0_scores <- grab_psiFUN(models$m0, data)
  m1_scores <- grab_psiFUN(models$m1, data)

  function(theta){
    e  <- plogis(Xe %*% theta[e_pos])
    m0 <- Xm0 %*% theta[m0_pos]
    m1 <- Xm1 %*% theta[m1_pos]
    rd_hat <- (Z*Y - (Z - e) * m1)/e - ((1 - Z) * Y - (Z - e) * m0)/(1 - e)
    c(e_scores(theta[e_pos]),
      m0_scores(theta[m0_pos]) * (Z == 0),
      m1_scores(theta[m1_pos]) * (Z == 1),
      rd_hat - theta[length(theta)])
  }
}


library(geex)
estimate_dr <- function(data, propensity_formula, outcome_formula){
  e_model  <- glm(propensity_formula, data = data, family = binomial)
  m0_model <- glm(outcome_formula, subset = (Z == 0), data = data)
  m1_model <- glm(outcome_formula, subset = (Z == 1), data = data)
  models <- list(e = e_model, m0 = m0_model, m1 = m1_model)
  nparms <- sum(unlist(lapply(models, function(x) length(coef(x))))) + 1

  m_estimate(
    estFUN = dr_estFUN,
    data   = data,
    root_control = setup_root_control(start = rep(0, nparms)),
    outer_args = list(models = models))
}


library(mvtnorm)
tau_0 <- c(-1, -1, 1, 1)
tau_1 <- tau_0 * -1
Sigma_X3 <- matrix(
  c(1, 0.5, -0.5, -0.5,
    0.5, 1, -0.5, -0.5,
    -0.5, -0.5, 1, 0.5,
    -0.5, -0.5, 0.5, 1), ncol = 4, byrow = TRUE)

gen_data <- function(n, beta, nu, xi){
  X3 <- rbinom(n, 1, prob = 0.2)
  V3 <- rbinom(n, 1, prob = (0.75 * X3 + (0.25 * (1 - X3))))
  hold <- rmvnorm(n,  mean = rep(0, 4), Sigma_X3)
  colnames(hold) <- c("X1", "V1", "X2", "V2")
  hold <- cbind(hold, X3, V3)
  hold <- apply(hold, 1, function(x){
    x[1:4] <- x[1:4] + tau_1^(x[5])*tau_0^(1 - x[5])
    x})
  hold <- t(hold)[, c("X1", "X2", "X3", "V1", "V2", "V3")]
  X <- cbind(Int = 1, hold)
  Z <- rbinom(n, 1, prob = plogis(X[, 1:4] %*% beta))
  X <- cbind(X[, 1:4], Z, X[, 5:7])
  data.frame(
    Y = X %*% c(nu, xi) + rnorm(n),
    X[ , -1])
}


dt <- gen_data(1000, c(0, 0.6, -0.6, 0.6), c(0, -1, 1, -1, 2), c(-1, 1, 1))
geex_results <- estimate_dr(dt, Z ~ X1 + X2 + X3, Y ~ X1 + X2 + X3)

e  <- predict(glm(Z ~ X1 + X2 + X3, data = dt, family = "binomial"),
              type = "response")
m0 <- predict(glm(Y ~ X1 + X2 + X3, data = dt, subset = Z==0), newdata = dt)
m1 <- predict(glm(Y ~ X1 + X2 + X3, data = dt, subset = Z==1), newdata = dt)
del_hat <- with(dt, mean( (Z * Y - (Z - e) * m1)/e)) -
  with(dt, mean(((1 - Z) * Y - (Z - e) * m0)/(1 - e)))




n <- 100
y1 <- rnorm(n,2,1)
y0 <- rnorm(n,1,1)
data <- cbind(y1, y0)
data <- as.data.frame(data)
colnames(data) <- c("y1","y0")


gc_estfun <- function(data){
  Y1 <- data$y1
  Y0 <- data$y0
  function(theta) {
    c(Y1 - theta[1],
      Y0 - theta[2],
      theta[1]-theta[2] - theta[3]
    )
  }
}
library(geex)
results <- geex::m_estimate(estFUN = gc_estfun,
                            data = data,
                            root_control = setup_root_control(start = c(0,0,0)))
est <- results@estimates[3]
se <- sqrt(results@vcov[3,3])

results@vcov[1,1] + results@vcov[2,2] - 2*results@vcov[1,2]

est.2 <- mean(y1)-mean(y0)
se.2 <- sqrt(var(y1)+var(y0))/sqrt(n)
c(est,est.2)
c(se,se.2)

c(results@vcov[1,1],mean((y1-mean(y1))^2)/n)
c(results@vcov[2,2],mean((y0-mean(y0))^2)/n)

### sandwitch robust se is the same as var(tau)/n
tau <- y1-y0
c(results@vcov[3,3], mean((tau - mean(tau))^2)/n)







