mu <- 1
sigma <- 4
sigma_est <- NULL
sigma_mean <- NULL
sample.size <- seq(100,20000,100)

for(n in sample.size){
  x <- rnorm(n, mean = mu, sd=sqrt(sigma))
  x.mean <- mean(x)
  sigma_mean_boot <- var(sapply(1:25, function(i) mean(sample(x,size = n, replace = TRUE))))
  sigma_hat <- var(x-x.mean)
  sigma_est <- c(sigma_est,sigma_hat)
  sigma_mean <- c(sigma_mean,sigma_mean_boot)
}

plot(x=sample.size,sigma_est)
plot(x=sample.size,sigma_mean)


library(boot)
n <- 1000
x1 <- rbinom(n,1,0.3)
x2 <- rbinom(n,1,0.5)
z <- ifelse((x1==1)&(x2==1),0,1)
logit.ps <- (x1+x2+z - mean(x1+x2+z))/sd(x1+x2+z)
ps <- inv.logit(logit.ps)
y <- as.factor(rbinom(n,1,ps))
data <- as.data.frame(cbind(x1,x2,z,y))
data %>% group_by(x1,x2,z) %>% summarise(n=n())
data$x1 <- as.factor(data$x1)
data$x2 <- as.factor(data$x2)
data$z <- as.factor(data$z)
data$y <- as.factor(data$y)

glm.obj <- glm(y~x1+x2+z, data = data, family = binomial(link = "logit"))

data.1 <- data
data.1$z <- levels(data$z)[2]
predict(glm.obj, newdata = data.1, type = "response")



# ===================================
# ===================================
# hypothesis
n <- 1000
var_name <- c("x1","x2","x3")
p_success <- c(0.7,0.9,0.2,0.3,0.2,0.3)
tau <- "7*x2+x3+2"
y0 <- "x1"
log.ps <- "x1*x2+x3"
rho1 <- c("x1","x2",0)
rho2 <- c("x2","x3",0)
# ===================================
# simulating data
data <- DGM(trial=FALSE,  n_rct, var_name, p_success_rct, tau, y0, log.ps=log.ps, binary = FALSE, noise = 1, rho1, rho2)
data$tau <- 7*data$x2+data$x3+2
# test if we miss variable x3 in outcome modeling, then would we find bimode of distribution of cate?
obj <- glm(y~x1+x2+z+x2:z, data = data)
data.1 <- data.0 <- data
data.1$z <- 1
data.0$z <- 0
y1.hat <- predict(obj, newdata = data.1, type = "response")
y0.hat <- predict(obj, newdata = data.0, type = "response")
cate <- y1.hat-y0.hat
data <- data.frame(data, cate=cate)

ggplot(data = data%>%filter((x1==0)&(x2==0)) , aes(x=cate))+geom_density()

group1 <- data %>% filter((x1==0)&(x2==0))
group1$cate
summary(group1$cate)
summary(group1$tau)


# ===================================
# test the effect of non-confouding predictors on treatment effect estimation
n_rct <- 10000; n_rwd <- 10000
var_name <- c("x1","x2","x3")
p_success_rct <- c(0.7,0.7,0.2)
p_success_rwd <- c(0.7,0.7,0.2)
tau_rct <- "x1+10*x2+1"
tau_rwd <- "x1+10*x2+1"
y0_rct <- "x1"
y0_rwd <- "x1"
log.ps <- "10*x2"
rho1 <- c("x1","x2",0)
rho2 <- c("x2","x3",0)
# simulating data
target.data <- RCTrep::DGM(trial=TRUE,  n_rct, var_name, p_success_rct, tau_rct, y0_rct, log.ps=0, binary = FALSE, noise=1, rho1)
source.data <- RCTrep::DGM(trial=FALSE, n_rwd, var_name, p_success_rwd, tau_rwd, y0_rwd, log.ps,   binary = FALSE, noise=1, rho1)
vars_name <- list(confounders_treatment=c("x1","x2","x3"),
                  confounders_sampling=c("x1","x2","x3"),
                  treatment_name=c('z'),
                  outcome_name=c('y')
)

Estimator <- "G_computation"
strata <- c("x1","x2")
strata_joint <- TRUE
data.public <- TRUE

output <- RCTREP(Estimator="G_computation", weighting_estimator = "Balancing",
                 #treatment_method = "glm",
                 outcome_method = "glm",
                 #treatment_formula= z ~ x1:x2 + x3:x4+ x5+ x6,
                 outcome_formula = y ~ z + x1:z + x1 + x2:z,
                 source.data=source.data, target.data=target.data, vars_name=vars_name,
                 stratification = strata, stratification_joint = strata_joint,
                 data.public = data.public)
summary(target.obj = output$target.obj, source.obj = output$source.obj)
summary(output$source.obj$model)

library(dplyr)
source.data$y <- as.numeric(as.character(source.data$y))
target.data$y <- as.numeric(as.character(target.data$y))

source.data %>% group_by(x2,z) %>% summarise(ybar=mean(y))
target.data %>% group_by(x2,z) %>% summarise(ybar=mean(y))

mean(target.data[target.data$z==1,"y"]) - mean(target.data[target.data$z==0,"y"])
mean(source.data[source.data$z==1,"y"]) - mean(source.data[source.data$z==0,"y"])






