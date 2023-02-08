set.seed(201)

#### Problem 1 ####
#### Part 1 ####

# Plot density function curve in R
y <- seq(0, 1, 0.01)
fy <- dnorm(y)
plot(y, fy, type = "l", col = "red", ylim=c(0,1), xlim=c(0,2))
# Plot proposal distribution g(y) which is Uniform(0, 1)
lines(y, 2*dcauchy(y), col = "blue") # 1*g(y)

# c = 2

# Rejection sampling method
n <- 1000
k <- 0 #counter for accepted
j <- 0 #counter for iterations
accepted_samples <- rep(0, n)

while(k < n){
  proposal_sample <- rcauchy(1) #random sample from Cauchy distribution which is Uniform(0, 1)
  uniform <- runif(1)
  j <- j + 1
  if (uniform < dnorm(proposal_sample)/(2*dcauchy(proposal_sample))){
    #we accept the proposal sample
    k <- k + 1
    accepted_samples[k] <- proposal_sample
  }
}

#### Part 2 ####

# Compare empirical and theoretical percentiles
p <- seq(0.1, 0.9, 0.01)
Qhat <- quantile(accepted_samples, p) # quantiles from sample
Q <- qnorm(p) # theoretical quantiles

round(rbind(Qhat, Q), 3)

#### Problem 2 ####
#### Part 1 ####

n <- 10000
x1 <- rnorm(n)

target_function <- (dnorm(x1, 1, 1)/2) + (dnorm(x1, 3, 1)/2)
importance_samples <- dnorm(x1)
theta.hat <- mean(x1 * target_function/importance_samples)

#### Part 2 ####

plot(cumsum(((x1 * target_function)/importance_samples))/(1:n), type = 'l', ylab = "")
abline(h=2, col = 'red')


