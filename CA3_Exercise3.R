df <- with(mtcars, data.frame(y=mpg, x1=disp, x2=hp, x3=wt))

nll_lm <- function(par, data){
  
  beta <- par[1:4]
  sigma_squared <- par[5]
  
  X <- cbind(1, as.matrix(data[, c("x1", "x2", "x3")]))
  y <- data$y
  mu <- X %*% beta
  
  llik <- dnorm(y, mean = mu, sd = sqrt(sigma_squared), log = TRUE)
  
  return(-sum(llik))
}

inits <- c(mean(df$y), 0, 0, 0, 1)
lower_bound <- c(-Inf, -Inf, -Inf, -Inf, 1e-8)
upper_bound <- c(Inf, Inf, Inf, Inf, Inf)

fit <- optim(par = inits, fn = nll_lm, data = df, method = "L-BFGS-B", lower = lower_bound, upper = upper_bound, hessian = TRUE)
fit

X <- cbind(1, as.matrix(df[, c("x1", "x2", "x3")]))
y <- df$y
beta <- fit$par[1:4]
beta_hat_matrix <- solve(t(X) %*% X) %*% t(X) %*% y
beta
beta_hat_matrix


sigma_hat <- fit$par[5]
resid <- y - X %*% beta_hat_matrix
sigma_hat2 <- sqrt(sum(resid^2) / (nrow(X) - ncol(X)))
sigma_hat
sigma_hat2

sqrt(diag(solve(fit$hessian)))[1:4]