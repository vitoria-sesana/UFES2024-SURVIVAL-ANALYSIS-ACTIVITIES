# exponencial -------------------------------------------------------------

time <- base$temp_falha
event <- base$ind_falha

# Função de log-verossimilhança negativa (pois optim minimiza)
loglik_exp <- function(log_lambda) {
  lambda <- exp(log_lambda)  # garantimos que lambda > 0
  -sum(event * log(lambda) - lambda * time)
}

# Estimando com optim
fit_exp <- optim(par = log(0.1), fn = loglik_exp, method = "BFGS", hessian = TRUE)

# Resultado
lambda_est <- exp(fit_exp$par)

cat("Estimativa de lambda:", lambda_est, "\n")



# weibull -----------------------------------------------------------------


# Função de log-verossimilhança negativa
loglik_weibull <- function(par) {
  log_shape <- par[1]
  log_scale <- par[2]
  
  shape <- exp(log_shape)
  scale <- exp(log_scale)
  
  loglik <- sum(
    event * (log(shape) - shape * log(scale) + (shape - 1) * log(time)) -
      (time / scale)^shape
  )
  
  return(-loglik)  # porque o optim minimiza
}

# Estimativa inicial
start_par <- log(c(1, mean(time)))

# Ajuste com optim
fit_weib <- optim(par = start_par, fn = loglik_weibull, method = "BFGS", hessian = TRUE)

# Transformando de volta
shape_est <- exp(fit_weib$par[1])
scale_est <- exp(fit_weib$par[2])

cat("Shape estimado:", shape_est, "\n")
cat("Scale estimado:", scale_est, "\n")

# log normal --------------------------------------------------------------

loglik_lognormal <- function(par) {
  mu <- par[1]
  log_sigma <- par[2]
  sigma <- exp(log_sigma)
  
  tt <- time
  
  log_f <- dlnorm(tt, meanlog = mu, sdlog = sigma, log = TRUE)
  log_S <- plnorm(tt, meanlog = mu, sdlog = sigma, lower.tail = FALSE, log.p = TRUE)
  
  loglik <- sum(event * log_f + (1 - event) * log_S)
  return(-loglik)
}

# Estimativas iniciais
start_par <- c(mean(log(time + 1e-5)), log(sd(log(time + 1e-5))))

# Otimização
fit_lognorm <- optim(par = start_par, fn = loglik_lognormal, method = "BFGS", hessian = TRUE)

# Resultados
mu_est <- fit_lognorm$par[1]
sigma_est <- exp(fit_lognorm$par[2])

cat("mu estimado:", mu_est, "\n")
cat("sigma estimado:", sigma_est, "\n")


# log logisitico ----------------------------------------------------------

# log-verossimilhança negativa do log-logístico
loglik_loglogistic <- function(par) {
  log_alpha <- par[1]
  log_lambda <- par[2]
  
  alpha <- exp(log_alpha)
  lambda <- exp(log_lambda)
  
  tt <- time
  
  # Funções
  log_f <- log(alpha / lambda) + (alpha - 1) * log(tt / lambda) - 2 * log(1 + (tt / lambda)^alpha)
  log_S <- -log(1 + (tt / lambda)^alpha)
  
  loglik <- sum(event * log_f + (1 - event) * log_S)
  return(-loglik)
}

# Estimativas iniciais
start_par <- log(c(1, median(time)))

# Otimização
fit_llogis <- optim(par = start_par, fn = loglik_loglogistic, method = "BFGS", hessian = TRUE)

# Resultados
alpha_est <- exp(fit_llogis$par[1])
lambda_est <- exp(fit_llogis$par[2])

cat("Shape (alpha) estimado:", alpha_est, "\n")
cat("Scale (lambda) estimado:", lambda_est, "\n")


