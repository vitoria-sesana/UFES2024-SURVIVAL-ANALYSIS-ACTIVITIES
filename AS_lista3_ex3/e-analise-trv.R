# Modelo Gama Generalizada
model_gengamma <- 
  flexsurvreg(
    Surv(
      time = temp_falha,
      event = ind_falha) ~ 1,
      data = base, dist = "gengamma")

model_gengamma$loglik
model_exponential$loglik
model_weibull$loglik
model_logNorm$loglik
model_logLogistic$loglik

loglik_gengamma <- model_gengamma$loglik %>% round(3)
loglik_exp <- model_exponential$loglik %>% round(3)
loglik_weibull <- model_weibull$loglik %>% round(3)
loglik_lnorm <- model_logNorm$loglik %>% round(3)
loglik_llog <- model_logLogistic$loglik %>% round(3)

trv_exp <- 2*(loglik_exp - loglik_gengamma) %>% round(3) %>% abs()
trv_weibull <- 2*(loglik_weibull- loglik_gengamma) %>% round(3)%>% abs()
trv_lnorm <- 2*(loglik_lnorm - loglik_gengamma) %>% round(3)%>% abs()
trv_llog <- 2*(loglik_llog - loglik_gengamma) %>% round(3)%>% abs()


p_exp <- 1 - pchisq(trv_exp, 2) %>% round(3)
p_weibull <- 1 - pchisq(trv_weibull, 1) %>% round(3)
p_lnorm <- 1 - pchisq(trv_lnorm, 1) %>% round(3)
p_llog <- 1 - pchisq(trv_llog, 1) %>% round(3)


matrix(
  c(
    loglik_gengamma, "-", "-", 
    loglik_exp, trv_exp, p_exp,
    loglik_weibull, trv_weibull, p_weibull,
    loglik_lnorm, trv_lnorm, p_lnorm,
    loglik_llog, trv_llog, p_llog
  ),
  ncol = 3,
  byrow = TRUE
) %>% 
  as.data.frame() %>% 
  xtable::xtable()

# weibull -------------------------------------

model_weibull$res %>% 
  xtable::xtable()

# parametros
shape <- model_weibull$res["shape", "est"]
scale <- model_weibull$res["scale", "est"]
shape
scale
# Média (esperança)
mean_weibull <- scale * gamma(1 + (1 / shape))

# Mediana
median_weibull <- scale * (-log(1-0.5))^(1/shape)

# 
t20_weibull <- scale * (-log(1-0.2))^(1/shape)
t20_weibull

cat("Tempo médio de vida:", mean_weibull, "\n")
cat("Tempo mediano de vida:", median_weibull, "\n")


mean_weibull
median_weibull
t20_weibull

matrix(
  c(
    mean_weibull,
    median_weibull,
    t20_weibull
  ),
  nrow = 3
) %>% 
  as.data.frame() %>% 
  xtable::xtable()
