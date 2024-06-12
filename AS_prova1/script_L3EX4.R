# ANÁLISE DE SOBREVIVÊNCIA
# EXERCiCIO 4 DA LISTA 3

# bibliotecas -------------------------------------------------------------
require(survival)
require(survminer)
require(summarytools)
require(dplyr)
require(flexsurv)

# base de dados -----------------------------------------------------------
base <- 
  cbind(
    c(0.19, 0.78, 0.96, 1.31, 2.78, 3.16, 4.67, 4.85, 
      6.50, 7.35, 8.27, 12.07, 32.52, 33.91, 36.71), 
    rep(1, 15)) 

colnames(base) <- c("temp_falha", "ind_falha") 
base <- as_tibble(base)

# kaplan meier ------------------------------------------------------------
KaplanMeir <- 
  survfit(
    Surv(time = temp_falha, 
         event = ind_falha) ~ 1, 
    data = base
    )

st_KaplanMeier <- KaplanMeir$surv

# exponential -------------------------------------------------------------
modelExponential <- 
  flexsurvreg(
    Surv(
      time = temp_falha,
      event = ind_falha
    ) ~ 1,
    data = base,  
    dist = "exponential"
  )

st_Exponencial <- as.data.frame(summary(modelExponential))$est

# weibull -----------------------------------------------------------------
modelWeibull <-
  flexsurvreg(
    Surv(
      time = temp_falha,
      event = ind_falha
    ) ~ 1,
    data = base,  
    dist = "weibull"
  )

st_Weibull <- as.data.frame(summary(modelWeibull))$est

# log normal --------------------------------------------------------------
modelLogNorm <- 
  flexsurvreg(
    Surv(
      time = temp_falha,
      event = ind_falha
      ) ~ 1,
    data = base,  
    dist = "lognorm"
  )

st_LogNorm <- as.data.frame(summary(modelLogNorm))$est

# log logistic ------------------------------------------------------------
modelLogLogistic <- 
  flexsurvreg(
    Surv(
      time = temp_falha,
      event = ind_falha
    ) ~ 1,
    data = base,  
    dist = "llogis"
  )

st_LogLogistic <- as.data.frame(summary(modelLogLogistic))$est

# gráficos comparativos --------------------------------------------------

# base com as estimativas de KP, EXP, WEIBULL, LOGNORM e LOGLOGISTIC
sts <- cbind(
  base$temp_falha,
  st_KaplanMeier,
  st_Exponencial,
  st_Weibull,
  st_LogNorm,
  st_LogLogistic
  ) %>%
  as_tibble()
  
# KP x EXP
ggExp <- sts %>%
ggplot(aes(x = st_KaplanMeier,
           y = st_Exponencial)) +
  geom_point() +
  xlab("S(t): Kaplan-Meier") +
  ylab("S(t): Exponencial") +
  theme_classic() +
  geom_abline(color = "red") +
  scale_x_continuous(breaks = seq(0,1, 0.1)) + 
  scale_y_continuous(breaks = seq(0,1, 0.1))

# KP x WEIBULL
ggWeib <- sts %>%
  ggplot(aes(x = st_KaplanMeier,
             y = st_Weibull)) +
  geom_point() +
  xlab("S(t): Kaplan-Meier") +
  ylab("S(t): Weibull") +
  theme_classic() +
  geom_abline(color = "red") +
  scale_x_continuous(breaks = seq(0,1, 0.1)) + 
  scale_y_continuous(breaks = seq(0,1, 0.1))

# KP x LN
ggLN <- sts %>%
  ggplot(aes(x = st_KaplanMeier,
             y = st_LogNorm)) +
  geom_point() +
  xlab("S(t): Kaplan-Meier") +
  ylab("S(t): Log-Normal") +
  theme_classic() +
  geom_abline(color = "red") +
  scale_x_continuous(breaks = seq(0,1, 0.1)) + 
  scale_y_continuous(breaks = seq(0,1, 0.1))

# KP x LL
ggLL <- sts %>%
  ggplot(aes(x = st_KaplanMeier,
             y = st_LogLogistic)) +
  geom_point() +
  xlab("S(t): Kaplan-Meier") +
  ylab("S(t): Log Logística") +
  theme_classic() +
  geom_abline(color = "red") +
  scale_x_continuous(breaks = seq(0,1, 0.1)) + 
  scale_y_continuous(breaks = seq(0,1, 0.1))

# erro quadrático
eq <- sts %>% 
  mutate(eq_Exp2 = (st_KaplanMeier - st_Exponencial)^2,
         eq_Wei2 = (st_KaplanMeier - st_Weibull )^2,
         eq_LN2 = (st_KaplanMeier - st_LogNorm )^2,
         eq_LL2 = (st_KaplanMeier - st_LogLogistic)^2,
         ) %>%
  select(eq_Exp2, eq_Wei2, eq_LN2, eq_LL2)

# erro quadrático médio
mqe <- apply(eq, 2, sum)/length(base$temp_falha)

# tabela parâmetros estimados
est <- modelLogNorm$res # escala original
est_log <-modelLogNorm$res.t # escala log

muln <- modelLogNorm$res[1,1]
sdln <- modelLogNorm$res[2,1]

# tempo mediano
t_0.5 = exp( (qnorm(0.5) * sdln) + muln)

# tempo medio
tm = exp( muln + ( (sdln^2) / 2 ) )

# tempo isolante 20% rupturados
z_0.2 <- qnorm(0.2)
t = exp(muln + (sdln * z_0.2) )

