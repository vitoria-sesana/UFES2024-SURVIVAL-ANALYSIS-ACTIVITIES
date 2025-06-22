
# kp ----------------------------------------------------------------------

# gráfico S(t) estimada
gg_kp <- 
  survminer::ggsurvplot(
    model_np_kaplan_meier, data = base,
    pval = FALSE, conf.int=TRUE, 
    conf.int.style = "ribbon",
    ylab = "S(t) estimada", 
    xlab = "Tempo (minutos)"
  )

gg_kp$plot <- 
  gg_kp$plot +
  scale_y_continuous(
    limits = c(0, 1), 
    breaks = seq(0, 1, by = 0.1)) +
  scale_x_continuous(
    limits = c(0, max(base$temp_falha)),
    breaks = seq(0, max(base$temp_falha), by = 1))+
  theme(legend.position = "none")

ggKP<-gg_kp$plot

# estimativas S(t) de cada modelo -----------------------------------------
st_KaplanMeier <- model_np_kaplan_meier$surv
st_exponencial <- as.data.frame(summary(model_exponential))$est
st_weibull <- as.data.frame(summary(model_weibull))$est
st_logNorm <- as.data.frame(summary(model_logNorm))$est
st_logLogistic <- as.data.frame(summary(model_logLogistic))$est

# tabela comparativa  -----------------------------------------------------

# base com as estimativas S(t) para KP, EXP, WEIBULL, LOGNORM e LOGLOGISTIC
tabela_sts <- cbind(
  base$temp_falha,
  st_KaplanMeier,
  st_exponencial,
  st_weibull,
  st_logNorm,
  st_logLogistic
) %>%
  as_tibble()

tabela_sts %>% 
  xtable::xtable()

# KP x EXP
ggExp <- sts %>%
  ggplot(aes(x = st_KaplanMeier,
             y = st_exponencial)) +
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
             y = st_weibull)) +
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
             y = st_logNorm)) +
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
             y = st_logLogistic)) +
  geom_point() +
  xlab("S(t): Kaplan-Meier") +
  ylab("S(t): Log Logística") +
  theme_classic() +
  geom_abline(color = "red") +
  scale_x_continuous(breaks = seq(0,1, 0.1)) + 
  scale_y_continuous(breaks = seq(0,1, 0.1))

ggKP
ggExp
ggWeib
ggLN
ggLL


ggsave(
  filename = "AS_lista3_ex3/g1.png",
  plot = ggKP,
  width = 15,
  height = 10,
  units = "cm",
  dpi = 300
)

ggsave(
  filename = "AS_lista3_ex3/g2.png",
  plot = ggExp,
  width = 15,
  height = 10,
  units = "cm",
  dpi = 300
)

ggsave(
  filename = "AS_lista3_ex3/g3.png",
  plot = ggWeib,
  width = 15,
  height = 10,
  units = "cm",
  dpi = 300
)

ggsave(
  filename = "AS_lista3_ex3/g4.png",
  plot = ggLN,
  width = 15,
  height = 10,
  units = "cm",
  dpi = 300
)

ggsave(
  filename = "AS_lista3_ex3/g5.png",
  plot = ggLL,
  width = 15,
  height = 10,
  units = "cm",
  dpi = 300
)

