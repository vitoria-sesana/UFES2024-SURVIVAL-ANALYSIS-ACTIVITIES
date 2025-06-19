
# pacotes -----------------------------------------

library(dplyr)
library(maxstat)
require(survival)
require(survminer)
require(summarytools)

# leitura ---------------------

base <- 
  readxl::read_xlsx(
    path = "P1_ex_4/bases/Lista2_Hodgkins.xlsx"
    )

# tratamento ------------------

base <-
  base %>% 
  mutate(
    age_cat = 
      case_when(
        age < 25 ~ "< 25",
        age >= 25 & age < 38 ~ "25 a 38 anos",
        age >= 38 & age < 53 ~ "38 a 53 anos",
        age >= 53 ~ ">= 53 anos"
        ), 
    aux_dead = 
      case_when(
        dead == 0 ~ "Óbito não observado",
        dead == 1 ~ "Óbito observado"
        ),
    aux_sex = 
      case_when(
        sex == 0 ~ "Feminino",
        sex == 1 ~ "Masculino"
        ),
    aux_stage = 
      case_when(
        stage == 0 ~ "Inicial",
        stage == 1 ~ "Avançado"
        ),
    aux_hist = 
      case_when(
        hist == 1 ~ "Esclerose nodular",
        hist == 2 ~ "Misto celular",
        hist == 3 ~ "Fepleção de linfócitos"
        ),
    aux_age_cat = 
      factor(
        age_cat, 
        labels = 
          c(
            "< 25",
            "25 a 38 anos", 
            "38 a 53 anos",
            ">= 53 anos"
            )
        )
    )


# gênero ----------------------------

ajuste_sexo <- 
  survfit(
    Surv(
      time = survivaltime, 
      event = dead
      ) ~ aux_sex, 
      data = base
  )

# tabela das estimativas de KM e intervalo de confiança:
surv_summary(ajuste_sexo, data = base)

# estimativas sobrevida por kaplan-meier
kp_sexo <- 
  surv_summary(
    ajuste_sexo, 
    data = base
    )

# gráfico S(t) estimada
gg_sexo <- 
  survminer::ggsurvplot(
    ajuste_sexo, data = base,
    pval = FALSE, conf.int=TRUE, 
    conf.int.style = "ribbon",
    ylab = "S(t) estimada", 
    xlab = "Tempo (meses)", 
    legend.title = "", 
    legend.labs = c("Feminino","Masculino")
    )

gg_sexo$plot <- 
  gg_sexo$plot +
  scale_y_continuous(
    limits = c(0, 1), 
    breaks = seq(0, 1, by = 0.1)) +
  scale_x_continuous(
    limits = c(0, max(base$survivaltime)),
    breaks = seq(0, max(base$survivaltime), by = 10)) +
  theme(legend.position = "bottom")

gg_sexo$plot

# teste log-rank
test_log_sexo <-
  survdiff(
    Surv(
      time = survivaltime, 
      event = dead) ~ aux_sex,
    data = base,
    rho = 0
    ) 

test_log_sexo$pvalue
test_log_sexo$chisq

# teste wilcoxon
test_wilcox_sexo <-
  survdiff(
    Surv(
      time = survivaltime, 
      event = dead) ~ aux_sex,
    data = base,
    rho = 1
  ) 

test_wilcox_sexo$pvalue
test_wilcox_sexo$chisq

# estágio da doença -----------------

ajuste_stage <- 
  survfit(
    Surv(
      time = survivaltime, 
      event = dead
    ) ~ aux_stage, 
    data = base
  )

ajuste_stage

# tabela das estimativas de KM e intervalo de confiança:
surv_summary(ajuste_stage, data = base)

# estimativas sobrevida por kaplan-meier
kp_stage <- 
  surv_summary(
    ajuste_stage, 
    data = base
  )

# gráfico S(t) estimada
gg_stage <- 
  survminer::ggsurvplot(
    ajuste_stage, data = base,
    pval = FALSE, conf.int=TRUE, 
    conf.int.style = "ribbon",
    ylab = "S(t) estimada", 
    xlab = "Tempo (meses)", 
    legend.title = "",
    legend.labs = levels(factor(base$aux_stage))
  )

gg_stage$plot <- 
  gg_stage$plot +
  scale_y_continuous(
    limits = c(0, 1), 
    breaks = seq(0, 1, by = 0.1)) +
  scale_x_continuous(
    limits = c(0, max(base$survivaltime)),
    breaks = seq(0, max(base$survivaltime), by = 10)) +
  theme(legend.position = "bottom")

gg_stage$plot


# teste log-rank
test_log_stage <-
  survdiff(
    Surv(
      time = survivaltime, 
      event = dead) ~ aux_stage,
    data = base,
    rho = 0
  ) 

test_log_stage$pvalue
test_log_stage$chisq

# teste wilcoxon
test_wilcox_stage <-
  survdiff(
    Surv(
      time = survivaltime, 
      event = dead) ~ aux_stage,
    data = base,
    rho = 1
  ) 

test_wilcox_stage$pvalue
test_wilcox_stage$chisq

# histologia ------------------------

ajuste_hist <- 
  survfit(
    Surv(
      time = survivaltime, 
      event = dead
    ) ~ aux_hist, 
    data = base
  )

ajuste_hist

# tabela das estimativas de KM e intervalo de confiança:
surv_summary(ajuste_hist, data = base)

# estimativas sobrevida por kaplan-meier
kp_hist <- 
  surv_summary(
    ajuste_hist, 
    data = base
  )

# gráfico S(t) estimada
gg_hist <- 
  survminer::ggsurvplot(
    ajuste_hist, data = base,
    pval = FALSE, conf.int=TRUE, 
    conf.int.style = "ribbon",
    ylab = "S(t) estimada", 
    xlab = "Tempo (meses)", 
    legend.title = "",
    legend.labs = levels(factor(base$aux_hist))
  )

gg_hist$plot <- 
  gg_hist$plot +
  scale_y_continuous(
    limits = c(0, 1), 
    breaks = seq(0, 1, by = 0.1)) +
  scale_x_continuous(
    limits = c(0, max(base$survivaltime)),
    breaks = seq(0, max(base$survivaltime), by = 10)) +
  theme(legend.position = "bottom")

gg_hist$plot

# teste log-rank 2 a 2 
test_log_hist1 <-
  survdiff(
    Surv(survivaltime,
         dead) ~ aux_hist, 
    data = 
      subset(
        base,
        aux_hist %in% 
          c("Esclerose nodular", 
            "Fepleção de linfócitos")),
    rho = 0
  )
test_log_hist2 <-
  survdiff(
    Surv(survivaltime,
         dead) ~ aux_hist, 
    data = 
      subset(
        base, 
        aux_hist %in% 
          c("Esclerose nodular",
            "Misto celular")
        ),
    rho = 0
  )
test_log_hist3 <-
  survdiff(
    Surv(survivaltime, dead) ~ aux_hist, 
    data = 
      subset(
        base, 
        aux_hist %in% 
          c("Fepleção de linfócitos",
            "Misto celular")
        ),
    rho = 0
  )

# teste wilcoxon 2 a 2 
test_wilcox_hist1 <-
  survdiff(
    Surv(survivaltime,
         dead) ~ aux_hist, 
    data = 
      subset(
        base,
        aux_hist %in% 
          c("Esclerose nodular", 
            "Fepleção de linfócitos")),
    rho = 1
  )
test_wilcox_hist2 <-
  survdiff(
    Surv(survivaltime,
         dead) ~ aux_hist, 
    data = 
      subset(
        base, 
        aux_hist %in% 
          c("Esclerose nodular",
            "Misto celular")
      ),
    rho = 1
  )
test_wilcox_hist3 <-
  survdiff(
    Surv(survivaltime, dead) ~ aux_hist, 
    data = 
      subset(
        base, 
        aux_hist %in% 
          c("Fepleção de linfócitos",
            "Misto celular")
      ),
    rho = 1
  )

# idade contínua --------------------
  
# ponto de corte
cut_age <- 
  maxstat::maxstat.test(
    Surv(survivaltime, dead) ~ age,
    data = base,
    smethod = "LogRank",  
    pmethod = "none",  
    minprop = 0.1,  
)

print(cut_age)
plot(cut_age)
plot(cut_age,
     xlab = "Idade", 
     ylab = "Estatística de log-rank padronizada")

ponto_corte <- cut_age$estimate
ponto_corte

base <- 
  base %>% 
  mutate(
    aux_age_pc =
      case_when(
        age <= ponto_corte ~ paste("<=", ponto_corte),
        age > ponto_corte ~ paste(">", ponto_corte),
      ),
    
    aux_age_pc =
      factor(
        aux_age_pc,
        levels = 
          c(
            paste("<=", ponto_corte),
            paste(">", ponto_corte)
          )
      )
  )


# ajuste 
ajuste_age_pc <- 
  survfit(
    Surv(
      time = survivaltime, 
      event = dead
    ) ~ aux_age_pc, 
    data = base
  )

ajuste_age_pc

# tabela das estimativas de KM e intervalo de confiança:
surv_summary(ajuste_age_pc, data = base)

# estimativas sobrevida por kaplan-meier
kp_age_pc <- 
  surv_summary(
    ajuste_age_pc, 
    data = base
  )

# gráfico S(t) estimada
gg_age_pc <- 
  survminer::ggsurvplot(
    ajuste_age_pc, data = base,
    pval = FALSE, conf.int=TRUE, 
    conf.int.style = "ribbon",
    ylab = "S(t) estimada", 
    xlab = "Tempo (meses)", 
    legend.title = "",
    legend.labs = levels(factor(base$aux_age_pc))
  )

gg_age_pc$plot <- 
  gg_age_pc$plot +
  scale_y_continuous(
    limits = c(0, 1), 
    breaks = seq(0, 1, by = 0.1)) +
  scale_x_continuous(
    limits = c(0, max(base$survivaltime)),
    breaks = seq(0, max(base$survivaltime), by = 10)) +
  theme(legend.position = "bottom")

gg_age_pc$plot


# teste log-rank
test_log_age_pc <-
  survdiff(
    Surv(
      time = survivaltime, 
      event = dead) ~ aux_age_pc,
    data = base,
    rho = 0
  ) 

test_log_age_pc$pvalue
test_log_age_pc$chisq

# teste wilcoxon
test_wilcox_age_pc <-
  survdiff(
    Surv(
      time = survivaltime, 
      event = dead) ~ aux_age_pc,
    data = base,
    rho = 1
  ) 

test_wilcox_age_pc$pvalue
test_wilcox_age_pc$chisq

# idade categórica ------------------

ajuste_age_cat <- 
  survfit(
    Surv(
      time = survivaltime, 
      event = dead
    ) ~ aux_age_cat, 
    data = base
  )

ajuste_age_cat

# tabela das estimativas de KM e intervalo de confiança:
surv_summary(ajuste_age_cat, data = base)

# estimativas sobrevida por kaplan-meier
kp_age_cat <- 
  surv_summary(
    ajuste_age_cat, 
    data = base
  )

# gráfico S(t) estimada
gg_age_cat <- 
  survminer::ggsurvplot(
    ajuste_age_cat, data = base,
    pval = FALSE, conf.int=TRUE, 
    conf.int.style = "ribbon",
    ylab = "S(t) estimada", 
    xlab = "Tempo (meses)", 
    legend.title = "", 
    legend.labs = levels(base$aux_age_cat)
  )  

gg_age_cat$plot <- 
  gg_age_cat$plot +
  scale_y_continuous(
    limits = c(0, 1), 
    breaks = seq(0, 1, by = 0.1)) +
  scale_x_continuous(
    limits = c(0, max(base$survivaltime)),
    breaks = seq(0, max(base$survivaltime), by = 10)) +
  scale_color_discrete(name = "") +
  scale_fill_discrete(name = "") +
  theme(legend.position = "bottom") 

gg_age_cat$plot


# teste log-rank 2 a 2 
test_log_age_cat1 <-
  survdiff(
    Surv(survivaltime,
         dead) ~ aux_age_cat, 
    data = 
      subset(
        base,
        aux_age_cat %in% 
          c("< 25", 
            "25 a 38 anos")),
    rho = 0
  )
test_log_age_cat2 <-
  survdiff(
    Surv(survivaltime,
         dead) ~ aux_age_cat, 
    data = 
      subset(
        base, 
        aux_age_cat %in% 
          c("< 25",
            "38 a 53 anos")
      ),
    rho = 0
  )
test_log_age_cat3 <-
  survdiff(
    Surv(survivaltime, dead) ~ aux_age_cat, 
    data = 
      subset(
        base, 
        aux_age_cat %in% 
          c("< 25",
            ">= 53 anos")
      ),
    rho = 0
  )
test_log_age_cat4 <-
  survdiff(
    Surv(survivaltime, dead) ~ aux_age_cat, 
    data = 
      subset(
        base, 
        aux_age_cat %in% 
          c("25 a 38 anos",
            "38 a 53 anos")
      ),
    rho = 0
  )
test_log_age_cat5 <-
  survdiff(
    Surv(survivaltime, dead) ~ aux_age_cat, 
    data = 
      subset(
        base, 
        aux_age_cat %in% 
          c("25 a 38 anos",
            ">= 53 anos")
      ),
    rho = 0
  )

test_log_age_cat6 <-
  survdiff(
    Surv(survivaltime, dead) ~ aux_age_cat, 
    data = 
      subset(
        base, 
        aux_age_cat %in% 
          c("38 a 53 anos",
            ">= 53 anos")
      ),
    rho = 0
  )

# teste wilcoxon 2 a 2 
test_wilcox_age_cat1 <-
  survdiff(
    Surv(survivaltime,
         dead) ~ aux_age_cat, 
    data = 
      subset(
        base,
        aux_age_cat %in% 
          c("< 25", 
            "25 a 38 anos")),
    rho = 1
  )
test_wilcox_age_cat2 <-
  survdiff(
    Surv(survivaltime,
         dead) ~ aux_age_cat, 
    data = 
      subset(
        base, 
        aux_age_cat %in% 
          c("< 25",
            "38 a 53 anos")
      ),
    rho = 1
  )
test_wilcox_age_cat3 <-
  survdiff(
    Surv(survivaltime, dead) ~ aux_age_cat, 
    data = 
      subset(
        base, 
        aux_age_cat %in% 
          c("< 25",
            ">= 53 anos")
      ),
    rho = 1
  )
test_wilcox_age_cat4 <-
  survdiff(
    Surv(survivaltime, dead) ~ aux_age_cat, 
    data = 
      subset(
        base, 
        aux_age_cat %in% 
          c("25 a 38 anos",
            "38 a 53 anos")
      ),
    rho = 1
  )
test_wilcox_age_cat5 <-
  survdiff(
    Surv(survivaltime, dead) ~ aux_age_cat, 
    data = 
      subset(
        base, 
        aux_age_cat %in% 
          c("25 a 38 anos",
            ">= 53 anos")
      ),
    rho = 1
  )

test_wilcox_age_cat6 <-
  survdiff(
    Surv(survivaltime, dead) ~ aux_age_cat, 
    data = 
      subset(
        base, 
        aux_age_cat %in% 
          c("38 a 53 anos",
            ">= 53 anos")
      ),
    rho = 1
  )


# saidas -------------------------------

# ggsave(
#   filename = "P1_ex_4/saidas/g1_sexo.png",
#   plot = gg_sexo$plot,
#   width = 15,
#   height = 10,
#   units = "cm",
#   dpi = 300
# )
# 
# ggsave(
#   filename = "P1_ex_4/saidas/g2_estagio.png",
#   plot = gg_stage$plot,
#   width = 15,
#   height = 10,
#   units = "cm",
#   dpi = 300
# )
# 
# ggsave(
#   filename = "P1_ex_4/saidas/g3_histologia.png",
#   plot = gg_hist$plot,
#   width = 15,
#   height = 10,
#   units = "cm",
#   dpi = 300
# )
# 
# png(
#   filename = "P1_ex_4/saidas/g4_corte.png", 
#   width = 1770, 
#   height = 1180, 
#   res = 300
#   )
# plot(
#   cut_age, 
#   xlab = "Idade", 
#   ylab = "Estatística de log-rank padronizada"
#   )
# dev.off()
# 
# ggsave(
#   filename = "P1_ex_4/saidas/g5_age_pc.png",
#   plot = gg_age_pc$plot,
#   width = 15,
#   height = 10,
#   units = "cm",
#   dpi = 300
# )
# 
# ggsave(
#   filename = "P1_ex_4/saidas/g6_age_cat.png",
#   plot = gg_age_cat$plot,
#   width = 15,
#   height = 10,
#   units = "cm",
#   dpi = 300
# )