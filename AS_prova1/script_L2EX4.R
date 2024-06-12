# ANÁLISE DE SOBREVIVÊNCIA
# EXERCiCIO 4 DA LISTA 2

# bibliotecas -------------------------------------------------------------
require(survival)
require(survminer)
require(summarytools)
require(dplyr)

# base de dados -----------------------------------------------------------
base <- readxl::read_xlsx("AS_lista2/Lista2_Hodgkins.xlsx") %>% 
  mutate(aux_age = case_when(
    (age > 53) ~ "53 anos ou mais",
    (age >= 38 & age < 53) ~ "38 anos até menos 53 anos",
    (age >= 25 & age < 38) ~ "25 anos até menos 38 anos",
    (age < 25) ~ "menos de 25 anos", 
  ),
  aux_dead = case_when(
    dead == 0 ~ "Evento não observado",
    dead == 1 ~ "Evento observado"
  ),
  aux_sex = case_when(
    sex == 0 ~ "Feminino",
    sex == 1 ~ "Masculino"
  ),
  aux_stage = case_when(
    stage == 0 ~ "Inicial",
    stage == 1 ~ "Avançado"
  ),
  aux_hist = case_when(
    hist == 1 ~ "Esclerose nodular",
    hist == 2 ~ "Misto celular",
    hist == 3 ~ "Fepleção de linfócitos"
  ),
  aux_age = factor(aux_age, labels = c("53 anos ou mais",
                       "38 anos até menos 53 anos",
                       "25 anos até menos 38 anos", 
                       "menos de 25 anos"))
  )

# analise descritiva ------------------------------------------------------
ctable(base$aux_sex, y=base$aux_dead, prop="t")
ctable(base$aux_age, y=base$aux_dead, prop="t")
ctable(base$aux_hist, y=base$aux_dead, prop="t")
ctable(base$aux_stage, y=base$aux_dead, prop="t")

ggplot(base) +
  aes(x = survivaltime, y = aux_dead) +
  geom_boxplot(fill = "#0c4c8a") +
  labs(x = "Tempo de vida dos pacientes", y = "Gênero") +
  theme_minimal()

# descritiva survivaltime -------------------------------------------------

min(base$survivaltime) # minimo em meses
min(base$survivaltime)*30 # minimo em dias
max(base$survivaltime) # máximo em meses
max(base$survivaltime)/12 #máximo em dias

boxplot(base$survivaltime, horizontal = TRUE)

descr(base$survivaltime, stats = c("min", "mean", "med","sd","max"),
      transpose = TRUE)

# tabela observações e censuras -------------------------------------------
Surv(time = base$survivaltime, event = base$dead) 

# genero ------------------------------------------------------------------
ajuste_sexo <- 
  survfit(
    Surv(time = survivaltime, 
         event = dead) ~ aux_sex, 
    data = base
    )

ajuste_sexo <- survfit(Surv(time = survivaltime, 
                            event = dead) ~ sex, 
                       data = base)

#Para fazer a tabela da estimativa de KM e intervalo de confiança:
surv_summary(ajuste_sexo, data = lung)

# estimativas sobrevida por kaplan-meier
kp_sexo <- surv_summary(ajuste_sexo, 
                        data = base)

# gráfico sobreviva estimada
gg_sexo <- survminer::ggsurvplot(ajuste_sexo, data = base,
                      pval = FALSE, conf.int=TRUE, conf.int.style = "ribbon",
                      ylab = "Sobrevida estimada", 
                      xlab = "Tempo em meses", 
                      legend.title = "", 
                      legend.labs = c("Feminino","Masculino"))

gg_sexo$plot <- gg_sexo$plot + 
  scale_y_continuous(limits = c(0, 1), 
                     breaks = seq(0, 1, by = 0.1)) +
  scale_x_continuous(limits = c(0, max(base$survivaltime)),
                     breaks = seq(0, max(base$survivaltime), by = 10)) +
  theme(legend.position = "bottom")

# teste log-rank
test_sexo <-survdiff(Surv(time = survivaltime, 
                          event = dead) ~ sex, 
                     data = base) 

# faixa etaria ------------------------------------------------------------
ajuste_age <- survfit(
  Surv(time = survivaltime, 
       event = dead,
       type = "right", 
       origin = 4) ~ aux_age,
  data = base)

# estimativas sobrevida por kaplan-meier
kp_age <- surv_summary(ajuste_age, data = base)

# gráfico sobreviva estimada
gg_age <- survminer::ggsurvplot(ajuste_age, data = base,
                                 pval = FALSE, conf.int=TRUE, 
                                conf.int.style = "step",
                                 ylab = "Sobrevida estimada", 
                                 xlab = "Tempo em meses", 
                                 legend.title = "",
                                 legend.labs =  c("53 anos ou mais",
                                                  "38 anos até menos 53 anos",
                                                  "25 anos até menos 38 anos", 
                                                  "menos de 25 anos"))  

gg_age$plot <- gg_age$plot +
  ggplot2::scale_y_continuous(limits = c(0, 1), 
                              breaks = seq(0, 1, by = 0.1)) +
  ggplot2::scale_x_continuous(limits = c(0, max(base$survivaltime)),
                              breaks = seq(0, max(base$survivaltime), by = 10)) +
  theme(legend.position = "bottom") +
  scale_fill_discrete(labels=c('High Program', 'Low Program', 'c', 'd'))


# teste log-rank
test_age <- survdiff(Surv(time = survivaltime, 
                            event = dead) ~ aux_age,
                       data = base) 


# histologia --------------------------------------------------------------
ajuste_hist <- survfit(Surv(time = survivaltime, 
                            event = dead) ~ hist, 
                       data = base)

# estimativas sobrevida por kaplan-meier
kp_hist <- surv_summary(ajuste_hist, data = base)

# gráfico sobreviva estimada
gg_hist <- survminer::ggsurvplot(ajuste_hist, 
                                 data = base,
                                 pval = FALSE, 
                                 conf.int=TRUE, 
                                 conf.int.style = "step",
                                 ylab = "Sobrevida estimada", 
                                 xlab = "Tempo em meses", 
                                 legend.title = "",
                                 legend.labs = c("Esclerose Nodular", 
                                                 "Misto Celular",
                                                 "Depleção de Linfócitos"))

gg_hist$plot 

gg_hist$plot <- gg_hist$plot + 
  scale_y_continuous(limits = c(0, 1), 
                     breaks = seq(0, 1, by = 0.1)) +
  scale_x_continuous(limits = c(0, max(base$survivaltime)),
                     breaks = seq(0, max(base$survivaltime), by = 10)) +
  theme(legend.position = "bottom")

# teste log-rank
test_hist <- survdiff(Surv(time = survivaltime, 
                           event = dead) ~ hist, 
                      data = base) 

# estagio da doença -------------------------------------------------------
ajuste_stage <- survfit(Surv(time = survivaltime, 
                             event = dead) ~ stage, 
                       data = base)

# estimativas sobrevida por kaplan-meier
kp_stage <- surv_summary(ajuste_stage, data = base)

# gráfico sobreviva estimada
gg_stage <- survminer::ggsurvplot(ajuste_stage, 
                                   data = base,
                                   pval = FALSE, 
                                   conf.int=TRUE, 
                                   conf.int.style = "ribbon",
                                   ylab = "Sobrevida estimada", 
                                   xlab = "Tempo em meses", 
                                   legend.title = "",
                                   legend.labs = c("Inicial", 
                                                   "Avançado"))

gg_stage$plot <- gg_stage$plot + 
  scale_y_continuous(limits = c(0, 1), 
                     breaks = seq(0, 1, by = 0.1)) +
  scale_x_continuous(limits = c(0, max(base$survivaltime)),
                     breaks = seq(0, max(base$survivaltime), by = 10)) +
  theme(legend.position = "bottom")

# teste log-rank
test_stage <- survdiff(Surv(time = survivaltime, 
                            event = dead) ~ stage, 
                       data = base) 
