# kaplan meier ------------------------------------------------------------
model_np_kaplan_meier <- # KAPLAN - MEIER
  survfit(
    Surv(time = temp_falha, 
         event = ind_falha) ~ 1, 
    data = base
  )


# exponential -------------------------------------------------------------
model_exponential <- 
  flexsurvreg(
    Surv(
      time = temp_falha,
      event = ind_falha
    ) ~ 1,
    data = base,  
    dist = "exponential"
  )

# weibull -----------------------------------------------------------------
model_weibull <-
  flexsurvreg(
    Surv(
      time = temp_falha,
      event = ind_falha
    ) ~ 1,
    data = base,  
    dist = "weibull"
  )


# log normal --------------------------------------------------------------
model_logNorm <- 
  flexsurvreg(
    Surv(
      time = temp_falha,
      event = ind_falha
    ) ~ 1,
    data = base,  
    dist = "lognorm"
  )

 
# log logistic ------------------------------------------------------------
model_logLogistic <- 
  flexsurvreg(
    Surv(
      time = temp_falha,
      event = ind_falha
    ) ~ 1,
    data = base,  
    dist = "llogis"
  )

