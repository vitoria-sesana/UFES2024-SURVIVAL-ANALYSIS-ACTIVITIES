# bibliotecas -------------------------------------------------------------
require(survival)
require(survminer)
require(survivaltime)
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

