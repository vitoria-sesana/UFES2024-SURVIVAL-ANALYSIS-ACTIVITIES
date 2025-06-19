
# genero
matrix(
  c(
    test_log_sexo$chisq,
    as.integer(length(test_log_sexo$n) - 1), 
    test_log_sexo$pvalue,
    test_wilcox_sexo$chisq,
    as.integer(length(test_wilcox_sexo$n) - 1), 
    test_wilcox_sexo$pvalue
    ), 
  ncol = 3, 
  byrow = TRUE
  ) %>% 
  as.data.frame() %>% 
  xtable::xtable() 


# estagio
matrix(
  c(
    test_log_stage$chisq,
    as.integer(length(test_log_stage$n) - 1), 
    test_log_stage$pvalue,
    test_wilcox_stage$chisq,
    as.integer(length(test_wilcox_stage$n) - 1), 
    test_wilcox_stage$pvalue
  ), 
  ncol = 3, 
  byrow = TRUE
) %>% 
  as.data.frame() %>% 
  xtable::xtable()


# idade cat pc
matrix(
  c(
    test_log_age_pc$chisq,
    as.integer(length(test_log_age_pc$n) - 1), 
    test_log_age_pc$pvalue,
    test_wilcox_age_pc$chisq,
    as.integer(length(test_wilcox_age_pc$n) - 1), 
    test_wilcox_age_pc$pvalue
  ), 
  ncol = 3, 
  byrow = TRUE
) %>% 
  as.data.frame() %>% 
  xtable::xtable()


# histologia - log rank

matrix(
  c(
    "Esclerose nodular x Fepleção de linfócitos",
    test_log_hist1$chisq %>% round(3),
    as.integer(length(test_log_hist1$n) - 1) %>% round(3), 
    test_log_hist1$pvalue %>% round(3), 
    
    "Esclerose nodular x Misto celular",
    test_log_hist2$chisq %>% round(3), 
    as.integer(length(test_log_hist2$n) - 1) %>% round(3),  
    test_log_hist2$pvalue %>% round(3), 
    
    "Fepleção de linfócitos x Misto celular",
    test_log_hist3$chisq %>% round(3), 
    as.integer(length(test_log_hist3$n) - 1) %>% round(3),  
    test_log_hist3$pvalue %>% round(3), 
    
    "Esclerose nodular x Fepleção de linfócitos",
    test_wilcox_hist1$chisq %>% round(3), 
    as.integer(length(test_wilcox_hist1$n) - 1) %>% round(3),  
    test_wilcox_hist1$pvalue %>% round(3), 
    
    "Esclerose nodular x Misto celular",
    test_wilcox_hist2$chisq %>% round(3), 
    as.integer(length(test_wilcox_hist2$n) - 1) %>% round(3),  
    test_wilcox_hist2$pvalue %>% round(3), 
    
    "Fepleção de linfócitos x Misto celular",
    test_wilcox_hist3$chisq %>% round(3), 
    as.integer(length(test_wilcox_hist3$n) - 1) %>% round(3),  
    test_wilcox_hist3$pvalue  %>% round(3)
    
  ), 
  ncol = 4, 
  byrow = TRUE
) %>% 
  as.data.frame() %>% 
  xtable::xtable()



# idade -------------------------------------------------------------------



matrix(
  c(
    "< 25 - 25 a 38 anos",
    test_log_age_cat1$chisq %>% round(3),
    as.integer(length(test_log_age_cat1$n) - 1) %>% round(3), 
    test_log_age_cat1$pvalue %>% round(3), 
    
    "< 25 - 38 a 53 anos",
    test_log_age_cat2$chisq %>% round(3), 
    as.integer(length(test_log_age_cat2$n) - 1) %>% round(3),  
    test_log_age_cat2$pvalue %>% round(3), 
    
    "< 25 - >= 53 anos",
    test_log_age_cat3$chisq %>% round(3), 
    as.integer(length(test_log_age_cat3$n) - 1) %>% round(3),  
    test_log_age_cat3$pvalue %>% round(3), 
    
    "25 a 38 anos - 38 a 53 anos",
    test_log_age_cat4$chisq %>% round(3),
    as.integer(length(test_log_age_cat4$n) - 1) %>% round(3), 
    test_log_age_cat4$pvalue %>% round(3), 
    
    "25 a 38 anos - >= 53 anos",
    test_log_age_cat5$chisq %>% round(3), 
    as.integer(length(test_log_age_cat5$n) - 1) %>% round(3),  
    test_log_age_cat5$pvalue %>% round(3), 
    
    "38 a 53 anos - >= 53 anos",
    test_log_age_cat6$chisq %>% round(3), 
    as.integer(length(test_log_age_cat6$n) - 1) %>% round(3),  
    test_log_age_cat6$pvalue %>% round(3), 
    
    
    "< 25 - 25 a 38 anos",
    test_wilcox_age_cat1$chisq %>% round(3), 
    as.integer(length(test_wilcox_age_cat1$n) - 1) %>% round(3),  
    test_wilcox_age_cat1$pvalue %>% round(3), 
    
    "< 25 - 38 a 53 anos",
    test_wilcox_age_cat2$chisq %>% round(3), 
    as.integer(length(test_wilcox_age_cat2$n) - 1) %>% round(3),  
    test_wilcox_age_cat2$pvalue %>% round(3), 
    
    "< 25 - >= 53 anos",
    test_wilcox_age_cat3$chisq %>% round(3), 
    as.integer(length(test_wilcox_age_cat3$n) - 1) %>% round(3),  
    test_wilcox_age_cat3$pvalue  %>% round(3),
    
    "25 a 38 anos - 38 a 53 anos",
    test_wilcox_age_cat4$chisq %>% round(3), 
    as.integer(length(test_wilcox_age_cat4$n) - 1) %>% round(3),  
    test_wilcox_age_cat4$pvalue %>% round(3), 
    
    "25 a 38 anos - >= 53 anos",
    test_wilcox_age_cat5$chisq %>% round(3), 
    as.integer(length(test_wilcox_age_cat5$n) - 1) %>% round(3),  
    test_wilcox_age_cat5$pvalue %>% round(3), 
    
    "38 a 53 anos - >= 53 anos",
    test_wilcox_age_cat6$chisq %>% round(3), 
    as.integer(length(test_wilcox_age_cat6$n) - 1) %>% round(3),  
    test_wilcox_age_cat6$pvalue  %>% round(3)
    
  ), 
  ncol = 4, 
  byrow = TRUE
) %>% 
  as.data.frame() %>% 
  xtable::xtable()


