# 0.0 CARREGA PACOTES ----

library(tidyverse)
library(readxl)
library(janitor)
library(haven)
library(DBI)
library(bigrquery)
library(labelled)

# 1.0 CARREGA DADOS ----

bar_completo <- readRDS("./data/barometer/barometer_completo.rds")

# 2.0 ANALISA DADOS ----

# 2.1 Confianca interpessoal por regiao e escolaridade ----

# 2.1.1 Escolaridade ----

tab_confianca_edu <- bar_completo %>% 
      mutate(pais = case_when(str_detect(pais, "Argentina") ~ "Argentina",
                              str_detect(pais, c("Brasil")) ~ "Brazil",
                              str_detect(pais, "Chile") ~ "Chile",
                              str_detect(pais, "Colombia") ~ "Colombia",
                              str_detect(pais, "Mexico") ~ "Mexico",
                              TRUE ~ as.character(pais))) %>% 
      filter(!ano == "1995") %>% 
      select(ano, pais, confianca_pessoal, educacao, wt) %>% 
      group_by(ano, pais, educacao) %>% 
      count(confianca_pessoal, wt = wt) %>% 
      mutate(prop = n/sum(n),
             prop = round(100*prop, 1))

tab_confianca_edu %>% 
      select(-n) %>% 
      data.frame %>% 
      xlsx::write.xlsx(., "./tabs/Tabela - Latinobarometro - Cruzamentos.xlsx", 
                       append = T, 
                       row.names = F, 
                       sheetName = "raw_trust_edu")

tab_confianca_edu %>% 
      select(-n) %>% 
      unite(ano_confianca, ano, confianca_pessoal) %>% 
      spread(ano_confianca, prop) %>% 
      ungroup() %>%
      data.frame %>% 
      xlsx::write.xlsx(., "./tabs/Tabela - Latinobarometro - Cruzamentos.xlsx", append = T, row.names = F, sheetName = "raw3")
      
# 2.1.2 Regiao ----
      
tab_confianca_regiao <- bar_completo %>% 
      mutate(pais = case_when(str_detect(pais, "Argentina") ~ "Argentina",
                              str_detect(pais, c("Brasil")) ~ "Brazil",
                              str_detect(pais, "Chile") ~ "Chile",
                              str_detect(pais, "Colombia") ~ "Colombia",
                              str_detect(pais, "Mexico") ~ "Mexico",
                              TRUE ~ as.character(pais))) %>% 
      filter(!ano %in% c("1995", "1996", "1997"),
          pais == "Brazil") %>% 
      mutate(regiao = case_when(str_detect(regiao, "Norte") ~ "Norte",
                             str_detect(regiao, "Nordeste") ~ "Nordeste",
                             str_detect(regiao, "Sudeste") ~ "Sudeste",
                             str_detect(regiao, "Sul") | str_detect(regiao, "Sur") ~ "Sul",
                             str_detect(regiao, "Centro-Oeste") ~ "Centro-Oeste",
                             TRUE ~ "Não informado"
   )) %>% 
   select(ano, pais, confianca_pessoal, regiao, wt) %>% 
   group_by(ano, pais, regiao) %>% 
   count(confianca_pessoal, wt = wt) %>% 
   mutate(prop = n/sum(n),
          prop = round(100*prop, 1))

tab_confianca_regiao %>% 
      data.frame %>% 
      xlsx::write.xlsx(., "./tabs/Tabela - Latinobarometro - Cruzamentos.xlsx", 
                       row.names = F, 
                       append = T, 
                       sheetName = "raw_trust_region")

tab_confianca_regiao %>% 
      select(-n) %>% 
      unite(ano_confianca, ano, confianca_pessoal) %>% 
      spread(ano_confianca, prop) %>% 
      ungroup() %>%
      data.frame %>% 
      xlsx::write.xlsx(., "./tabs/Tabela - Latinobarometro - Cruzamentos.xlsx", row.names = F, append = T, sheetName = "CONFIANCA_INTERPES_REGIAO")


# 2.2 Corrupcao maior problema por regiao e escolaridade ----

# 2.2.1 Escolaridade ----

bar_completo %>% 
      mutate(pais = case_when(str_detect(pais, "Argentina") ~ "Argentina",
                              str_detect(pais, c("Brasil")) ~ "Brazil",
                              str_detect(pais, "Chile") ~ "Chile",
                              str_detect(pais, "Colombia") ~ "Colombia",
                              str_detect(pais, "Mexico") ~ "Mexico",
                              TRUE ~ as.character(pais))) %>% 
      select(ano, pais, problema, educacao, wt) %>% 
      group_by(ano, pais, educacao) %>% 
      count(problema, wt = wt) %>% 
      mutate(prop = n/sum(n),
             prop = round(100*prop, 1)) %>% 
      select(-n) %>% 
      filter(problema %in% c("Corruption", "Corrupcion")) %>% 
      # unite(ano_confianca, ano, problema) %>%
      # spread(ano_confianca, prop) %>% 
      # ungroup() %>%
      data.frame %>% 
      xlsx::write.xlsx(., "./tabs/Tabela - Latinobarometro - Cruzamentos.xlsx", 
                       append = T, 
                       row.names = F, 
                       sheetName = "raw_corruption_edu")

bar_completo %>% 
      mutate(pais = case_when(str_detect(pais, "Argentina") ~ "Argentina",
                              str_detect(pais, c("Brasil")) ~ "Brazil",
                              str_detect(pais, "Chile") ~ "Chile",
                              str_detect(pais, "Colombia") ~ "Colombia",
                              str_detect(pais, "Mexico") ~ "Mexico",
                              TRUE ~ as.character(pais))) %>% 
      select(ano, pais, problema, educacao, wt) %>% 
      group_by(ano, pais, educacao) %>% 
      count(problema, wt = wt) %>% 
      mutate(prop = n/sum(n),
             prop = round(100*prop, 1)) %>% 
      select(-n) %>% 
      filter(problema %in% c("Corruption", "Corrupcion")) %>% 
      unite(ano_confianca, ano, problema) %>%
      spread(ano_confianca, prop) %>% 
      ungroup() %>%
      data.frame %>% 
      xlsx::write.xlsx(., "./tabs/Tabela - Latinobarometro - Cruzamentos.xlsx", append = T, row.names = F, sheetName = "CORRUPCAO_EDUCACAO")

# 2.2.2 Regiao ----

bar_completo %>% 
      mutate(pais = case_when(str_detect(pais, "Argentina") ~ "Argentina",
                              str_detect(pais, c("Brasil")) ~ "Brazil",
                              str_detect(pais, "Chile") ~ "Chile",
                              str_detect(pais, "Colombia") ~ "Colombia",
                              str_detect(pais, "Mexico") ~ "Mexico",
                              TRUE ~ as.character(pais))) %>% 
      filter(pais == "Brazil") %>% 
      mutate(regiao = case_when(str_detect(regiao, "Norte") ~ "Norte",
                                str_detect(regiao, "Nordeste") ~ "Nordeste",
                                str_detect(regiao, "Sudeste") ~ "Sudeste",
                                str_detect(regiao, "Sul") | str_detect(regiao, "Sur") ~ "Sul",
                                str_detect(regiao, "Centro-Oeste") ~ "Centro-Oeste",
                                TRUE ~ "Não informado")) %>% 
      select(ano, pais, problema, regiao, wt) %>% 
      group_by(ano, pais, regiao) %>% 
      count(problema, wt = wt) %>% 
      mutate(prop = n/sum(n),
             prop = round(100*prop, 1)) %>% 
      select(-n) %>%
      filter(problema %in% c("Corruption", "Corrupcion")) %>%
      # unite(ano_confianca, ano, problema) %>%
      # spread(ano_confianca, prop) %>% 
      ungroup() %>%
      data.frame %>% 
      xlsx::write.xlsx(., "./tabs/Tabela - Latinobarometro - Cruzamentos.xlsx", 
                       append = T, 
                       row.names = F, 
                       sheetName = "raw_corruption_region")

bar_completo %>% 
      mutate(pais = case_when(str_detect(pais, "Argentina") ~ "Argentina",
                              str_detect(pais, c("Brasil")) ~ "Brazil",
                              str_detect(pais, "Chile") ~ "Chile",
                              str_detect(pais, "Colombia") ~ "Colombia",
                              str_detect(pais, "Mexico") ~ "Mexico",
                              TRUE ~ as.character(pais))) %>% 
      filter(pais == "Brazil") %>% 
      mutate(regiao = case_when(str_detect(regiao, "Norte") ~ "Norte",
                                str_detect(regiao, "Nordeste") ~ "Nordeste",
                                str_detect(regiao, "Sudeste") ~ "Sudeste",
                                str_detect(regiao, "Sul") | str_detect(regiao, "Sur") ~ "Sul",
                                str_detect(regiao, "Centro-Oeste") ~ "Centro-Oeste",
                                TRUE ~ "Não informado")) %>% 
      select(ano, pais, problema, regiao, wt) %>% 
      group_by(ano, pais, regiao) %>% 
      count(problema, wt = wt) %>% 
      mutate(prop = n/sum(n),
             prop = round(100*prop, 1)) %>% 
      select(-n) %>% 
      filter(problema %in% c("Corruption", "Corrupcion")) %>% 
      unite(ano_confianca, ano, problema) %>%
      spread(ano_confianca, prop) %>% 
      ungroup() %>%
      data.frame %>% 
      xlsx::write.xlsx(., "./tabs/Tabela - Latinobarometro - Cruzamentos.xlsx", append = T, row.names = F, sheetName = "CORRUPCAO_REGIAO")

# 2.3 Correlacao entre confianca na policia e no judiciario ----

bar_completo %>% 
      select(ano, pais, confianca_policia, confianca_judiciario, wt) %>% 
      filter(!is.na(confianca_policia), !is.na(confianca_judiciario))
      









