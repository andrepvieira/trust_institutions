
#' Confianca interpessoal -- variação regional, por renda e por escolaridade
#' Confianca interpessoal vs Confianca no judiciario -- 1995-2018, todos os paises
#' Confianca no judiciario vs Confianca na policia
#' Corrupcao maior problema -- escolaridade, renda e regiao (BR)
#' Politicos envolvidos em corrupcao -- tabular resultados gerais

# 0.0 CARREGA PACOTES ----

library(tidyverse)
library(readxl)
library(janitor)
library(haven)
library(DBI)
library(bigrquery)
library(labelled)

# bq_auth(path = "D:/trust_institutions/projeto-dados-313423-183ad33369b0.json")

# 1.0 CARREGA DADOS ----

# 1.1 Teste com basedosdados ----

# projeto-dados-313423

# con <- dbConnect(
#       bigrquery::bigquery(),
#       billing = "projeto-dados-313423",
#       project = "basedosdados"
#       )
# 
# query <- "SELECT * FROM basedosdados.latinobarometro LIMIT 100"
# 
# df.latin <- dbGetQuery(con, query)
# 
# query = "SELECT * FROM `basedosdados.br_ms_sim.municipio`"
# 
# df.sim = dbGetQuery(con, query)

# 1.2 Carrega dta ----

bar_95 <- haven::read_dta("./data/barometer/Latinobarometro_1995.dta")
bar_96 <- haven::read_dta("./data/barometer/Latinobarometro_1996.dta")
bar_97 <- haven::read_dta("./data/barometer/Latinobarometro_1997.dta")
bar_98 <- haven::read_dta("./data/barometer/Latinobarometro_1998.dta")

bar_00 <- haven::read_dta("./data/barometer/Latinobarometro_2000.dta")
bar_01 <- haven::read_dta("./data/barometer/Latinobarometro_2001.dta")
bar_02 <- haven::read_dta("./data/barometer/Latinobarometro_2002.dta")
bar_03 <- haven::read_dta("./data/barometer/Latinobarometro_2003.dta")
bar_04 <- haven::read_dta("./data/barometer/Latinobarometro_2004.dta")
bar_05 <- haven::read_dta("./data/barometer/Latinobarometro_2005.dta")
bar_06 <- haven::read_dta("./data/barometer/Latinobarometro_2006.dta")
bar_07 <- haven::read_dta("./data/barometer/Latinobarometro_2007.dta")
bar_08 <- haven::read_dta("./data/barometer/Latinobarometro_2008.dta")
bar_09 <- haven::read_dta("./data/barometer/Latinobarometro_2009.dta")
bar_10 <- haven::read_dta("./data/barometer/Latinobarometro_2010.dta")

bar_11 <- haven::read_dta("./data/barometer/Latinobarometro_2011.dta")
bar_13 <- haven::read_dta("./data/barometer/Latinobarometro_2013.dta")
bar_15 <- haven::read_dta("./data/barometer/Latinobarometro_2015.dta")
bar_16 <- haven::read_dta("./data/barometer/Latinobarometro_2016.dta")
bar_17 <- haven::read_dta("./data/barometer/Latinobarometro_2017.dta")
bar_18 <- haven::read_dta("./data/barometer/Latinobarometro_2018.dta")

# 2.0 SELECIONA COLUNAS ----

source("./script/functions.R", local = TRUE)

bar_95 <- limpa_latinobarometro(bar_95)
bar_96 <- limpa_latinobarometro(bar_96)
bar_97 <- limpa_latinobarometro(bar_97)
bar_98 <- limpa_latinobarometro(bar_98)

bar_00 <- limpa_latinobarometro(bar_00)
bar_01 <- limpa_latinobarometro(bar_01)
bar_02 <- limpa_latinobarometro(bar_02)
bar_03 <- limpa_latinobarometro(bar_03)
bar_04 <- limpa_latinobarometro(bar_04)
bar_05 <- limpa_latinobarometro(bar_05)
bar_06 <- limpa_latinobarometro(bar_06)
bar_07 <- limpa_latinobarometro(bar_07)
bar_08 <- limpa_latinobarometro(bar_08)
bar_09 <- limpa_latinobarometro(bar_09)
bar_10 <- limpa_latinobarometro(bar_10)

bar_11 <- limpa_latinobarometro(bar_11)
bar_13 <- limpa_latinobarometro(bar_13)
bar_15 <- limpa_latinobarometro(bar_15)
bar_16 <- limpa_latinobarometro(bar_16)
bar_17 <- limpa_latinobarometro(bar_17)
bar_18 <- limpa_latinobarometro(bar_18)

# 3.0 EMPILHA DADOS ----

bar_completo <- mget(ls(pattern="^bar")) %>%
      bind_rows()

# 4.0 LIMPA DADOS ----

bar_completo <- bar_completo %>% 
      mutate(
            confianca_pessoal = case_when(str_detect(confianca_pessoal, "know") | 
                                                str_detect(confianca_pessoal, "answer") | 
                                                str_detect(confianca_pessoal, "sabe") ~ "Don´t know/No answer",
                                          str_detect(confianca_pessoal, "puede") ~ "Most people can be trusted",
                                          str_detect(confianca_pessoal, "nunca") | 
                                                str_detect(confianca_pessoal, "can never") ~ "You can never be too careful when dealing with others",
                                          str_detect(confianca_pessoal, "puede confiar") | 
                                                str_detect(confianca_pessoal, "trust most") ~ "Most people can be trusted",
                                          TRUE ~ as.character(confianca_pessoal)
                                          )) %>% 
      mutate(confianca_judiciario = case_when(str_detect(confianca_judiciario, "No sabe") | 
                                                    str_detect(confianca_judiciario, "No responde") |
                                                    str_detect(confianca_judiciario, "know") | 
                                                    str_detect(confianca_judiciario, "No answer") ~ "No answer/Refused",
                                              str_detect(confianca_judiciario, "Mucha") | 
                                                    str_detect(confianca_judiciario, "Lot") ~ "A lot of confidence",
                                              str_detect(confianca_judiciario, "Ninguna") | 
                                                    str_detect(confianca_judiciario, "No trust")~ "No confidence at all",
                                              str_detect(confianca_judiciario, "Poca") | 
                                                    str_detect(confianca_judiciario, "little") ~ "Little confidence",
                                              str_detect(confianca_judiciario, "Algo") | 
                                                    str_detect(confianca_judiciario, "Some")
                                                    ~ "Some confidence",
                                              TRUE ~ as.character(confianca_judiciario))) %>% 
      mutate(confianca_policia = case_when(str_detect(confianca_policia, "No sabe") | 
                                                    str_detect(confianca_policia, "No responde") |
                                                    str_detect(confianca_policia, "know") | 
                                                    str_detect(confianca_policia, "No answer") ~ "No answer/Refused",
                                              str_detect(confianca_policia, "Mucha") | 
                                                    str_detect(confianca_policia, "Lot") ~ "A lot of confidence",
                                              str_detect(confianca_policia, "Ninguna") | 
                                                    str_detect(confianca_policia, "No trust") | 
                                                 str_detect(confianca_policia, "Nothing")~ "No confidence at all",
                                              str_detect(confianca_policia, "Poca") | 
                                                    str_detect(confianca_policia, "little") | 
                                                 str_detect(confianca_policia, "Little")~ "Little confidence",
                                              str_detect(confianca_policia, "Algo") | 
                                                    str_detect(confianca_policia, "Some")
                                              ~ "Some confidence",
                                              TRUE ~ as.character(confianca_policia))) %>%
      mutate(educacao = case_when(str_detect(educacao, "answer") |
                                     str_detect(educacao, "-5") |
                                        str_detect(educacao, "applicable") ~ "No answer/Refused",
                                  str_detect(educacao, "Basica y menos") |
                                     str_detect(educacao, "Illiterate") | 
                                     str_detect(educacao, "Incomplete primary") | 
                                     str_detect(educacao, "Primary incomplete") | 
                                     str_detect(educacao, "Without education") | 
                                     educacao %in% c("1 year", "2 years", "3 years", "4 years", 
                                                     "5 years", "6 years", "7 years", "8 years")
                                     ~ "Without education or less than primary",
                                  str_detect(educacao, "Primary") | 
                                     str_detect(educacao, "Complete primary") | 
                                     str_detect(educacao, "Higher incomplete") | 
                                     str_detect(educacao, "Incomplete Secondary") | 
                                     str_detect(educacao, "High school/academies/Incomplete") | 
                                     str_detect(educacao, "Secundary, intermediate, vocational incomplete") | 
                                     str_detect(educacao, "Secundaria, media, tecnica y menos") | 
                                     educacao %in% c("9 years", "10 years", "11 years")
                                     ~ "Primary complete or secondary incomplete",
                                  str_detect(educacao, "Complete Secondary") |
                                     str_detect(educacao, "12 years") | 
                                     str_detect(educacao, "Secundary, intermediate, vocational complete") | 
                                     str_detect(educacao, "High school/academies/Complete ") ~ "Secondary complete",
                                  str_detect(educacao, "university") | 
                                        str_detect(educacao, "Higher") | 
                                        str_detect(educacao, "Complete high") | 
                                        str_detect(educacao, "Incomplete high") | 
                                     str_detect(educacao, "Superior") ~ "Some college or higher education",
                                  TRUE ~ as.character(educacao)))
            

# 5.0 SALVA DADOS ----

bar_completo %>%
      data.frame %>%
      write.csv2(., "./data/barometer/barometer_completo.csv", row.names = F)

bar_completo %>%
      saveRDS(., "./data/barometer/barometer_completo.rds")


# bar_18$NUMINVES[1] == 2018
# rm(bar_11, bar_13, bar_15, bar_16, bar_17, bar_18)

























# Nao é um problema de inconsistencia, mas de tratamento detalhado
# 
# Concluintes: 
#    -- as coletas sao feitas em momentos diferentes
#       enade 17 - agosto 17
#       censo 17 - coleta no ano seguinte
# 
#    inscricao de maneira equivocada
#    nao conseguiu terminar
#    
#    tempo medio de conclusao de curso do estudante 
#       -- censo lancado mais de uma vez como formado -- erro de preenchimento
#          correcao pela ies no ano seguinte, lancamento como formado novamente
#          estamos usando o ano mais recente em que ele aparece como formado
#          
#    enade é a fonte menos segura pra definir se o cara é concluinte
#    
#    o cara aparece no censo como formado em ano anterior de ser inscrito como ser concluinte no enade
#    esses a gente tira do estudo
#    
#    tecnologia -- censo e enade tem que ser no mesmo ano -- concluinte é % de integralizado ou conclusao do curso no semestre de referencia
#    
#    se o resultado do enade é importante, tem que pegar os que realmente se caracterizam como concluinte no enade e no censo
#    
#    a gente consegue alguma tabulacao dessas inconsistencias?
#        nao temos pessoal pra fazer isso
#        fazemos essas analises de inconsistencias apenas para estudos especificos
#    
#    tempo de integralizacao dos cursos/conclusao do censo é problematica no censo
   



# submeter a proposta ao inep -- sala de sigilo
   # isso vai deixar o estudo mais redondo
   # muda inclusive correlacoes em estudos, como aconteceu no nosso

# medicina:
#    tivemos forte pressao para indicador de egresso (e.g. empregabilidade), mas nao é um indicador de
#    qualidade do curso -- tem varios fatores que interfeem -- mercado, origem, capital












