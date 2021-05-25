
# notes pre-cleaning ----

# data to be used in the analysis of trust in institutions and interpersonal trust:

# a. WSV
# b. Barometer of Americas
# c. Varieties of Democracy

# variables of interest in the WSV data:

# V24 
# V25:V35 - group orgs in three types (V28:V29, V26:V32, V25)
# V56
# V96:V126b
# V131, V133, V137, V138, V140, V141
# V212:V216
# V238:V239
# V248

# V108: V126b: Para essa questão, assim para as questões sobre confiança nas pessoas, 
# seria muito importante ter uma série temporal. A maior possível. 

# Sobre as escalas de 10 pontos acho que podem ser agregadas em 5. 
# Ja a confiança pode manter separadas.
# Também as classes devem estar separadas. 
# A escolaridade que pode ser agregada em no máximo 5 grupos 
# (valendo-se de sua experiência no tema).

# TIRAR N DAS TABELAS FINAIS
# CRUZAR STATUS COM DEMAIS VARIAVEIS APENAS PARA BRASIL
# TABELAR REGIAO E CRUZAR COM CONFIANCA

# ESCREVER EMAIL LISTANDO VARIAVEIS IMPORTANTES
# LER ARTIGO DE 

# clean up workspace ----

rm(list=ls())

# close all figure windows created ----

graphics.off()

# load packages ----

library(tidyverse)
library(janitor)
library(arrow)
library(js)
library(here)
library(rlang)

# load raw data ----

wvs <- readr::read_rds("./data/wvs/WVS_TimeSeries_R_v1_6.rds")
cnt_code <- clean_names(readxl::read_excel("./data/wvs/F00003844-WVS_Time_Series_List_of_Variables_and_equivalences.xlsx", sheet = 2, skip = 2, n_max = 104))
cnt_wave_code <- clean_names(readxl::read_excel("./data/wvs/F00003844-WVS_Time_Series_List_of_Variables_and_equivalences.xlsx", sheet = 6, skip = 1, n_max = 293))

wvs_7 <- readr::read_rds("./data/wvs/WVS_Cross-National_Wave_7_R_v1_6.rds")

# clean data ----

wvs_clean_br <- wvs %>% 
      rename(year_wave_code = S024, id = S007, cnt_code = S003, wt = S017) %>% 
      left_join(cnt_wave_code, by = c("year_wave_code" = "code")) %>%
      mutate(label = case_when(cnt_code == 620 ~ "Portugal (7)",
                               cnt_code == 804 ~ "Ukraine (7)",
                               TRUE ~ as.character(label))) %>%
      # filter(str_detect(label, paste(c("Brazil", "Sweden"), collapse = "|"))) %>% 
      filter(str_detect(label, paste(c("Brazil"), collapse = "|"))) %>% 
      mutate_at(vars(E069_01:E069_54), function(x) {
            x = case_when(x == 1 ~ "A great deal",
                          x == 2 ~ "Quite a lot",
                          x == 3 ~ "Not very much",
                          x == 4 ~ "None at all")}) %>% 
      dplyr::mutate(label = case_when(label == "Brazil (2)" ~ "1990-1994",
                                      label == "Brazil (3)" ~ "1995-1998",
                                      label == "Brazil (5)" ~ "2005-2009",
                                      label == "Brazil (6)" ~ "2010-2014",
                                      label == "Brazil (7)" ~ "2017-2020"))

cnt_list <- c("Brazil", "Argentina", "Mexico", "Colombia", "Chile")

variables_list <- c(
      # trust
      "A165", 
      "A168A",
      # member
      "A065", "A066", "A067", "A068", "A071", "A072", "A074", "A079", 
      # about opinion
      "E035", "E036", "E037", "E038", "E039", "E040", "E041",
      # about trust in groups
      "D001_B", "G007_17", "G007_18", "G007_33_B", "G007_34_B", "G007_35_B", "G007_36_B",
      # about trust in institutions
      "E069_01", "E069_02", "E069_03", "E069_04", "E069_05", "E069_06", "E069_07", "E069_08",
      "E069_09", "E069_10", "E069_11", "E069_12", "E069_13", "E069_14", "E069_15", "E069_16", "E069_17", 
      "E069_26", "E069_38", "E069_40", "E069_41", "E069_54",
      # about democracy
      "E224", "E226", "E233A", "E233B",
      # about importance of democracy
      "E235",
      # democracy one's own country
      "E236",
      # citizen
      "G019", "G020", "G021", "G022A", "G023",
      # sociodemographic
      "X045", "X047", "X025"
      )

variables_name <- c("year_wave_code",
                    "id",
                    "cnt_code",
                    "wt",
                    
                    "trust_people",
                    "people_take_advantage",
                    
                    "member_religious",
                    "member_education",
                    "member_unions",
                    "member_pol_party",
                    "member_ecology",
                    "member_profession",
                    "member_recreation",
                    "member_other",
                    
                    "opinion_income",
                    "opinion_business",
                    "opinion_gov_resp",
                    "opinion_unemp",
                    "opinion_competition",
                    "opinion_hard_work",
                    "opinion_wealth",
                    
                    "trust_family",
                    "trust_friends",
                    "trust_neighborhood",
                    "trust_people_know",
                    "trust_people_first",
                    "trust_other_religion",
                    "trust_other_nation",
                    
                    "confidence_church",
                    "confidence_forces",
                    "confidence_education",
                    "confidence_press",
                    "confidence_unions",
                    "confidence_police",
                    "confidence_parliament",
                    "confidence_civil",
                    "confidence_social_security",
                    "confidence_tv",
                    "confidence_gov",
                    "confidence_parties",
                    "confidence_companies",
                    "confidence_envir",
                    "confidence_women_mov",
                    "confidence_health",
                    "confidence_justice",
                    "confidence_mercosur",
                    "confidence_presidency",
                    "confidence_charity",
                    "confidence_banks",
                    "confidence_university",
                    
                    "democracy_tax_rich",
                    "democracy_elections",
                    "democracy_equal",
                    "democracy_obey",
                    
                    "democracy_importance",
                    "democracy_own_cnt",
                    
                    "citizen_world",
                    "citizen_local",
                    "citizen_nation",
                    "citizen_latin",
                    "citizen_autonomous",
                    
                    "social_class",
                    "income",
                    "highest_edu",
                    
                    "label",
                    "wave",
                    "cnt"
                    
                    )

fct_case_when <- function(...) {
      args <- as.list(match.call())
      levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
      levels <- levels[!is.na(levels)]
      factor(dplyr::case_when(...), levels=levels)
}

wvs_al_tbl <- wvs %>% 
      select(year_wave_code = S024, 
             id = S007, 
             cnt_code = S003, 
             wt = S017,
             variables_list) %>% 
      # rename(social_class = X045,
      #        highest_edu = X025,
      #        income = X047) %>%
      left_join(cnt_wave_code, by = c("year_wave_code" = "code")) %>%
      mutate(
            # label = fct_case_when(cnt_code == 620 ~ "Portugal (7)",
            #                        cnt_code == 804 ~ "Ukraine (7)",
            #                        TRUE ~ label),
             A165 = fct_case_when(A165 == 1 ~ "Most people can be trusted",
                              A165 == 2 ~ "Neet to be very careful"),
             # A168 = fct_case_when(A168 == 1 ~ "Would take advantage",
             #                   A168 == 2 ~ "Try to be fair"),
             A168A = fct_case_when(A168A %in% c(1, 2) ~ 1,
                               A168A %in% c(3, 4) ~ 2,
                               A168A %in% c(5, 6) ~ 3,
                               A168A %in% c(7, 8) ~ 4,
                               A168A %in% c(9, 10) ~ 5),
             E235 = fct_case_when(E235 %in% c(1, 2) ~ 1,
                              E235 %in% c(3, 4) ~ 2,
                              E235 %in% c(5, 6) ~ 3,
                              E235 %in% c(7, 8) ~ 4,
                              E235 %in% c(9, 10) ~ 5),
             E236 = fct_case_when(E236 %in% c(1, 2) ~ 1,
                              E236 %in% c(3, 4) ~ 2,
                              E236 %in% c(5, 6) ~ 3,
                              E236 %in% c(7, 8) ~ 4,
                              E236 %in% c(9, 10) ~ 5),
             X025 = fct_case_when(X025 == 1 ~ "Inadequately completed elementary education",
                              X025 == 2 ~ "Completed elementary education",
                              X025 %in% c(3, 5) ~ "Incomplete secondary school",
                              X025 %in% c(4, 6) ~ "Complete secondary school",
                              X025 == 7 ~ "Some university",
                              X025 == 8 ~ "University"),
             X047 = fct_case_when(X047 %in% c(1, 2) ~ "Lower",
                              X047 %in% c(3, 4) ~ "Lower upper",
                              X047 %in% c(5, 6) ~ "Middle",
                              X047 %in% c(7, 8) ~ "Upper lower",
                              X047 %in% c(9, 10) ~ "Upper"),
             X045 = fct_case_when(X045 == 1 ~ "Upper class",
                              X045 == 2 ~ "Upper middle class",
                              X045 == 3 ~ "Lower middle class",
                              X045 == 4 ~ "Working class",
                              X045 == 5 ~ "Lower class")
             ) %>%
      # count(label)
      dplyr::filter(str_detect(label, paste(cnt_list, collapse = "|"))) %>% 
      # glimpse
      mutate_at(vars(E069_01:E069_54), function(x) {
            x = fct_case_when(x == 1 ~ "A great deal",
                          x == 2 ~ "Quite a lot",
                          x == 3 ~ "Not very much",
                          x == 4 ~ "None at all")}) %>%
      mutate_at(vars(A065:A079), function(x) {
            x = fct_case_when(x == 0 ~ "Not mentioned",
                          x == 1 ~ "Belong")}) %>%
      mutate_at(vars(E035:E041), function(x) {
            x = fct_case_when(x %in% c(1, 2) ~ 1,
                          x %in% c(3, 4) ~ 2,
                          x %in% c(5, 6) ~ 3,
                          x %in% c(7, 8) ~ 4,
                          x %in% c(9, 10) ~ 5)}) %>% 
      mutate_at(vars(D001_B:G007_36_B), function(x) {
            x = fct_case_when(x == 1 ~ "Trust completely",
                          x == 2 ~ "Trust somewhat",
                          x == 3 ~ "Do not trust very much",
                          x == 4 ~ "Do not trust at all")}) %>% 
      mutate_at(vars(E069_01:E069_54), function(x) {
            x = fct_case_when(x == 1 ~ "A great deal",
                          x == 2 ~ "Quite a lot",
                          x == 3 ~ "Not very much",
                          x == 4 ~ "None at all")}) %>% 
      mutate_at(vars(E224:E233B), function(x) {
            x = fct_case_when(x %in% c(1, 2) ~ 1,
                          x %in% c(3, 4) ~ 2,
                          x %in% c(5, 6) ~ 3,
                          x %in% c(7, 8) ~ 4,
                          x %in% c(9, 10) ~ 5)}) %>%
      mutate_at(vars(G019:G023), function(x) {
            x = fct_case_when(x == 1 ~ "Strongly agree",
                          x == 2 ~ "Agree",
                          x == 3 ~ "Disagree",
                          x == 4 ~ "Strongly disagree")}) %>% 
      mutate(wave = fct_case_when(str_detect(label, "(1)") ~ "1981-1984",
                              str_detect(label, "(2)") ~ "1990-1994",
                              str_detect(label, "(3)") ~ "1995-1998",
                              str_detect(label, "(4)") ~ "1999-2004",
                              str_detect(label, "(5)") ~ "2005-2009",
                              str_detect(label, "(6)") ~ "2010-2014",
                              str_detect(label, "(7)") ~ "2017-2020"
                              ),
             cnt = fct_case_when(str_detect(label, "Arg") ~ "Argentina",
                             str_detect(label, "Bra") ~ "Brazil",
                             str_detect(label, "Chile") ~ "Chile",
                             str_detect(label, "Col") ~ "Colombia",
                             str_detect(label, "Mex") ~ "Mexico")) %>%
      setNames(variables_name)
      
wvs_al_tab <- wvs_al_tbl %>% 
      select(-c(label, year_wave_code, id, cnt_code)) %>% 
      gather(variable, value, -cnt, -wave, -wt) %>% 
      group_by(cnt, wave, variable, value) %>% 
      count(wt = wt) %>% 
      # summarize(n = n()) %>% 
      ungroup() %>% 
      group_by(cnt, wave, variable) %>% 
      mutate(pct = round(100*n/sum(n), 1)) %>% 
      select(-n)

saveRDS(wvs_al_tbl, "./tabs/wvs_al_tbl.rds")

xlsx::write.xlsx(wvs_al_tab %>% data.frame, "./tabs/Table - % by Variable, CTN+Wave.xlsx", row.names = F)

wvs_al_socio_tab <- wvs_al_tbl %>% 
      select(-c(label, year_wave_code, id, cnt_code)) %>% 
      gather(variable, value, -cnt, -wave, -wt, -social_class, -income, -highest_edu) %>% 
      gather(var_socio, value_socio, -cnt, -wave, -wt, -variable, -value) %>% 
      group_by(cnt, wave, variable, value, value_socio) %>% 
      count(wt = wt) %>% 
      # summarize(n = n()) %>% 
      ungroup() %>% 
      group_by(cnt, wave, variable, value) %>% 
      mutate(pct = round(100*n/sum(n), 1)) %>% 
      select(-n)

xlsx::write.xlsx(wvs_al_socio_tab %>% data.frame, "./tabs/Table - % by Variable Sociodemographic, CTN+Wave.xlsx", row.names = F)


# tabulate data ----

# S003 - 76 (Brazil)

# TRUST VARIABLES:

# A165 - Most people can be trusted
# D001_B - How much you trust: Your family (B) (check D001)
# G007_17 - Trust: Your friends
# G007_18 - Trust: Your Neighborhood (check G007_18_B)
# G007_33_B - People you know personally
# G007_34_B - People you meet for the first time
# G007_35_B - People of another religion
# G007_36_B - People of another nationality

# SOCIODEMOGRAPHIC VARIABLES:

# X025A2 - Highest educational level attained
# X045 - Social class(subjective)
# X045B - Social class (subjective) with 6 categories

# table of n+% most people can be trusted, by cnt and wave ----

tbl_trust_1 <- wsv %>% 
      select(year_wave_code = S024, id = S007, cnt_code = S003, A165, wt = S017) %>% 
      # left_join(cnt_code, by = c("cnt_code" = "code")) %>% 
      left_join(cnt_wave_code, by = c("year_wave_code" = "code")) %>% 
      # count(year_wave_code) %>% 
      mutate(label = case_when(cnt_code == 620 ~ "Portugal (7)",
                               cnt_code == 804 ~ "Ukraine (7)",
                               TRUE ~ as.character(label))) %>%
      # filter(label == "Portugal")
      group_by(label) %>% 
      summarize(n_obs = n(),
                pct_trust = round(100*weighted.mean(A165 == 1, na.rm = T, w = wt), 1)) %>% 
      mutate(wave = str_sub(label, -3),
             label = str_trim(str_sub(label, 1, -4))) %>% 
      # count(wave)
      gather(var, value, -label, -wave) %>% 
      unite(wave_var, wave, var) %>% 
      spread(wave_var, value)

tbl_trust_1 %>% data.frame %>% xlsx::write.xlsx("./tabs/Table - % Most people can be trusted, CNT+Wave.xlsx", row.names = F, sheetName = "raw")

# table of n+% trust in different groups, brazil+sweden ----

wsv %>% 
   select(year_wave_code = S024, id = S007, cnt_code = S003, A165, D001_B, wt = S017) %>% 
   left_join(cnt_wave_code, by = c("year_wave_code" = "code")) %>%
   mutate(label = case_when(cnt_code == 620 ~ "Portugal (7)",
                            cnt_code == 804 ~ "Ukraine (7)",
                            TRUE ~ as.character(label))) %>%
   filter(str_detect(label, paste(c("Brazil", "Sweden"), collapse = "|"))) %>% 
   group_by(label) %>% 
   count(D001_B, wt = wt) %>%
   mutate(n = round(n)) %>%
   group_by(label) %>% 
   mutate(pct_trust = round(100*n/sum(n), 1)) %>% 
   filter(!is.na(D001_B)) %>% 
   mutate(D001_B = case_when(D001_B == 1 ~ "Trust completely",
                             D001_B == 2 ~ "Trust somewhat",
                             D001_B == 3 ~ "Do not trust very much",
                             D001_B == 4 ~ "Do not trust at all")) %>% 
   gather(var, value, -label, -D001_B) %>% 
   unite(label_var, label, var) %>% 
   spread(label_var, value) %>% 
   slice(3, 4, 2, 1) %>% 
   data.frame %>% 
   xlsx::write.xlsx("./tabs/Table - % Trust Groups, CNT+Wave.xlsx", row.names = F, sheetName = "raw")

wsv %>% 
      select(year_wave_code = S024, id = S007, cnt_code = S003, A165, G007_18_B, wt = S017) %>% 
      left_join(cnt_wave_code, by = c("year_wave_code" = "code")) %>%
      mutate(label = case_when(cnt_code == 620 ~ "Portugal (7)",
                               cnt_code == 804 ~ "Ukraine (7)",
                               TRUE ~ as.character(label))) %>%
      filter(str_detect(label, paste(c("Brazil", "Sweden"), collapse = "|"))) %>% 
      group_by(label) %>% 
      count(G007_18_B, wt = wt) %>%
      mutate(n = round(n)) %>%
      group_by(label) %>% 
      mutate(pct_trust = round(100*n/sum(n), 1)) %>% 
      filter(!is.na(G007_18_B)) %>% 
      mutate(G007_18_B = case_when(G007_18_B == 1 ~ "Trust completely",
                                  G007_18_B == 2 ~ "Trust somewhat",
                                  G007_18_B == 3 ~ "Do not trust very much",
                                  G007_18_B == 4 ~ "Do not trust at all")) %>% 
      gather(var, value, -label, -G007_18_B) %>% 
      unite(label_var, label, var) %>% 
      spread(label_var, value) %>% 
      slice(3, 4, 2, 1) %>% 
      data.frame %>% 
      xlsx::write.xlsx("./tabs/Table - % Trust Groups, CNT+Wave.xlsx", row.names = F, sheetName = str_c("raw", "G007_18_B", sep = "_"), append = TRUE)

wsv %>% 
      select(year_wave_code = S024, id = S007, cnt_code = S003, A165, G007_33_B, wt = S017) %>% 
      left_join(cnt_wave_code, by = c("year_wave_code" = "code")) %>%
      mutate(label = case_when(cnt_code == 620 ~ "Portugal (7)",
                               cnt_code == 804 ~ "Ukraine (7)",
                               TRUE ~ as.character(label))) %>%
      filter(str_detect(label, paste(c("Brazil", "Sweden"), collapse = "|"))) %>% 
      group_by(label) %>% 
      count(G007_33_B, wt = wt) %>%
      mutate(n = round(n)) %>%
      group_by(label) %>% 
      mutate(pct_trust = round(100*n/sum(n), 1)) %>% 
      filter(!is.na(G007_33_B)) %>% 
      mutate(G007_33_B = case_when(G007_33_B == 1 ~ "Trust completely",
                                   G007_33_B == 2 ~ "Trust somewhat",
                                   G007_33_B == 3 ~ "Do not trust very much",
                                   G007_33_B == 4 ~ "Do not trust at all")) %>% 
      gather(var, value, -label, -G007_33_B) %>% 
      unite(label_var, label, var) %>% 
      spread(label_var, value) %>% 
      slice(3, 4, 2, 1) %>% 
      data.frame %>% 
      xlsx::write.xlsx("./tabs/Table - % Trust Groups, CNT+Wave.xlsx", 
                       row.names = F, 
                       sheetName = str_c("raw", "G007_33_B", sep = "_"), 
                       append = TRUE)

wsv %>% 
      select(year_wave_code = S024, id = S007, cnt_code = S003, A165, G007_34_B, wt = S017) %>% 
      left_join(cnt_wave_code, by = c("year_wave_code" = "code")) %>%
      mutate(label = case_when(cnt_code == 620 ~ "Portugal (7)",
                               cnt_code == 804 ~ "Ukraine (7)",
                               TRUE ~ as.character(label))) %>%
      filter(str_detect(label, paste(c("Brazil", "Sweden"), collapse = "|"))) %>% 
      group_by(label) %>% 
      count(G007_34_B, wt = wt) %>%
      mutate(n = round(n)) %>%
      group_by(label) %>% 
      mutate(pct_trust = round(100*n/sum(n), 1)) %>% 
      filter(!is.na(G007_34_B)) %>% 
      mutate(G007_34_B = case_when(G007_34_B == 1 ~ "Trust completely",
                                   G007_34_B == 2 ~ "Trust somewhat",
                                   G007_34_B == 3 ~ "Do not trust very much",
                                   G007_34_B == 4 ~ "Do not trust at all")) %>% 
      gather(var, value, -label, -G007_34_B) %>% 
      unite(label_var, label, var) %>% 
      spread(label_var, value) %>% 
      slice(3, 4, 2, 1) %>% 
      data.frame %>% 
      xlsx::write.xlsx("./tabs/Table - % Trust Groups, CNT+Wave.xlsx", 
                       row.names = F, 
                       sheetName = str_c("raw", "G007_34_B", sep = "_"), 
                       append = TRUE)

wsv %>% 
      select(year_wave_code = S024, id = S007, cnt_code = S003, A165, G007_35_B, wt = S017) %>% 
      left_join(cnt_wave_code, by = c("year_wave_code" = "code")) %>%
      mutate(label = case_when(cnt_code == 620 ~ "Portugal (7)",
                               cnt_code == 804 ~ "Ukraine (7)",
                               TRUE ~ as.character(label))) %>%
      filter(str_detect(label, paste(c("Brazil", "Sweden"), collapse = "|"))) %>% 
      group_by(label) %>% 
      count(G007_35_B, wt = wt) %>%
      mutate(n = round(n)) %>%
      group_by(label) %>% 
      mutate(pct_trust = round(100*n/sum(n), 1)) %>% 
      filter(!is.na(G007_35_B)) %>% 
      mutate(G007_35_B = case_when(G007_35_B == 1 ~ "Trust completely",
                                   G007_35_B == 2 ~ "Trust somewhat",
                                   G007_35_B == 3 ~ "Do not trust very much",
                                   G007_35_B == 4 ~ "Do not trust at all")) %>% 
      gather(var, value, -label, -G007_35_B) %>% 
      unite(label_var, label, var) %>% 
      spread(label_var, value) %>% 
      slice(3, 4, 2, 1) %>% 
      data.frame %>% 
      xlsx::write.xlsx("./tabs/Table - % Trust Groups, CNT+Wave.xlsx", 
                       row.names = F, 
                       sheetName = str_c("raw", "G007_35_B", sep = "_"), 
                       append = TRUE)

wvs %>% 
      select(year_wave_code = S024, id = S007, cnt_code = S003, A165, G007_36_B, wt = S017) %>% 
      left_join(cnt_wave_code, by = c("year_wave_code" = "code")) %>%
      mutate(label = case_when(cnt_code == 620 ~ "Portugal (7)",
                               cnt_code == 804 ~ "Ukraine (7)",
                               TRUE ~ as.character(label))) %>%
      # filter(str_detect(label, paste(c("Brazil", "Sweden"), collapse = "|"))) %>% 
      filter(str_detect(label, paste(c("Brazil"), collapse = "|"))) %>% 
      group_by(label) %>% 
      count(G007_36_B, wt = wt) %>%
      mutate(n = round(n)) %>%
      group_by(label) %>% 
      mutate(pct_trust = round(100*n/sum(n), 1)) %>% 
      filter(!is.na(G007_36_B)) %>% 
      mutate(G007_36_B = case_when(G007_36_B == 1 ~ "Trust completely",
                                   G007_36_B == 2 ~ "Trust somewhat",
                                   G007_36_B == 3 ~ "Do not trust very much",
                                   G007_36_B == 4 ~ "Do not trust at all")) %>% 
      gather(var, value, -label, -G007_36_B) %>% 
      unite(label_var, label, var) %>% 
      spread(label_var, value) %>% 
      slice(3, 4, 2, 1) %>% 
      data.frame %>% 
      xlsx::write.xlsx("./tabs/Table - % Trust Groups, CNT+Wave.xlsx", 
                       row.names = F, 
                       sheetName = str_c("raw", "G007_36_B", sep = "_"), 
                       append = TRUE)

# G007_17 - Trust: Your friends
# G007_18_B - Trust: Your Neighborhood (check G007_18_B)
# G007_33_B - People you know personally
# G007_34_B - People you meet for the first time
# G007_35_B - People of another religion
# G007_36_B - People of another nationality

# https://www.worldvaluessurvey.org/WVSOnline.jsp


# table of n+% by highest educational level attained ----

# X025 tem menos missing do que X025A2, que só tem obs para 2017-2020

wvs_clean_br %>% 
      select(year_wave_code, id, cnt_code, wt, label, highest_edu = X025) %>% 
      dplyr::mutate(highest_edu = case_when(highest_edu == 1 ~ "Inadequately completed elementary education",
                                            highest_edu == 2 ~ "Completed elementary education",
                                            highest_edu %in% c(3, 5) ~ "Incomplete secondary school",
                                            highest_edu %in% c(4, 6) ~ "Complete secondary school",
                                            highest_edu == 7 ~ "Some university",
                                            highest_edu == 8 ~ "University")) %>%
      group_by(label) %>% 
      count(highest_edu, wt = wt) %>%
      mutate(n = round(n)) %>%
      group_by(label) %>% 
      mutate(pct = round(100*n/sum(n), 1)) %>% 
      filter(!is.na(highest_edu)) %>% 
      gather(var, value, -label, -highest_edu) %>% 
      unite(label_var, label, var) %>% 
      spread(label_var, value) %>% 
      slice(3, 2, 4, 1, 5, 6) %>% 
      data.frame %>% 
      xlsx::write.xlsx("./tabs/Table - % Highest education, BR.xlsx", 
                       row.names = F, 
                       sheetName = str_c("raw", "X025", sep = "_"), 
                       append = TRUE)

# table of n+% by household income ----

# X47 : agregar em 5 pontos

wvs_clean_br %>% 
      select(year_wave_code, id, cnt_code, wt, label, income = X047) %>% 
      dplyr::mutate(income = case_when(income %in% c(1, 2) ~ "Lower",
                                       income %in% c(3, 4) ~ "Lower upper",
                                       income %in% c(5, 6) ~ "Middle",
                                       income %in% c(7, 8) ~ "Upper lower",
                                       income %in% c(9, 10) ~ "Upper")) %>% 
      group_by(label) %>% 
      count(income, wt = wt) %>%
      mutate(n = round(n)) %>%
      group_by(label) %>% 
      mutate(pct = round(100*n/sum(n), 1)) %>% 
      filter(!is.na(income)) %>% 
      gather(var, value, -label, -income) %>% 
      unite(label_var, label, var) %>% 
      spread(label_var, value) %>% 
      slice(4, 5, 3, 2, 1) %>% 
      data.frame %>% 
      xlsx::write.xlsx("./tabs/Table - % Income, BR.xlsx", 
                       row.names = F, 
                       sheetName = str_c("raw", "X047", sep = "_"), 
                       append = TRUE)

# table of n+% by social class ----

wvs_clean_br %>% 
      select(year_wave_code, id, cnt_code, wt, label, social_class = X045) %>% 
      dplyr::mutate(social_class = case_when(social_class == 1 ~ "Upper class",
                                             social_class == 2 ~ "Upper middle class",
                                             social_class == 3 ~ "Lower middle class",
                                             social_class == 4 ~ "Working class",
                                             social_class == 5 ~ "Lower class")) %>% 
      group_by(label) %>% 
      count(social_class, wt = wt) %>%
      mutate(n = round(n)) %>%
      group_by(label) %>% 
      mutate(pct = round(100*n/sum(n), 1)) %>% 
      filter(!is.na(social_class)) %>% 
      gather(var, value, -label, -social_class) %>% 
      unite(label_var, label, var) %>% 
      spread(label_var, value) %>% 
      slice(3, 4, 2, 1, 5) %>% 
      data.frame %>% 
      xlsx::write.xlsx("./tabs/Table - % Social Class, BR.xlsx", 
                       row.names = F, 
                       sheetName = str_c("raw", "X045", sep = "_"), 
                       append = TRUE)

# table of n+% trust by highest educational level attained ----




# table of n+% trust by social class ----






# table of n+% trust by income ----




# table of n+% trust in institutions - overall ----

confidence_tabs <- function(var) {
      
      # var <- enquo(var)
      
      wvs_clean_br %>% 
            # dplyr::mutate(var2 = case_when(!!var == 1 ~ "A great deal",
            #                                !!var == 2 ~ "Quite a lot",
            #                                !!var == 3 ~ "Not very much",
            #                                !!var == 4 ~ "None at all")) %>%
            group_by(label) %>%
            count(.data[[var]], wt = wt) %>% 
            mutate(n = round(n)) %>%
            mutate(pct_trust = round(100*n/sum(n), 1)) %>%
            filter(!is.na(.data[[var]])) %>%
            # select(-.data[[var]])
            gather(var, value, -label, -.data[[var]]) %>%
            unite(label_var, label, var) %>%
            spread(label_var, value)
            
}

confidence_vars <- names(wvs)[c(410:426, 435, 445, 447:448, 459)]
confidence_vars <- set_names(confidence_vars)

confidence_tabs <- map(confidence_vars, ~confidence_tabs(.x))

openxlsx::write.xlsx(confidence_tabs, file = "./tabs/Table - % Trust Institutions, BR.xlsx")

#  - Confience: Chuchres
#  - Confience: Armed forces
#  - Confidence: Education System
#  - Confidence: Press
#  - Confidence: Labour Unions
#  - Confidence: The Police
#  - Confidence: Parliament
#  - Confidence: The Civil Services
#  - Confidence: Social Secutiry System
#  - Confidence: Television
#  - Confidence: The Government
#  - Confidence: Political Parties
#  - Confidence: Major Companies
#  - Confidence: Health Care System
#  - Confidence: The Presidency
#  - Confidence: The United Nations
#  - Confidence: Mercorsur
#  - Confidence: Charitable or humanitarian organizations
#  - Confidence: The Environmental Protection Movement
#  - Confidence: The Women´s Movement
#  - Confidence: Justice System/Courts
#  - Confidence: Banks
#  - Confidence: Universities


























