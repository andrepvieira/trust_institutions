
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

# load raw data ----

wvs <- readr::read_rds("./data/wvs/WVS_TimeSeries_R_v1_6.rds")
cnt_code <- clean_names(readxl::read_excel("./data/wvs/F00003844-WVS_Time_Series_List_of_Variables_and_equivalences.xlsx", sheet = 2, skip = 2, n_max = 104))
cnt_wave_code <- clean_names(readxl::read_excel("./data/wvs/F00003844-WVS_Time_Series_List_of_Variables_and_equivalences.xlsx", sheet = 6, skip = 1, n_max = 293))

wvs_7 <- readr::read_rds("./data/wvs/WVS_Cross-National_Wave_7_R_v1_6.rds")

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

wsv %>% 
      select(year_wave_code = S024, id = S007, cnt_code = S003, A165, wt = S017) %>% 
      # left_join(cnt_code, by = c("cnt_code" = "code")) %>% 
      left_join(cnt_wave_code, by = c("year_wave_code" = "code")) %>% 
      filter(str_detect(label, "Sweden")) %>% 
      count(label)

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

# table of n+% by social class ----

# table of n+% trust by highest educational level attained ----

# table of n+% trust by by social class ----

# table of n+% trust in institutions - overall ----

wvs_clean_br <- wvs %>% 
      rename(year_wave_code = S024, id = S007, cnt_code = S003, wt = S017) %>% 
      # select(year_wave_code = S024, id = S007, cnt_code = S003,
      #        E069_01:E069_17,
      #        E069_26, E069_38, E069_40:E069_41,
      #        E069_54,
      #        wt = S017) %>%
      left_join(cnt_wave_code, by = c("year_wave_code" = "code")) %>%
      mutate(label = case_when(cnt_code == 620 ~ "Portugal (7)",
                               cnt_code == 804 ~ "Ukraine (7)",
                               TRUE ~ as.character(label))) %>%
      filter(str_detect(label, paste(c("Brazil"), collapse = "|")))
# purrr::map(., ~confidence_tabs(.x))

wvs_clean_br %>%
      # group_by(label) %>%
      count(E069_16, wt = wt) %>%
      mutate(n = round(n)) %>%
      mutate(pct_trust = round(100*n/sum(n), 1)) %>%
      filter(!is.na(E069_01)) %>%
      dplyr::mutate(E069_01 = case_when(E069_01 == 1 ~ "A great deal",
                                        E069_01 == 2 ~ "Quite a lot",
                                        E069_01 == 3 ~ "Not very much",
                                        E069_01 == 4 ~ "None at all")) %>%
      # select(-.data[[var]])
      gather(var, value, -label, -E069_01) %>%
      unite(label_var, label, var) %>%
#       spread(label_var, value)

library(rlang)

wvs_clean_br <- wvs_clean_br %>% 
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


























