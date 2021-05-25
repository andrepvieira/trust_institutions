library(tidyverse)
library(labelled)

bar_04_18 <- haven::read_dta("./data/barometer/2004-2018 LAPOP AmericasBarometer Merge (v1.0FREE).dta")

# bar_04_18 %>%
#       filter(pais %in% c(1, 8, 15, 13, 17)) %>%
#       mutate(pais = to_factor(pais)) %>%
#       count(pais)

bar_04_18_filtered <- bar_04_18 %>%
      filter(pais %in% c(1, 8, 13, 15, 17)) %>%
      select(pais,
             prov,
             year,
             estratopri,
             wt,
             trust_people = it1,
             b1:b47a,
             problem_cnt = a4,
             politician_corruption = exc7new,
             public_agent_corruption = exc7,
             # mil10a:mil10z,
             income = q10,
             education = ed
      ) %>%
      mutate_if(is.labelled, list(~replace_na(., 99))) %>%
      mutate_at(vars(matches("^b")), to_factor) %>%
      mutate(pais = to_factor(pais),
             prov = to_factor(prov),
             trust_people = to_factor(trust_people),
             education = to_factor(education),
             income = to_factor(income),
             problem_cnt = to_factor(problem_cnt),
             public_agent_corruption = to_factor(public_agent_corruption),
             politician_corruption = to_factor(politician_corruption))


bar_04_18_filtered <- bar_04_18_filtered %>% 
      mutate(regiao = case_when(prov %in% c("Amapá", "Amazonas", "Pará", "Roraima",
                                            "Rondônia", "Acre", "Tocantins") ~ "Norte",
                                prov %in% c("Alagoas", "Sergipe", "Pernambuco", "Bahía",
                                            "Ceará", "Paraibá", "Rio Grande do Norte", 
                                            "Maranhão", "Piauí") ~ "Nordeste",
                                prov %in% c("Rio de Janeiro", "São Paulo",
                                            "Espírito Santo", "Minas Gerais") ~ "Sudeste",
                                prov %in% c("Santa Catarina",
                                            "Rio Grande do Sul",
                                            "Paraná") ~ "Sul",
                                prov %in% c("Goiás", "Distrito Federal", "Mato Grosso",
                                            "Mato Grosso do Sul") ~ "Centro-Oeste"))

bar_04_18_filtered <- bar_04_18_filtered %>% 
      mutate(education = case_when(education == "None" ~ "None",
                                   education %in% c("1", "2", "3", "4", "5", "6") ~ "Primary education",
                                   education %in% c("7", "8", "9", "10", "11", "12") ~ "Secondary education",
                                   education %in% c("16", "17", "18+") ~ "University",
                                   education %in% c("13", "14", "15") ~ "Post-secondary, not university"))

bar_04_18_filtered <- bar_04_18_filtered %>% 
      mutate(weight1500 = case_when(pais == "Mexico" ~ wt*1500/1562,
                                    pais == "Colombia" ~ wt*1500/1506,
                                    pais == "Chile" ~ wt*1500/1965,
                                    pais == "Brazil" ~ wt*1500/2482,
                                    pais == "Argentina" ~ wt*1410/1500))


# IT1 vs regiao (BR), renda, escolaridade ----

tab_trust_people_overall <- bar_04_18_filtered %>% 
      select(pais, year, wt, weight1500, trust_people, regiao, income, education) %>% 
      na_if(99) %>% 
      # mutate_all(replace_na, replace = "Missing")
      select(pais, year, wt, weight1500, trust_people) %>% 
      group_by(pais, year) %>% 
      count(trust_people, wt = wt) %>% 
      group_by(pais, year) %>% 
      mutate(total = sum(n),
             prop = round(100*n/total, 1)) %>% 
      mutate(crossing = "Overall") %>% 
      select(pais, year, trust_people, crossing, prop)
      
tab_trust_people_inc_edu <- bar_04_18_filtered %>% 
      select(pais, year, wt, trust_people, income, education) %>% 
      na_if(99) %>% 
      gather(var, value, -pais, -year, -trust_people, -wt) %>% 
      group_by(pais, year, var, value) %>% 
      count(trust_people, wt = wt) %>% 
      group_by(pais, year, var, value) %>% 
      mutate(total = sum(n),
             prop = round(100*n/total, 1)) %>% 
      ungroup() %>% 
      select(pais, year, trust_people, crossing = value, prop)
      
tab_trust_people_regiao <- bar_04_18_filtered %>% 
      filter(pais == "Brazil") %>% 
      select(pais, year, wt, trust_people, regiao) %>% 
      filter(!is.na(regiao)) %>% 
      na_if(99) %>% 
      group_by(pais, year, regiao) %>% 
      count(trust_people, wt = wt) %>% 
      group_by(pais, year, regiao) %>% 
      mutate(total = sum(n),
             prop = round(100*n/total, 1)) %>% 
      select(pais, year, trust_people, crossing = regiao, prop)
  
tab_trust_people_complete <- bind_rows(tab_trust_people_overall,
                                       tab_trust_people_inc_edu,
                                       tab_trust_people_regiao)


saveRDS(tab_trust_people_complete, "./app/barometer_tab_trust_people.rds")

# A4 vs regiao (BR), renda, escolaridade ----

tab_corruption_overall <- bar_04_18_filtered %>% 
      select(pais, year, wt, problem_cnt, regiao, income, education) %>% 
      na_if(99) %>% 
      # mutate_all(replace_na, replace = "Missing")
      select(pais, year, wt, problem_cnt) %>% 
      group_by(pais, year) %>% 
      count(problem_cnt, wt = wt) %>% 
      group_by(pais, year) %>% 
      mutate(total = sum(n),
             prop = round(100*n/total, 1)) %>% 
      mutate(crossing = "Overall") %>% 
      filter(problem_cnt == "Corruption") %>% 
      select(pais, year, problem_cnt, crossing, prop)

tab_corruption_inc_edu <- bar_04_18_filtered %>% 
      select(pais, year, wt, problem_cnt, income, education) %>% 
      na_if(99) %>% 
      gather(var, value, -pais, -year, -problem_cnt, -wt) %>% 
      group_by(pais, year, var, value) %>% 
      count(problem_cnt, wt = wt) %>% 
      group_by(pais, year, var, value) %>% 
      mutate(total = sum(n),
             prop = round(100*n/total, 1)) %>% 
      ungroup() %>% 
      filter(problem_cnt == "Corruption") %>% 
      select(pais, year, problem_cnt, crossing = value, prop)

tab_corruption_regiao <- bar_04_18_filtered %>% 
      filter(pais == "Brazil") %>% 
      select(pais, year, wt, problem_cnt, regiao) %>% 
      filter(!is.na(regiao)) %>% 
      na_if(99) %>% 
      group_by(pais, year, regiao) %>% 
      count(problem_cnt, wt = wt) %>% 
      filter(problem_cnt == "Corruption") %>% 
      group_by(pais, year) %>% 
      mutate(total = sum(n),
             prop = round(100*n/total, 1)) %>% 
      select(pais, year, problem_cnt, crossing = regiao, prop)


tab_corruption_complete <- bind_rows(tab_corruption_overall,
                                       tab_corruption_inc_edu,
                                       tab_corruption_regiao)

tab_corruption_complete <- tab_corruption_complete %>% 
      ungroup() %>% 
      mutate(variable = case_when(crossing %in% c("Nordeste", "Centro-Oeste", "Sul", "Norte", "Sudeste") ~ "Region",
                                  crossing %in% c("Entre $$1.201 y $$2.000",
                                                  "Entre $$2.001 y $$2.600",
                                                  "Entre $$2.601 y $$3.300",
                                                  "Entre $$3.301 y $$4.200",
                                                  "Entre $$4.201 y $$5.200",
                                                  "Entre $$5.201 y $$6.800",
                                                  "Entre $$6.801 y $$8.900",
                                                  "Entre $$8.901 y $$13.000",
                                                  "Más de $$13.000",
                                                  "Menos de $$1.200", 
                                                  "Ningún ingreso") ~ "Income",
                                  crossing %in% c("Overall") ~ "Overall",
                                  crossing %in% c("Primary education", "Secondary education", "University", "Post-secondary, not university") ~ "Education"))

tab_corruption_complete <- tab_corruption_complete %>% 
      mutate(crossing = factor(crossing, levels = c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste",
                                                    "Primary education", "Secondary education", "Post-secondary, not university", "University",
                                                    "Ningún ingreso",
                                                    "Menos de $$1.200", 
                                                    "Entre $$1.201 y $$2.000",
                                                    "Entre $$2.001 y $$2.600",
                                                    "Entre $$2.601 y $$3.300",
                                                    "Entre $$3.301 y $$4.200",
                                                    "Entre $$4.201 y $$5.200",
                                                    "Entre $$5.201 y $$6.800",
                                                    "Entre $$6.801 y $$8.900",
                                                    "Entre $$8.901 y $$13.000",
                                                    "Más de $$13.000",
                                                    "Overall")))

saveRDS(tab_corruption_complete, "./app/tab_corruption_complete.rds")

tab_corruption_complete %>% 
      filter(pais == "Brazil",
             variable == "Region") %>% 
      data.frame

# IT1 vs variaveis de confianca (especialmente B1) ----

bar_04_18_filtered %>% 
      select(pais, year, wt, trust_people, b1:b47a)


# EXC7NEW (tabular dados gerais) ----

tab_pub_corruption_complete <- bar_04_18_filtered %>% 
      # filter(pais == "Brazil") %>% 
      # count(public_agent_corruption) %>% 
      select(pais, year, wt, public_agent_corruption) %>% 
      na_if(99) %>% 
      # mutate_all(replace_na, replace = "Missing")
      select(pais, year, wt, public_agent_corruption) %>% 
      group_by(pais, year) %>% 
      count(public_agent_corruption, wt = wt) %>% 
      group_by(pais, year) %>% 
      mutate(total = sum(n),
             prop = round(100*n/total, 1)) %>% 
      mutate(crossing = "Overall") %>% 
      select(pais, year, public_agent_corruption, crossing, prop)

tab_pub_corruption_complete <- tab_pub_corruption_complete %>% 
      ungroup() %>% 
      # count(public_agent_corruption)
      mutate(public_agent_corruption = factor(public_agent_corruption, levels = c("Not Widespread at All",
                                                                                  "Not Very Widespread",
                                                                                  "Somewhat Widespread",
                                                                                  "Very Widespread")))

saveRDS(tab_pub_corruption_complete, "./app/tab_pub_corruption_complete.rds")




















