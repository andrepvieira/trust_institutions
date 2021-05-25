# 0. CARREGA PACOTES ----
library(tidyverse)
library(readxl)
library(janitor)

# 1. CARREGA DADOS ----

sips <- readxl::read_excel("./data/SIPS Valores.xlsx")

# 2. LIMPA DADOS ----

sips_limpo <- sips %>% 
      select(confianca = Q27, renda = Q10.1, fam = Q10.2) %>% 
      filter(!renda == 999, !renda == 888,
             !fam == 999, !fam == 888,
             !fam == 44,
             !confianca == 888) %>% 
      mutate(renda_per_capita = renda/fam,
             renda_per_capita2 = cut(renda_per_capita, 
                                     breaks = quantile(renda_per_capita, probs=seq(0,1, by=0.25), na.rm=TRUE),
                                     include.lowest=TRUE),
             renda_per_capita_cat = case_when(renda_per_capita2 == "[0.125,204]" ~ "1º Quartil",
                                              renda_per_capita2 == "(204,375]" ~ "2º Quartil",
                                              renda_per_capita2 == "(375,600]" ~ "3º Quartil",
                                              renda_per_capita2 == "(600,3e+04]" ~ "4º Quartil"),
             confianca = case_when(confianca == 1 ~ "Concorda bastante",
                                   confianca == 2 ~ "Concorda",
                                   confianca == 3 ~ "Não concorda, nem discorda",
                                   confianca == 4 ~ "Discorda",
                                   confianca == 5 ~ "Discorda bastante"),
             confianca = factor(confianca, levels = c("Concorda bastante",
                                                      "Concorda",
                                                      "Não concorda, nem discorda",
                                                      "Discorda",
                                                      "Discorda bastante")))

# 3. ANALISE ----

# 3.1 Confianca por renda familiar per capita (q10.1 + q10.2) ----
      
fig_sips <- sips_limpo %>% 
      select(renda_per_capita_cat, confianca) %>% 
      group_by(renda_per_capita_cat, confianca) %>%
      mutate(n = n()) %>% 
      group_by(renda_per_capita_cat) %>% 
      mutate(total = n()) %>% 
      ungroup() %>% 
      mutate(prop = round(100*n/total, 1)) %>% 
      ggplot(aes(renda_per_capita_cat, prop, fill = confianca)) + 
      geom_bar(stat = "identity", position = position_dodge()) +
      geom_text(aes(label = prop), size = 6, position = position_dodge(width = 0.9), vjust = -0.25) +
      tidyquant::theme_tq() +
      scale_fill_brewer(palette = "Set1") +
      labs(x = "Renda familiar per capita",
           y = "Percentual (%)") +
      theme(legend.title = element_blank(),
            axis.text = element_text(size = 16, color = "black"),
            axis.title = element_text(size = 18, color = "black"),
            legend.text = element_text(size = 14, color = "black"))

jpeg(filename = "./figs/Figura 1 - Confianca por renda per capita - SIPS.jpeg", width = 966, height = 667)

fig_sips

dev.off()

tab_sips <- sips_limpo %>% 
      select(renda_per_capita_cat, confianca) %>% 
      group_by(renda_per_capita_cat, confianca) %>%
      mutate(n = n()) %>% 
      group_by(renda_per_capita_cat) %>% 
      mutate(total = n()) %>% 
      ungroup() %>% 
      mutate(prop = round(100*n/total, 1)) %>% 
      select(-n, -total) %>%
      distinct(renda_per_capita_cat, confianca, .keep_all = T) %>% 
      spread(renda_per_capita_cat, prop)

tab_sips %>% data.frame %>% xlsx::write.xlsx(., "Tabela 1 - Confianca por renda per capita - SIPS.xlsx", row.names = F)

rm(list=ls())

# 3.2 Numero de OSCs ativas por UF e regiao (AGUARDANDO DADOS) ----

load("./data/base_mortalidade_osc_2018.rda")

pop_2000_2010 <- readxl::read_excel("./data/tabela1552_populacao_por_sexo_e_urbana_rural_2000_2010_censo.xlsx", skip = 6, n_max = 5565)
pop_2019_2020 <- readxl::read_excel("./data/Tabela 6579_https___sidra.ibge.gov.br_Tabela_6579.xlsx", skip = 3)

pop_2000_2010_uf <- pop_2000_2010 %>% 
      select(uf, `2000_Total_Total`, `2010_Total_Total`) %>%
      gather(ano, populacao, -uf) %>% 
      mutate(ano = str_sub(ano, 1, 4),
             populacao = as.numeric(populacao)) %>% 
      group_by(uf, ano) %>% 
      summarize(populacao = sum(populacao, na.rm = T)) %>% 
      ungroup()
   
pop_2019_2020_uf <- pop_2019_2020 %>% 
      select(uf, `2019`, `2020`) %>% 
      gather(ano, populacao, -uf) %>% 
      group_by(uf, ano) %>% 
      summarize(populacao = sum(populacao, na.rm = T)) %>% 
      ungroup()

pop_2000_2020_reg <- pop_2000_2010_uf %>% 
      bind_rows(pop_2019_2020_uf) %>% 
      mutate(regiao = case_when(uf %in% c("RO", "AC", "AM", "RR", "PA", "AP", "TO") ~ "Norte",
                                uf %in% c("MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA") ~ "Nordeste",
                                uf %in% c("MG", "ES", "RJ", "SP") ~ "Sudeste",
                                uf %in% c("PR", "SC", "RS") ~ "Sul",
                                uf %in% c("MS", "MT", "GO", "DF") ~ "Centro-Oeste")) %>% 
      group_by(regiao, ano) %>% 
      summarize(populacao = sum(populacao)) %>% 
      filter(!is.na(regiao)) %>% 
      ungroup() %>% 
      rename(territorio = regiao)
      
pop_2000_2020_uf <-  pop_2000_2010_uf %>% 
      bind_rows(pop_2019_2020_uf) %>% 
      rename(territorio = uf) %>% 
      arrange(territorio, ano)

pop_2000_2020_completo <- bind_rows(pop_2000_2020_uf, pop_2000_2020_reg)

osc_2000_2018_uf <- bind_rows(
      cadastro_osc2018 %>% 
            filter(ativa == "Ativa") %>% 
            filter(ano_situacao <= 2000) %>% 
            group_by(uf) %>% 
            summarize(n_osc = n_distinct(cnpj)) %>% 
            add_column(ano = 2000, .before = "n_osc"),
      cadastro_osc2018 %>% 
            filter(ativa == "Ativa") %>% 
            filter(ano_situacao <= 2010) %>% 
            group_by(uf) %>% 
            summarize(n_osc = n_distinct(cnpj)) %>% 
            add_column(ano = 2010, .before = "n_osc"),
      cadastro_osc2018 %>% 
            filter(ativa == "Ativa") %>% 
            filter(ano_situacao <= 2018) %>% 
            group_by(uf) %>% 
            summarize(n_osc = n_distinct(cnpj)) %>% 
            add_column(ano = 2018, .before = "n_osc")
      )


