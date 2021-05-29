# 0. CARREGA PACOTES ----
library(tidyverse)
library(readxl)
library(janitor)

# 1. CARREGA DADOS ----

sips <- readxl::read_excel("./data/sips/SIPS Valores.xlsx")

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

