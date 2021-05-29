# 0. CARREGA PACOTES ----
library(tidyverse)
library(readxl)
library(janitor)

# 1. CARREGA DADOS ----

load("./data/base_mortalidade_osc_2018.rda")
df_osc <- readxl::read_excel("./data/osc/OSCs ativas e baixadas ate 2020 municipio.xlsx", sheet = 1) %>% janitor::clean_names()
df_ibge <- readxl::read_excel("./data/Tabela Estados IBGE.xlsx") %>% janitor::clean_names()

pop_2000_2010 <- readxl::read_excel("./data/tabela1552_populacao_por_sexo_e_urbana_rural_2000_2010_censo.xlsx", skip = 6, n_max = 5565)
pop_2019_2020 <- readxl::read_excel("./data/Tabela 6579_https___sidra.ibge.gov.br_Tabela_6579.xlsx", skip = 3)

# 2. LIMPA DADOS ----

df_osc_limpo <- df_osc %>% 
      mutate(cd_uf = as.numeric(str_sub(codemun, 1, 2))) %>% 
      left_join(df_ibge, by = c("cd_uf" = "codigo_da_uf")) %>% 
      group_by(ano, regiao, uf) %>% 
      summarize(n_osc_ativa = sum(ativa))

pop_2000_2010_uf <- pop_2000_2010 %>% 
      select(uf, `2000_Total_Total`, `2010_Total_Total`) %>%
      left_join(select(df_ibge, uf, regiao)) %>% 
      gather(ano, populacao, -uf, -regiao) %>% 
      mutate(ano = str_sub(ano, 1, 4),
             populacao = as.numeric(populacao)) %>% 
      group_by(ano, uf, regiao) %>% 
      summarize(populacao = sum(populacao, na.rm = T)) %>% 
      ungroup()

pop_2019_2020_uf <- pop_2019_2020 %>% 
      select(uf, `2019`, `2020`) %>% 
      left_join(select(df_ibge, uf, regiao)) %>% 
      gather(ano, populacao, -uf, -regiao) %>% 
      group_by(ano, uf, regiao) %>% 
      summarize(populacao = sum(populacao, na.rm = T)) %>% 
      ungroup()

pop_2000_2020_uf <- pop_2000_2010_uf %>% 
      bind_rows(pop_2019_2020_uf) %>% 
      mutate(ano = as.numeric(ano))

# pop_2000_2020_completo <- bind_rows(pop_2000_2020_uf, pop_2000_2020_reg)

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

df_osc_per_capita <- df_osc_limpo %>% 
      filter(ano %in% c(2000, 2010, 2019, 2020)) %>% 
      left_join(select(pop_2000_2020_uf, -regiao)) %>%
      mutate(osc_per_capita_uf = (n_osc_ativa/populacao)*100000) %>% 
      group_by(ano, regiao) %>% 
      mutate(n_osc_ativa_regiao = sum(n_osc_ativa),
             populacao_regiao = sum(populacao),
             osc_per_capita_regiao = (n_osc_ativa_regiao/populacao_regiao)*100000) %>%
      ungroup()
      
# 3. ANALISE ----

# 3.1 Correlacao entre N de OSCs/per capita e confianca na regiao, por ano ----

bar_completo <- readRDS("./data/barometer/barometer_completo.rds")

df_bar_limpo <- bar_completo %>% 
      mutate(pais = as.character(pais),
             pais = ifelse(pais == "[%76%] Brasil", "Brazil", pais)) %>% 
      filter(ano %in% c("2000", "2010", "2018"),
             pais == "Brazil") %>% 
      mutate(ano = as.numeric(as.character(ano)),
             ano = ifelse(ano == 2018, 2019, ano)) %>% 
      mutate(regiao = case_when(str_detect(regiao, "Norte") ~ "Norte",
                                str_detect(regiao, "Nordeste") ~ "Nordeste",
                                str_detect(regiao, "Sudeste") ~ "Sudeste",
                                str_detect(regiao, "Sul") | str_detect(regiao, "Sur") ~ "Sul",
                                str_detect(regiao, "Centro-Oeste") ~ "Centro-Oeste",
                                TRUE ~ "Não informado")) %>% 
      filter(!regiao == "Não informado") %>% 
      group_by(ano, regiao) %>% 
      count(confianca_pessoal, wt = wt) %>% 
      mutate(prop = n/sum(n),
             prop = round(100*prop, 1)) %>% 
      ungroup() %>% 
      filter(confianca_pessoal == "Most people can be trusted") %>% 
      select(ano, regiao, prop_confia = prop)

fig_osc_baro_1 <- df_osc_per_capita %>%
      filter(ano %in% c(2000, 2010, 2019)) %>% 
      select(ano, regiao, osc_per_capita_regiao) %>% 
      distinct(ano, regiao, .keep_all = T) %>% 
      filter(!is.na(regiao)) %>% 
      left_join(df_bar_limpo) %>% 
      filter(!is.na(prop_confia)) %>% 
      ggscatter(
            x = "osc_per_capita_regiao", 
            y = "prop_confia",
            add = "reg.line",  # Add regressin line
            # add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
            conf.int = TRUE # Add confidence interval
      ) +
      stat_cor(p.accuracy = 0.001, r.accuracy = 0.01) +
      ggrepel::geom_label_repel(aes(label = regiao, alpha = .6)) +
      facet_wrap(~ano) +
      labs(x = "Número de OSCs por 100.000 habitantes",
           y = "Confiança interpessoal (%)") +
      tidyquant::theme_tq() +
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 14), 
            legend.position = "none",
            strip.text = element_text(size = 14))
      
jpeg(filename = "./figs/Figura_Confianca_OSCs_percapita_BAROMETRO_REGIAO.jpeg", width = 966, height = 667)
fig_osc_baro_1
dev.off()

df_osc_per_capita %>%
      filter(ano %in% c(2000, 2010, 2019)) %>% 
      select(ano, regiao, osc_per_capita_regiao) %>% 
      distinct(ano, regiao, .keep_all = T) %>% 
      filter(!is.na(regiao)) %>% 
      left_join(df_bar_limpo) %>% 
      filter(!is.na(prop_confia)) %>% 
      data.frame %>% 
      xlsx::write.xlsx("./tabs/Tabela_Confianca_OSCs_percapita_BAROMETRO_REGIAO.xlsx", row.names = F, sheetName = "raw")

df_osc_per_capita %>%
      filter(ano %in% c(2000, 2010, 2019)) %>% 
      select(ano, regiao, osc_per_capita_regiao) %>% 
      distinct(ano, regiao, .keep_all = T) %>% 
      filter(!is.na(regiao)) %>% 
      left_join(df_bar_limpo) %>% 
      filter(!is.na(prop_confia)) %>% 
      gather(var, value, -ano, -regiao) %>% 
      unite(ano_var, ano, var) %>% 
      spread(ano_var, value) %>% 
      data.frame %>% 
      xlsx::write.xlsx("./tabs/Tabela_Confianca_OSCs_percapita_BAROMETRO_REGIAO.xlsx", row.names = F, sheetName = "clean", append = T)

# 3.2 Correlacao entre N de OSCs/per capita e confianca por UF, no SIPS ----

sips <- readxl::read_excel("./data/sips/SIPS Valores.xlsx")

sips_limpo <- sips %>% 
      select(regiao = REGIAO, uf = UF, confianca = Q27, renda = Q10.1, fam = Q10.2) %>% 
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

df_confia_sips <- sips_limpo %>% 
      group_by(uf) %>% 
      summarize(prop_confia = round(100*sum(confianca %in% c("Discorda bastate", "Discorda"))/n(), 1))

library(ggpubr)

sp <- ggscatter(df, x = "wt", y = "mpg",
                color = "cyl", palette = "jco",
                add = "reg.line", conf.int = TRUE)

sp + stat_cor(aes(color = cyl), label.x = 3)

fig_osc_sips_1 <- df_osc_per_capita %>% 
      filter(ano == 2010) %>% 
      select(regiao, uf, osc_per_capita_uf) %>% 
      left_join(df_confia_sips) %>% 
      filter(!is.na(uf)) %>% 
      ggplot(aes(osc_per_capita_uf, prop_confia, label = uf)) +
      geom_point(size = 2) +
      ggrepel::geom_label_repel() +
      # geom_smooth(method = "lm") +
      geom_smooth(method = "loess") +
      labs(x = "Número de OSCs por 100.000 habitantes",
           y = "Confiança interpessoal (%)") +
      tidyquant::theme_tq() +
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 14), 
            legend.position = "none")

fig_osc_sips_1 <- df_osc_per_capita %>% 
      filter(ano == 2010) %>% 
      select(regiao, uf, osc_per_capita_uf) %>% 
      left_join(df_confia_sips) %>% 
      filter(!is.na(uf)) %>% 
      ggscatter(
            x = "osc_per_capita_uf", 
            y = "prop_confia",
            add = "reg.line",  # Add regressin line
            # add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE # Add confidence interval
          ) +
      stat_cor(p.accuracy = 0.001, r.accuracy = 0.01) +
      ggrepel::geom_label_repel(aes(label = uf, alpha = .6)) +
      labs(x = "Número de OSCs por 100.000 habitantes",
           y = "Confiança interpessoal (%)") +
      tidyquant::theme_tq() +
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 14), 
            legend.position = "none")

jpeg(filename = "./figs/Figura_Confianca_OSCs_percapita_SIPS_UF.jpeg", width = 966, height = 667)
fig_osc_sips_1
dev.off()

fig_osc_sips_2 <- df_osc_per_capita %>% 
      filter(ano == 2010) %>% 
      select(regiao, uf, osc_per_capita_uf) %>% 
      left_join(df_confia_sips) %>% 
      filter(!is.na(uf)) %>% 
      ggscatter(x = "osc_per_capita_uf", 
                # label = "uf",
                y = "prop_confia",
                color = "regiao", 
                palette = "jco",
                add = "reg.line", 
                conf.int = TRUE) +
      stat_cor(aes(color = regiao), label.x = 3) +
      ggrepel::geom_label_repel(aes(label = uf, fill = regiao, alpha = .6)) +
      labs(x = "Número de OSCs por 100.000 habitantes",
           y = "Confiança interpessoal (%)") +
      tidyquant::theme_tq() +
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 14))
      
jpeg(filename = "./figs/Figura_Confianca_OSCs_percapita_SIPS_Regiao.jpeg", width = 966, height = 667)
fig_osc_sips_2
dev.off()

df_osc_per_capita %>% 
      filter(ano == 2010) %>% 
      select(regiao, uf, osc_per_capita_uf) %>% 
      left_join(df_confia_sips) %>% 
      filter(!is.na(uf)) %>% 
      data.frame %>% 
      xlsx::write.xlsx("./tabs/Tabela_Confianca_OSCs_percapita_SIPS_UF.xlsx", row.names = F, sheetName = "raw")















