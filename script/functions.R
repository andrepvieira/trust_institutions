limpa_latinobarometro <- function(base) {
      if (isTRUE(base$numero[1] == 1995) | isTRUE(base$NUMINVES[1] == 1995)) {
            return(
                  base %>% 
                        filter(pais %in% c(32, 76, 152, 484)) %>% 
                        mutate(confianca_pessoal = NA) %>% 
                        select(ano = numero, 
                               pais, 
                               regiao = region, 
                               confianca_pessoal,
                               confianca_policia = p27h,
                               confianca_judiciario = p27d,
                               problema = p13, 
                               educacao = s20, 
                               wt) %>% 
                        mutate_if(is.labelled, to_factor) %>% 
                        mutate(wt = as.numeric(wt))
            )
      }
      else if (isTRUE(base$numero[1] == 1996) | isTRUE(base$numinves[1] == 1996) | isTRUE(base$NUMINVES[1] == 1996)) {
            return(
                  base %>% 
                        filter(pais %in% c(32, 76, 152, 170, 484)) %>% 
                        select(ano = numero, 
                               pais, 
                               regiao = region, 
                               confianca_pessoal = p12,
                               confianca_policia = p33h,
                               confianca_judiciario = p33d,
                               problema = p11, 
                               educacao = s16a, 
                               wt) %>% 
                        mutate_if(is.labelled, to_factor) %>% 
                        mutate(wt = as.numeric(wt))
            )
      }
      else if (isTRUE(base$numinves[1] == 1997) | isTRUE(base$NUMINVES[1] == 1997)) {
            return(
                  base %>%
                        filter(idenpa %in% c(32, 76, 152, 170, 484)) %>%
                        select(ano = numinves,
                               pais = idenpa,
                               regiao = reg,
                               confianca_pessoal = sp21,
                               confianca_policia = sp63e,
                               confianca_judiciario = sp63c,
                               problema = sp8,
                               educacao = s12a,
                               wt) %>%
                        mutate_if(is.labelled, to_factor) %>% 
                        mutate(wt = as.numeric(wt))
            )
      }
      else if (isTRUE(base$numinves[1] == 1998) | isTRUE(base$NUMINVES[1] == 1999)) {
            return(
                  base %>%
                        filter(idenpa %in% c(32, 76, 152, 170, 484)) %>%
                        select(ano = numinves,
                               pais = idenpa,
                               regiao = reg,
                               confianca_pessoal = sp20,
                               confianca_policia = sp38e,
                               confianca_judiciario = sp38c,
                               problema = sp8,
                               educacao = s14a,
                               wt = pondera) %>%
                        mutate_if(is.labelled, to_factor) %>% 
                        mutate(wt = as.numeric(wt))
            )
      }
      if (isTRUE(base$NUMINVES[1] == 2000) | isTRUE(base$numinves[1] == 2000)) {
            return(
                  base %>%
                        clean_names() %>%
                        filter(idenpa %in% c(32, 76, 152, 170, 484)) %>%
                        select(ano = numinves,
                               pais = idenpa,
                               regiao = reg,
                               confianca_pessoal = p17st,
                               confianca_policia = p35st_e,
                               confianca_judiciario = p35st_c,
                               problema = p12st,
                               educacao = reeduc1,
                               wt) %>%
                        mutate_if(is.labelled, to_factor) %>% 
                        mutate(wt = as.numeric(wt))
            )
      }
      else if (isTRUE(base$NUMINVES[1] == 2001) | isTRUE(base$numinves[1] == 2001)) {
            return(
                  base %>%
                        clean_names() %>%
                        filter(idenpa %in% c(32, 76, 152, 170, 484)) %>%
                        mutate(confianca_judiciario = NA) %>% 
                        select(ano = numinves,
                               pais = idenpa,
                               regiao = reg,
                               confianca_pessoal = p42st,
                               confianca_policia = p61ste,
                               confianca_judiciario,
                               problema = p13st,
                               educacao = reeduc1,
                               wt) %>%
                        mutate_if(is.labelled, to_factor) %>% 
                        mutate(wt = as.numeric(wt))
            )
      }
      if (isTRUE(base$NUMINVES[1] == 2002) | isTRUE(base$numinves[1] == 2002)) {
            return(
                  base %>%
                        clean_names() %>%
                        filter(idenpa %in% c(32, 76, 152, 170, 484)) %>%
                        mutate(confianca_policia = NA) %>% 
                        select(ano = numinves,
                               pais = idenpa,
                               regiao = reg,
                               confianca_pessoal = p29st,
                               confianca_policia,
                               confianca_judiciario = p34stc,
                               problema = p4st,
                               educacao = reeduc1,
                               wt) %>%
                        mutate_if(is.labelled, to_factor) %>% 
                        mutate(wt = as.numeric(wt))
            )
      }
      else if (isTRUE(base$NUMINVES[1] == 2003) | isTRUE(base$numinves[1] == 2003)) {
            return(
                  base %>%
                        clean_names() %>%
                        filter(idenpa %in% c(32, 76, 152, 170, 484)) %>%
                        select(ano = numinves,
                               pais = idenpa,
                               regiao = reg,
                               confianca_pessoal = p20st,
                               confianca_policia = p21stb,
                               confianca_judiciario = p21ste,
                               problema = p8st,
                               educacao = s18,
                               wt) %>%
                        mutate_if(is.labelled, to_factor) %>% 
                        mutate(wt = as.numeric(wt))
            )
      }
      else if (isTRUE(base$NUMINVES[1] == 2004) | isTRUE(base$numinves[1] == 2004)) {
            return(
                  base %>%
                        clean_names() %>%
                        filter(idenpa %in% c(32, 76, 152, 170, 484)) %>%
                        mutate(confianca_policia = NA) %>% 
                        select(ano = numinves,
                               pais = idenpa,
                               regiao = reg,
                               confianca_pessoal = p43st,
                               confianca_policia,
                               confianca_judiciario = p34stb,
                               problema = p10st,
                               educacao = reeduc1,
                               wt) %>%
                        mutate_if(is.labelled, to_factor) %>% 
                        mutate(wt = as.numeric(wt))
            )
      }
      else if (isTRUE(base$NUMINVES[1] == 2005) | isTRUE(base$numinves[1] == 2005)) {
            return(
                  base %>%
                        clean_names() %>%
                        filter(idenpa %in% c(32, 76, 152, 170, 484)) %>%
                        select(ano = numinves,
                               pais = idenpa,
                               regiao = reg,
                               confianca_pessoal = p14st,
                               confianca_policia = p42stf,
                               confianca_judiciario = p42std,
                               problema = p8st,
                               educacao = s11,  # precisa recodificar
                               wt) %>%
                        mutate_if(is.labelled, to_factor) %>% 
                        mutate(wt = as.numeric(wt))
            )
      }
      else if (isTRUE(base$NUMINVES[1] == 2006) | isTRUE(base$numinves[1] == 2006)) {
            return(
                  base %>%
                        clean_names() %>%
                        filter(idenpa %in% c(32, 76, 152, 170, 484)) %>%
                        select(ano = numinves,
                               pais = idenpa,
                               regiao = reg,
                               confianca_pessoal = p45st,
                               confianca_policia = p24st_a,
                               confianca_judiciario = p24st_d,
                               problema = p10st,
                               educacao = reeduc1, # precisa recodificar?
                               wt) %>%
                        mutate_if(is.labelled, to_factor) %>% 
                        mutate(wt = as.numeric(wt))
            )
      }
      else if (isTRUE(base$NUMINVES[1] == 2007) | isTRUE(base$numinves[1] == 2007)) {
            return(
                  base %>%
                        clean_names() %>%
                        filter(idenpa %in% c(32, 76, 152, 170, 484)) %>%
                        mutate(confianca_policia = NA) %>% 
                        select(ano = numinves,
                               pais = idenpa,
                               regiao = reg,
                               confianca_pessoal = p23st,
                               confianca_policia,
                               confianca_judiciario = p24st_d,
                               problema = p7st,
                               educacao = reeduc1,  # precisa recodificar?
                               wt) %>%
                        mutate_if(is.labelled, to_factor) %>% 
                        mutate(wt = as.numeric(wt))
            )
      }
      else if (isTRUE(base$numinves[1] == 2008)) {
            return(
                  base %>%
                        filter(idenpa %in% c(32, 76, 152, 170, 484)) %>%
                        mutate(confianca_policia = NA) %>% 
                        select(ano = numinves,
                               pais = idenpa,
                               regiao = reg,
                               confianca_pessoal = p21wvsst,
                               confianca_policia,
                               confianca_judiciario = p28st_b,
                               problema = p2st, # precisa recodificar?
                               educacao = reeduc1,
                               wt) %>%
                        mutate_if(is.labelled, to_factor) %>% 
                        mutate(wt = as.numeric(wt))
            )
      }
      else if (isTRUE(base$numinves[1] == 2009)) {
            return(
                  base %>%
                        clean_names() %>%
                        filter(idenpa %in% c(32, 76, 152, 170, 484)) %>%
                        mutate(confianca_policia = NA) %>% 
                        select(ano = numinves,
                               pais = idenpa,
                               regiao = reg,
                               confianca_pessoal = p58st,
                               confianca_policia,
                               confianca_judiciario = p26st_b,
                               problema = p2st,
                               educacao = reeduc1,
                               wt) %>%
                        mutate_if(is.labelled, to_factor) %>% 
                        mutate(wt = as.numeric(wt))
            )
      }
      else if (isTRUE(base$numinves[1] == 2010)) {
            return(
                  base %>%
                        clean_names() %>%
                        filter(idenpa %in% c(32, 76, 152, 170, 484)) %>%
                        mutate(confianca_policia = NA) %>% 
                        select(ano = numinves,
                               pais = idenpa,
                               regiao = reg,
                               confianca_pessoal = p55st,
                               confianca_policia,
                               confianca_judiciario = p20st_b,
                               problema = p2st,
                               educacao = reeduc1,
                               wt) %>%
                        mutate_if(is.labelled, to_factor) %>% 
                        mutate(wt = as.numeric(wt))
            )
      }
      else if (isTRUE(base$numinves[1] == 16)) {
            return(
                  base %>%
                        clean_names() %>%
                        filter(idenpa %in% c(32, 76, 152, 170, 484)) %>%
                        mutate(confianca_policia = NA) %>% 
                        select(ano = numinves,
                               pais = idenpa,
                               regiao = reg,
                               confianca_pessoal = p25st,
                               confianca_policia,
                               confianca_judiciario = p22st_b,
                               problema = p2st,
                               educacao = reeduc1,
                               wt) %>%
                        mutate_if(is.labelled, to_factor) %>% 
                        mutate(wt = as.numeric(wt))
            )
      }
      else if (isTRUE(base$numinves[1] == 17)) {
            return(
                  base %>%
                        clean_names() %>%
                        filter(idenpa %in% c(32, 76, 152, 170, 484)) %>%
                        mutate(confianca_policia = NA) %>% 
                        select(ano = numinves,
                               pais = idenpa,
                               regiao = reg,
                               confianca_pessoal = p29stgbs,
                               confianca_policia,
                               confianca_judiciario = p26tgb_e,
                               problema = p9stgbs,
                               educacao = reeduc_1,
                               wt) %>%
                        mutate_if(is.labelled, to_factor) %>% 
                        mutate(wt = as.numeric(wt))
            )
      }
      else if (isTRUE(base$numinves[1] == 18)) {
            return(
                  base %>%
                        clean_names() %>%
                        filter(idenpa %in% c(32, 76, 152, 170, 484)) %>%
                        select(ano = numinves,
                               pais = idenpa,
                               regiao = reg,
                               confianca_pessoal = p15stgbs,
                               confianca_policia = p16tgb_b,
                               confianca_judiciario = p16st_h,
                               problema = p9stgbs,
                               educacao = reeduc_1,
                               wt) %>%
                        mutate_if(is.labelled, to_factor) %>% 
                        mutate(wt = as.numeric(wt))
            )
      }
      else if (isTRUE(base$numinves[1] == 2016)) {
            return(
                  base %>%
                        clean_names() %>%
                        filter(idenpa %in% c(32, 76, 152, 170, 484)) %>%
                        select(ano = numinves,
                               pais = idenpa,
                               regiao = reg,
                               confianca_pessoal = p12stgbs,
                               confianca_policia = p13stgbsb,
                               confianca_judiciario = p13stf,
                               problema = p3stgbs,
                               educacao = reeduc_1,
                               wt) %>%
                        mutate_if(is.labelled, to_factor) %>% 
                        mutate(wt = as.numeric(wt))
            )
      }
      else if (isTRUE(base$numinves[1] == 2017)) {
            return(
                  base %>%
                        clean_names() %>%
                        filter(idenpa %in% c(32, 76, 152, 170, 484)) %>%
                        select(ano = numinves,
                               pais = idenpa,
                               regiao = reg,
                               confianca_pessoal = p13stgbs,
                               confianca_policia = p14stgbs_b,
                               confianca_judiciario = p14st_f,
                               problema = p3stgbs,
                               educacao = reeduc_1,
                               wt) %>%
                        mutate_if(is.labelled, to_factor) %>% 
                        mutate(wt = as.numeric(wt))
            )
      }
      else if (isTRUE(base$numinves[1] == 2018) | isTRUE(base$NUMINVES[1] == 2018)) {
            return(
                  base %>% # categorias em espanhol!
                        clean_names() %>% 
                        filter(idenpa %in% c(32, 76, 152, 170, 484)) %>% 
                        select(ano = numinves, 
                               pais = idenpa, 
                               regiao = reg, 
                               confianca_pessoal = p11stgbs,
                               confianca_policia = p15stgbsc_f,
                               confianca_judiciario = p15stgbsc_b,
                               problema = p3stgbsc, 
                               educacao = reeduc_1,
                               wt) %>% 
                        mutate_if(is.labelled, to_factor) %>% 
                        mutate(wt = as.numeric(wt))
            )
      }
}
