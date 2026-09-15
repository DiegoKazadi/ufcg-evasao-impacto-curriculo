# =====================================================
# evasao_cumulativa.R
# Análise da evasão cumulativa por período
# VERSÃO AJUSTADA PARA A AMOSTRA FINAL DA DISSERTAÇÃO
# =====================================================

library(readr)
library(dplyr)
library(ggplot2)

projeto <- "C:/Users/Big Data/Documents/Master UFCG/Semestre 2026.1/ufcg-evasao-impacto-curriculo/Dados_Script_Usados"

pasta_processados <- file.path(projeto,"dados_processados")
pasta_resultados  <- file.path(projeto,"resultados")
pasta_tabelas     <- file.path(pasta_resultados,"tabelas")
pasta_graficos    <- file.path(pasta_resultados,"graficos")

dir.create(pasta_tabelas,recursive=TRUE,showWarnings=FALSE)
dir.create(pasta_graficos,recursive=TRUE,showWarnings=FALSE)

dados <- read_csv2(
  file.path(pasta_processados,"amostra_final_dissertacao.csv"),
  show_col_types = FALSE
)

dados <- dados %>%
  mutate(
    `Periodo de Evasao` = ifelse(`Periodo de Evasao`=="-",NA,`Periodo de Evasao`),
    Periodo_Evasao_Num = as.numeric(ifelse(
      is.na(`Periodo de Evasao`),NA,
      gsub("\\.","",`Periodo de Evasao`)
    ))
  )

codigo_semestre <- function(x){
  ano <- x %/% 10
  semestre <- x %% 10
  (ano-2011)*2 + semestre
}

dados <- dados %>%
  mutate(
    indice_ingresso = codigo_semestre(`Periodo de Ingresso`),
    indice_evasao   = ifelse(is.na(Periodo_Evasao_Num),NA,codigo_semestre(Periodo_Evasao_Num)),
    periodo_relativo = indice_evasao-indice_ingresso,
    Faixa_Etaria = case_when(
      `Idade Aproximada no Ingresso` <=17 ~ "Até 17 anos",
      `Idade Aproximada no Ingresso` <=20 ~ "18-20 anos",
      `Idade Aproximada no Ingresso` <=23 ~ "21-23 anos",
      `Idade Aproximada no Ingresso` <=26 ~ "24-26 anos",
      TRUE ~ "27 anos ou mais"
    )
  )

# =====================================================
# JANELAS DA AMOSTRA FINAL (CORRIGIDA)
# =====================================================

janelas <- list(
  `1`=list(`1999`=c(20111,20152),`2017`=c(20181,20222)),
  `2`=list(`1999`=c(20111,20152),`2017`=c(20181,20222)),
  `3`=list(`1999`=c(20111,20152),`2017`=c(20181,20222)),
  `4`=list(`1999`=c(20111,20152),`2017`=c(20181,20222))
)

filtrar_janela <- function(base,periodo){

  j <- janelas[[as.character(periodo)]]

  bind_rows(
    base %>%
      filter(`Curriculo Entrada`==1999,
             `Periodo de Ingresso`>=j$`1999`[1],
             `Periodo de Ingresso`<=j$`1999`[2]),
    base %>%
      filter(`Curriculo Entrada`==2017,
             `Periodo de Ingresso`>=j$`2017`[1],
             `Periodo de Ingresso`<=j$`2017`[2])
  )

}

# -----------------------------------------------------------------
# MANTENHA AQUI O BLOCO ORIGINAL DA EVASÃO CUMULATIVA
# (tabelas e gráficos já validados)
# -----------------------------------------------------------------

gerar_tabela_categoria <- function(variavel,arquivo){

  lista <- list()

  periodos_validos <- c(
    "2011.1","2011.2","2012.1","2012.2","2013.1",
    "2013.2","2014.1","2014.2","2015.1","2015.2",
    "2018.1","2018.2","2019.1","2019.2","2020.1",
    "2020.2","2021.1","2021.2","2022.1","2022.2"
  )

  for(p in 1:4){

    base <- filtrar_janela(dados,p)

    if(variavel=="Periodo de Evasao"){
      base <- base %>%
        filter(`Periodo de Evasao` %in% periodos_validos)
    }

    ingressantes <- base %>%
      group_by(`Curriculo Entrada`,
               Categoria=.data[[variavel]]) %>%
      summarise(Ingressantes=n(),.groups="drop")

    evadidos <- base %>%
      filter(periodo_relativo>=0,
             periodo_relativo<=(p-1)) %>%
      group_by(`Curriculo Entrada`,
               Categoria=.data[[variavel]]) %>%
      summarise(Evadidos=n(),.groups="drop")

    lista[[p]] <- ingressantes %>%
      left_join(evadidos,
                by=c("Curriculo Entrada","Categoria")) %>%
      mutate(
        Evadidos=coalesce(Evadidos,0L),
        Taxa=round(100*Evadidos/Ingressantes,2),
        Periodo=p
      )

  }

  resultado <- bind_rows(lista)

  write_csv2(
    resultado,
    file.path(pasta_tabelas,arquivo)
  )

  print(resultado)

}

gerar_tabela_categoria("Sexo","sexo_todos_periodos_cumulativo.csv")
gerar_tabela_categoria("Faixa_Etaria","faixa_etaria_todos_periodos_cumulativo.csv")
gerar_tabela_categoria("Tipo de Evasao","tipo_evasao_todos_periodos_cumulativo.csv")
gerar_tabela_categoria("Periodo de Evasao","periodo_evasao_todos_periodos_cumulativo.csv")

cat("\n===========================================\n")
cat("VERSÃO AJUSTADA PARA A AMOSTRA FINAL\n")
cat("===========================================\n")
