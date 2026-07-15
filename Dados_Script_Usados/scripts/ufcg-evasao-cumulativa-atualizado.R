# =====================================================
# evasao_cumulativa.R
# Análise da evasão cumulativa por período
# + Tabelas demográficas e acadêmicas
# =====================================================

library(readr)
library(dplyr)
library(ggplot2)

# =====================================================
# Diretórios
# =====================================================

projeto <- "C:/Users/Big Data/Documents/Master UFCG/Semestre 2026.1/ufcg-evasao-impacto-curriculo/Dados_Script_Usados"

pasta_processados <- file.path(projeto, "dados_processados")
pasta_resultados  <- file.path(projeto, "resultados")
pasta_tabelas     <- file.path(pasta_resultados, "tabelas")
pasta_graficos    <- file.path(pasta_resultados, "graficos")

dir.create(pasta_tabelas, recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_graficos, recursive = TRUE, showWarnings = FALSE)

dados <- read_csv2(
  file.path(pasta_processados, "amostra_final_dissertacao.csv"),
  show_col_types = FALSE
)

dados <- dados %>%
  mutate(
    `Periodo de Evasao` = ifelse(`Periodo de Evasao` == "-", NA, `Periodo de Evasao`),
    Periodo_Evasao_Num = as.numeric(ifelse(
      is.na(`Periodo de Evasao`),
      NA,
      gsub("\\.", "", `Periodo de Evasao`)
    ))
  )

codigo_semestre <- function(x){
  ano <- x %/% 10
  semestre <- x %% 10
  (ano - 2011) * 2 + semestre
}

dados <- dados %>%
  mutate(
    indice_ingresso = codigo_semestre(`Periodo de Ingresso`),
    indice_evasao   = ifelse(is.na(Periodo_Evasao_Num), NA,
                             codigo_semestre(Periodo_Evasao_Num)),
    periodo_relativo = indice_evasao - indice_ingresso,
    Faixa_Etaria = case_when(
      `Idade Aproximada no Ingresso` <= 17 ~ "Até 17 anos",
      `Idade Aproximada no Ingresso` <= 20 ~ "18-20 anos",
      `Idade Aproximada no Ingresso` <= 23 ~ "21-23 anos",
      `Idade Aproximada no Ingresso` <= 26 ~ "24-26 anos",
      TRUE ~ "27 anos ou mais"
    )
  )

formatar_periodo <- function(x){
  x <- as.character(x)
  paste0(substr(x,1,4),".",substr(x,5,5))
}

janelas <- list(
  `1` = list(`1999`=c(20111,20172), `2017`=c(20181,20231)),
  `2` = list(`1999`=c(20111,20161), `2017`=c(20181,20222)),
  `3` = list(`1999`=c(20111,20152), `2017`=c(20181,20221)),
  `4` = list(`1999`=c(20111,20142), `2017`=c(20181,20212))
)

filtrar_janela <- function(base, periodo){
  j <- janelas[[as.character(periodo)]]
  bind_rows(
    base %>% filter(`Curriculo Entrada`==1999,
                    `Periodo de Ingresso`>=j$`1999`[1],
                    `Periodo de Ingresso`<=j$`1999`[2]),
    base %>% filter(`Curriculo Entrada`==2017,
                    `Periodo de Ingresso`>=j$`2017`[1],
                    `Periodo de Ingresso`<=j$`2017`[2])
  )
}

# -------------------------------------------------------------------
# MANTENHA AQUI O BLOCO ORIGINAL DE CÁLCULO DAS TABELAS E GRÁFICOS
# DE EVASÃO CUMULATIVA (sem alterações).
# -------------------------------------------------------------------

# =====================================================
# NOVAS TABELAS (Ingressantes, Evadidos e Taxa)
# =====================================================

gerar_tabela_categoria <- function(variavel, arquivo){

  lista <- list()

  for(p in 1:4){

    base <- filtrar_janela(dados, p)

    ingressantes <- base %>%
      group_by(
        `Curriculo Entrada`,
        Categoria = .data[[variavel]]
      ) %>%
      summarise(
        Ingressantes = n(),
        .groups="drop"
      )

    evadidos <- base %>%
      filter(
        periodo_relativo >= 0,
        periodo_relativo <= (p-1)
      ) %>%
      group_by(
        `Curriculo Entrada`,
        Categoria = .data[[variavel]]
      ) %>%
      summarise(
        Evadidos = n(),
        .groups="drop"
      )

    tabela <- ingressantes %>%
      left_join(
        evadidos,
        by=c("Curriculo Entrada","Categoria")
      ) %>%
      mutate(
        Evadidos = coalesce(Evadidos,0L),
        Taxa = round(100*Evadidos/Ingressantes,2),
        Periodo = p
      )

    lista[[p]] <- tabela

  }

  resultado <- bind_rows(lista)

  write_csv2(
    resultado,
    file.path(pasta_tabelas, arquivo)
  )

  print(resultado)

}

gerar_tabela_categoria(
  "Sexo",
  "sexo_todos_periodos_cumulativo.csv"
)

gerar_tabela_categoria(
  "Faixa_Etaria",
  "faixa_etaria_todos_periodos_cumulativo.csv"
)

gerar_tabela_categoria(
  "Tipo de Evasao",
  "tipo_evasao_todos_periodos_cumulativo.csv"
)

gerar_tabela_categoria(
  "Periodo de Evasao",
  "periodo_evasao_todos_periodos_cumulativo.csv"
)

cat("\n=========================================\n")
cat("TABELAS DEMOGRÁFICAS E ACADÊMICAS GERADAS\n")
cat("=========================================\n")

