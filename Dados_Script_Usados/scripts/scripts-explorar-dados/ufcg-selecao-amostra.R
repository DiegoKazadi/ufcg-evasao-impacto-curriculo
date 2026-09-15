library(readr)
library(dplyr)

pasta_dados <- "C:/Users/Big Data/Documents/Master UFCG/Semestre 2026.1/ufcg-evasao-impacto-curriculo/Dados_Script_Usados/dados"

arquivo <- file.path(
  pasta_dados,
  "alunos-final.csv"
)

alunos <- read_csv(
  arquivo,
  show_col_types = FALSE
)

# remover espaços extras dos nomes

names(alunos) <- trimws(names(alunos))

# renomear última coluna (Ingressantes)

names(alunos)[15] <- "Ingressantes"

# converter colunas numéricas

alunos <- alunos %>%
  mutate(
    `Currículo Entrada` = as.numeric(`Currículo Entrada`),
    `Período de Ingresso` = as.numeric(`Período de Ingresso`)
  )

# seleção da amostra da defesa

amostra_final <- alunos %>%
  filter(
    (`Currículo Entrada` == 1999 &
       `Período de Ingresso` >= 2011.1 &
       `Período de Ingresso` <= 2015.2)
    |
      (`Currículo Entrada` == 2017 &
         `Período de Ingresso` >= 2018.1 &
         `Período de Ingresso` <= 2022.2)
  )

# ingressantes por período

tabela_periodos <- amostra_final %>%
  group_by(
    `Currículo Entrada`,
    `Período de Ingresso`
  ) %>%
  summarise(
    Ingressantes = sum(Ingressantes),
    .groups = "drop"
  ) %>%
  arrange(
    `Currículo Entrada`,
    `Período de Ingresso`
  )

print(tabela_periodos, n = Inf)

# totais

tabela_periodos %>%
  group_by(`Currículo Entrada`) %>%
  summarise(
    Total = sum(Ingressantes)
  )
