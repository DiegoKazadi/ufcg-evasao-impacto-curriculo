# Seleção da amostra final da dissertação

library(readr)
library(dplyr)

# Diretório dos dados

pasta_dados <- "C:/Users/Big Data/Documents/Master UFCG/Semestre 2026.1/ufcg-evasao-impacto-curriculo/Dados_Script_Usados/dados"

arquivo <- file.path( pasta_dados,"alunos-final.csv")


# Carregar dados

alunos <- read_csv( arquivo,  show_col_types = FALSE)

# Verificar estrutura da base


cat("ESTRUTURA DA BASE\n")
cat("=====================================\n")

print(names(alunos))

cat("\nTotal de registros:", nrow(alunos), "\n")

# =====================================================

# Seleção da amostra utilizada na defesa

# =====================================================

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

# Total da amostra

cat("\n=====================================\n")
cat("TOTAL DA AMOSTRA\n")
cat("=====================================\n")

cat("Total:", nrow(amostra_final), "\n")

# =====================================================

# Quantidade por currículo

# =====================================================

cat("\n=====================================\n")
cat("POR CURRÍCULO DE ENTRADA\n")
cat("=====================================\n")

print(
  amostra_final %>%
    count(`Currículo Entrada`)
)

# =====================================================

# Quantidade por período

# =====================================================

cat("\n=====================================\n")
cat("INGRESSANTES POR PERÍODO\n")
cat("=====================================\n")

tabela_periodos <- amostra_final %>%
  count(
    `Currículo Entrada`,
    `Período de Ingresso`,
    name = "Ingressantes"
  ) %>%
  arrange(
    `Currículo Entrada`,
    `Período de Ingresso`
  )

print(tabela_periodos, n = Inf)

# =====================================================

# Totais por currículo

# =====================================================

cat("\n=====================================\n")
cat("TOTAIS POR CURRÍCULO\n")
cat("=====================================\n")

totais <- tabela_periodos %>%
  group_by(`Currículo Entrada`) %>%
  summarise(
    Total = sum(Ingressantes),
    .groups = "drop"
  )

print(totais)

# =====================================================

# Salvar amostra

# =====================================================

write_csv(
  amostra_final,
  file.path(
    pasta_dados,
    "amostra_final_dissertacao.csv"
  )
)

cat("\nArquivo salvo com sucesso!\n")

readLines(arquivo, n = 5)
