# =========================================================
# 5.7.5 - ANÁLISE COMPARATIVA DE DISCIPLINAS
# EXPLORAÇÃO INICIAL DA BASE ENROLLMENTS
#
# Objetivo:
# 1. Verificar estrutura da nova base
# 2. Validar registration
# 3. Comparar registrations com a amostra final
# 4. Verificar subjectCode
# 5. Preparar o relacionamento com as tabelas de disciplinas
# =========================================================

rm(list = ls())

library(readr)
library(dplyr)
library(stringr)

options(scipen = 999)

# =========================================================
# 1. DIRETÓRIOS
# =========================================================

projeto <- "C:/Users/Big Data/Documents/Master UFCG/Semestre 2026.1/ufcg-evasao-impacto-curriculo/Dados_Script_Usados"

pasta_dados <- file.path(
  projeto,
  "dados"
)

pasta_processados <- file.path(
  projeto,
  "dados_processados"
)

# =========================================================
# 2. CARREGAR A AMOSTRA FINAL
# =========================================================

amostra <- read_csv2(
  file.path(
    pasta_processados,
    "amostra_final_dissertacao.csv"
  ),
  show_col_types = FALSE
)

# =========================================================
# 3. CARREGAR ENROLLMENTS
# =========================================================

enrollments <- read_csv2(
  file.path(
    pasta_dados,
    "enrollments.csv"
  ),
  col_types = cols(
    registration = col_character(),
    subjectCode = col_character(),
    term = col_character(),
    classId = col_character(),
    credits = col_character(),
    grade = col_character(),
    status = col_character()
  )
)

# =========================================================
# 4. PRIMEIRA INSPEÇÃO
# =========================================================

cat("\n=========================================================\n")
cat("ENROLLMENTS - ESTRUTURA\n")
cat("=========================================================\n")

cat("\nLinhas:", nrow(enrollments), "\n")
cat("Colunas:", ncol(enrollments), "\n")

cat("\nNomes das colunas:\n")
print(names(enrollments))

cat("\nPrimeiras linhas:\n")
print(head(enrollments))

cat("\nEstrutura:\n")
glimpse(enrollments)