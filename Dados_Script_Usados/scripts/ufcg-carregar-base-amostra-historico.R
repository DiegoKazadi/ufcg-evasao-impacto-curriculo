#=========================================================
# DISSERTAÇÃO - ANÁLISE COMPARATIVA ENTRE DISCIPLINAS
# Seção 5.7.5
#
# Etapa 1 - Carregamento das bases
#=========================================================

rm(list = ls())

library(readr)
library(dplyr)

options(scipen = 999)

#=========================================================
# 1. Diretórios
#=========================================================

projeto <- "C:/Users/Big Data/Documents/Master UFCG/Semestre 2026.1/ufcg-evasao-impacto-curriculo/Dados_Script_Usados"

pasta_dados <- file.path(
  projeto,
  "dados"
)

pasta_processados <- file.path(
  projeto,
  "dados_processados"
)

pasta_resultados <- file.path(
  projeto,
  "resultados"
)

pasta_tabelas <- file.path(
  pasta_resultados,
  "tabelas"
)

#=========================================================
# 2. Carregar AMOSTRA FINAL
#=========================================================

amostra_final <- read_csv2(
  file.path(
    pasta_processados,
    "amostra_final_dissertacao.csv"
  ),
  show_col_types = FALSE
)

#=========================================================
# 3. Carregar HISTÓRICO ACADÊMICO
#=========================================================

historico <- read_csv2(
  file.path(
    pasta_dados,
    "historico.csv"
  ),
  show_col_types = FALSE
)

#=========================================================
# 4. Carregar ESTRUTURA CURRICULAR 1999
#=========================================================

disciplinas_1999 <- read_csv2(
  file.path(
    pasta_dados,
    "disciplinas_curriculo_1999.csv"
  ),
  show_col_types = FALSE
)

#=========================================================
# 5. Carregar ESTRUTURA CURRICULAR 2017
#=========================================================

disciplinas_2017 <- read_csv2(
  file.path(
    pasta_dados,
    "disciplinas_curriculo_2017.csv"
  ),
  show_col_types = FALSE
)

#=========================================================
# 6. CONFIRMAÇÃO DAS BASES CARREGADAS
#=========================================================

cat("\n=========================================================\n")
cat("BASES CARREGADAS\n")
cat("=========================================================\n")

cat("\n1. AMOSTRA FINAL\n")
cat("Linhas:", nrow(amostra_final), "\n")
cat("Colunas:", ncol(amostra_final), "\n")
cat("Matrículas distintas:",
    n_distinct(amostra_final$Matricula),
    "\n")

cat("\n2. HISTÓRICO ACADÊMICO\n")
cat("Linhas:", nrow(historico), "\n")
cat("Colunas:", ncol(historico), "\n")
cat("Matrículas distintas:",
    n_distinct(historico$MATRICULA),
    "\n")

cat("\n3. CURRÍCULO 1999\n")
cat("Linhas:", nrow(disciplinas_1999), "\n")
cat("Colunas:", ncol(disciplinas_1999), "\n")

cat("\n4. CURRÍCULO 2017\n")
cat("Linhas:", nrow(disciplinas_2017), "\n")
cat("Colunas:", ncol(disciplinas_2017), "\n")

#=========================================================
# 7. VISUALIZAÇÃO DAS COLUNAS
#=========================================================

cat("\n=========================================================\n")
cat("COLUNAS DAS BASES\n")
cat("=========================================================\n")

cat("\n--- AMOSTRA FINAL ---\n")
print(names(amostra_final))

cat("\n--- HISTÓRICO ---\n")
print(names(historico))

cat("\n--- CURRÍCULO 1999 ---\n")
print(names(disciplinas_1999))

cat("\n--- CURRÍCULO 2017 ---\n")
print(names(disciplinas_2017))

#=========================================================
# 8. PRIMEIRAS LINHAS
#=========================================================

cat("\n=========================================================\n")
cat("PRIMEIRAS 5 LINHAS DAS BASES\n")
cat("=========================================================\n")

cat("\n--- AMOSTRA FINAL ---\n")
print(head(amostra_final, 5))

cat("\n--- HISTÓRICO ---\n")
print(head(historico, 5))

cat("\n--- CURRÍCULO 1999 ---\n")
print(head(disciplinas_1999, 5))

cat("\n--- CURRÍCULO 2017 ---\n")
print(head(disciplinas_2017, 5))

#=========================================================
# FIM DA ETAPA 1
#=========================================================

cat("\n=========================================================\n")
cat("ETAPA 1 - BASES CARREGADAS COM SUCESSO\n")
cat("=========================================================\n")