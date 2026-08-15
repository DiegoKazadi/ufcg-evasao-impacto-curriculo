#=========================================================
# DISSERTAÇÃO - ANÁLISE COMPARATIVA ENTRE DISCIPLINAS
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

#=========================================================
# 2. Carregar amostra final da dissertação
#=========================================================

amostra_final <- read_csv2(
  file.path(
    pasta_processados,
    "amostra_final_dissertacao.csv"
  ),
  show_col_types = FALSE
)

#=========================================================
# 3. Carregar tabela de histórico
#=========================================================

tabela_historico <- read_csv2(
  file.path(
    pasta_dados,
    "tabela_historico.csv"
  ),
  show_col_types = FALSE
)

#=========================================================
# 4. Carregar disciplinas - Currículo 1999
#=========================================================

disciplinas_1999 <- read_csv2(
  file.path(
    pasta_dados,
    "disciplinas_curriculo_1999.csv"
  ),
  show_col_types = FALSE
)

#=========================================================
# 5. Carregar disciplinas - Currículo 2017
#=========================================================

disciplinas_2017 <- read_csv2(
  file.path(
    pasta_dados,
    "disciplinas_curriculo_2017.csv"
  ),
  show_col_types = FALSE
)

#=========================================================
# 6. Verificação do carregamento
#=========================================================

cat("\n=========================================================\n")
cat("BASES CARREGADAS\n")
cat("=========================================================\n")

cat("\n1. AMOSTRA FINAL\n")
cat("Linhas:", nrow(amostra_final), "\n")
cat("Colunas:", ncol(amostra_final), "\n")

cat("\n2. TABELA HISTÓRICO\n")
cat("Linhas:", nrow(tabela_historico), "\n")
cat("Colunas:", ncol(tabela_historico), "\n")

cat("\n3. DISCIPLINAS CURRÍCULO 1999\n")
cat("Linhas:", nrow(disciplinas_1999), "\n")
cat("Colunas:", ncol(disciplinas_1999), "\n")

cat("\n4. DISCIPLINAS CURRÍCULO 2017\n")
cat("Linhas:", nrow(disciplinas_2017), "\n")
cat("Colunas:", ncol(disciplinas_2017), "\n")

#=========================================================

cat("\n=======================================================\n")
cat("CARREGAMENTO CONCLUÍDO\n")
cat("=========================================================\n")


#=========================================================
# Etapa 2 - Visualização e inspeção das bases
#=========================================================

#=========================================================
# 1. AMOSTRA FINAL DA DISSERTAÇÃO
#=========================================================

cat("\n=========================================================\n")
cat("1. AMOSTRA FINAL DA DISSERTAÇÃO\n")
cat("=========================================================\n")

cat("\nColunas:\n")
print(names(amostra_final))

cat("\nPrimeiras 5 linhas:\n")
print(head(amostra_final, 5))


#=========================================================
# 2. TABELA DE HISTÓRICO
#=========================================================

cat("\n=========================================================\n")
cat("2. TABELA DE HISTÓRICO\n")
cat("=========================================================\n")

cat("\nColunas:\n")
print(names(tabela_historico))

cat("\nPrimeiras 5 linhas:\n")
print(head(tabela_historico, 5))


#=========================================================
# 3. DISCIPLINAS - CURRÍCULO 1999
#=========================================================

cat("\n=========================================================\n")
cat("3. DISCIPLINAS - CURRÍCULO 1999\n")
cat("=========================================================\n")

cat("\nColunas:\n")
print(names(disciplinas_1999))

cat("\nPrimeiras 5 linhas:\n")
print(head(disciplinas_1999, 5))


#=========================================================
# 4. DISCIPLINAS - CURRÍCULO 2017
#=========================================================

cat("\n=========================================================\n")
cat("4. DISCIPLINAS - CURRÍCULO 2017\n")
cat("=========================================================\n")

cat("\nColunas:\n")
print(names(disciplinas_2017))

cat("\nPrimeiras 5 linhas:\n")
print(head(disciplinas_2017, 5))


#=========================================================
# FIM DA ETAPA 2
#=========================================================

cat("\n=========================================================\n")
cat("ETAPA 2 CONCLUÍDA\n")
cat("=========================================================\n")