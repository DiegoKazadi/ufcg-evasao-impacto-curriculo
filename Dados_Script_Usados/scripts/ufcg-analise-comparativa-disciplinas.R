#=========================================================
# DISSERTAÇÃO - ANÁLISE COMPARATIVA ENTRE DISCIPLINAS
# Etapa 1 - Exploração e preparação das bases
#=========================================================

rm(list = ls())

library(readr)
library(dplyr)
library(tidyr)

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

dir.create(
  pasta_processados,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  pasta_tabelas,
  recursive = TRUE,
  showWarnings = FALSE
)


#=========================================================
# 2. Carregar amostra final da dissertação
#=========================================================

amostra <- read_csv2(
  file.path(
    pasta_processados,
    "amostra_final_dissertacao.csv"
  ),
  show_col_types = FALSE
)


#=========================================================
# 3. Exploração da amostra final
#=========================================================

cat("\n=========================================================\n")
cat("AMOSTRA FINAL DA DISSERTAÇÃO\n")
cat("=========================================================\n")

cat("\nNúmero de registros:", nrow(amostra), "\n")

cat("\nNúmero de colunas:", ncol(amostra), "\n")

cat("\nColunas:\n")
print(names(amostra))


#=========================================================
# 4. Verificar distribuição por currículo
#=========================================================

cat("\n=========================================================\n")
cat("DISTRIBUIÇÃO DA AMOSTRA POR CURRÍCULO\n")
cat("=========================================================\n")

distribuicao_curriculo <- amostra %>%
  count(curriculo, name = "ingressantes")

print(distribuicao_curriculo)


#=========================================================
# 5. Verificar períodos de ingresso por currículo
#=========================================================

cat("\n=========================================================\n")
cat("PERÍODOS DE INGRESSO POR CURRÍCULO\n")
cat("=========================================================\n")

coortes <- amostra %>%
  count(
    curriculo,
    periodo_de_ingresso,
    name = "ingressantes"
  ) %>%
  arrange(
    curriculo,
    periodo_de_ingresso
  )

print(coortes)


#=========================================================
# 6. Criar bases separadas por currículo
#=========================================================

amostra_1999 <- amostra %>%
  filter(curriculo == 1999)

amostra_2017 <- amostra %>%
  filter(curriculo == 2017)


cat("\n=========================================================\n")
cat("TAMANHO DAS AMOSTRAS POR CURRÍCULO\n")
cat("=========================================================\n")

cat("\nCurrículo 1999:", nrow(amostra_1999), "\n")
cat("Currículo 2017:", nrow(amostra_2017), "\n")
cat("Total:", nrow(amostra_1999) + nrow(amostra_2017), "\n")


#=========================================================
# 7. Verificar duplicidade de matrícula
#=========================================================

cat("\n=========================================================\n")
cat("VERIFICAÇÃO DE DUPLICIDADE DE MATRÍCULA\n")
cat("=========================================================\n")

duplicadas <- amostra %>%
  count(Matricula, name = "n") %>%
  filter(n > 1)

cat(
  "\nNúmero de matrículas duplicadas:",
  nrow(duplicadas),
  "\n"
)

if (nrow(duplicadas) > 0) {
  print(duplicadas)
}


#=========================================================
# 8. Carregar estrutura curricular 1999
#=========================================================

disciplinas_1999 <- read_csv2(
  file.path(
    pasta_dados,
    "disciplinas_curriculo_1999.csv"
  ),
  show_col_types = FALSE
)


#=========================================================
# 9. Carregar estrutura curricular 2017
#=========================================================

disciplinas_2017 <- read_csv2(
  file.path(
    pasta_dados,
    "disciplinas_curriculo_2017.csv"
  ),
  show_col_types = FALSE
)


#=========================================================
# 10. Exploração da tabela de disciplinas - currículo 1999
#=========================================================

cat("\n=========================================================\n")
cat("DISCIPLINAS - CURRÍCULO 1999\n")
cat("=========================================================\n")

cat("\nNúmero de registros:", nrow(disciplinas_1999), "\n")
cat("Número de colunas:", ncol(disciplinas_1999), "\n")

cat("\nColunas:\n")
print(names(disciplinas_1999))

cat("\nPrimeiros registros:\n")
print(head(disciplinas_1999))


#=========================================================
# 11. Exploração da tabela de disciplinas - currículo 2017
#=========================================================

cat("\n=========================================================\n")
cat("DISCIPLINAS - CURRÍCULO 2017\n")
cat("=========================================================\n")

cat("\nNúmero de registros:", nrow(disciplinas_2017), "\n")
cat("Número de colunas:", ncol(disciplinas_2017), "\n")

cat("\nColunas:\n")
print(names(disciplinas_2017))

cat("\nPrimeiros registros:\n")
print(head(disciplinas_2017))


#=========================================================
# 12. Verificar períodos ideais - currículo 1999
#=========================================================

cat("\n=========================================================\n")
cat("SEMESTRES IDEAIS - CURRÍCULO 1999\n")
cat("=========================================================\n")

semestres_1999 <- disciplinas_1999 %>%
  count(
    SEMESTRE_IDEAL,
    name = "quantidade_disciplinas"
  ) %>%
  arrange(SEMESTRE_IDEAL)

print(semestres_1999)


#=========================================================
# 13. Verificar períodos ideais - currículo 2017
#=========================================================

cat("\n=========================================================\n")
cat("SEMESTRES IDEAIS - CURRÍCULO 2017\n")
cat("=========================================================\n")

semestres_2017 <- disciplinas_2017 %>%
  count(
    SEMESTRE_IDEAL,
    name = "quantidade_disciplinas"
  ) %>%
  arrange(SEMESTRE_IDEAL)

print(semestres_2017)


#=========================================================
# 14. Salvar amostra filtrada por currículo
#=========================================================

write_csv2(
  amostra_1999,
  file.path(
    pasta_processados,
    "amostra_disciplinas_curriculo_1999.csv"
  )
)

write_csv2(
  amostra_2017,
  file.path(
    pasta_processados,
    "amostra_disciplinas_curriculo_2017.csv"
  )
)


#=========================================================
# 15. Salvar tabela dos coortes
#=========================================================

write_csv2(
  coortes,
  file.path(
    pasta_processados,
    "coortes_amostra_disciplinas.csv"
  )
)


#=========================================================
# 16. Salvar estruturas curriculares
#=========================================================

write_csv2(
  disciplinas_1999,
  file.path(
    pasta_processados,
    "disciplinas_curriculo_1999_processado.csv"
  )
)

write_csv2(
  disciplinas_2017,
  file.path(
    pasta_processados,
    "disciplinas_curriculo_2017_processado.csv"
  )
)


#=========================================================
# FIM
#=========================================================

cat("\n=========================================================\n")
cat("ETAPA 1 CONCLUÍDA\n")
cat("=========================================================\n")

cat("\nAmostra total:", nrow(amostra), "\n")
cat("Currículo 1999:", nrow(amostra_1999), "\n")
cat("Currículo 2017:", nrow(amostra_2017), "\n")

cat("\nArquivos processados salvos em:\n")
cat(pasta_processados, "\n")
