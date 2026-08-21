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

cat("\n=======================================================\n")
cat("1. AMOSTRA FINAL DA DISSERTAÇÃO\n")
cat("=========================================================\n")

cat("\nColunas:\n")
print(names(amostra_final))

cat("\nPrimeiras 5 linhas:\n")
print(head(amostra_final, 5))


#=========================================================
# 2. TABELA DE HISTÓRICO
#=========================================================

cat("\n=======================================================\n")
cat("2. TABELA DE HISTÓRICO\n")
cat("=========================================================\n")

cat("\nColunas:\n")
print(names(tabela_historico))

cat("\nPrimeiras 5 linhas:\n")
print(head(tabela_historico, 5))


#=========================================================
# 3. DISCIPLINAS - CURRÍCULO 1999
#=========================================================

cat("\n=======================================================\n")
cat("3. DISCIPLINAS - CURRÍCULO 1999\n")
cat("=========================================================\n")

cat("\nColunas:\n")
print(names(disciplinas_1999))

cat("\nPrimeiras 5 linhas:\n")
print(head(disciplinas_1999, 5))


#=========================================================
# 4. DISCIPLINAS - CURRÍCULO 2017
#=========================================================

cat("\n=======================================================\n")
cat("4. DISCIPLINAS - CURRÍCULO 2017\n")
cat("=========================================================\n")

cat("\nColunas:\n")
print(names(disciplinas_2017))

cat("\nPrimeiras 5 linhas:\n")
print(head(disciplinas_2017, 5))


#=========================================================

cat("\n=========================================================\n")
cat("ETAPA 2 CONCLUÍDA\n")
cat("=========================================================\n")

#=========================================================
# Etapa 3 - Validação da chave de relacionamento
#=========================================================

#=========================================================
# 1. Verificar tipo das variáveis de matrícula
#=========================================================

cat("\n=========================================================\n")
cat("1. TIPO DAS VARIÁVEIS DE MATRÍCULA\n")
cat("=========================================================\n")

cat(
  "\nAmostra final - Matricula:",
  class(amostra_final$Matricula),
  "\n"
)

cat(
  "Histórico - MATRICULA:",
  class(tabela_historico$MATRICULA),
  "\n"
)

#=========================================================
# 2. Quantidade de matrículas distintas
#=========================================================

cat("\n=========================================================\n")
cat("2. MATRÍCULAS DISTINTAS\n")
cat("=========================================================\n")

cat(
  "\nMatrículas distintas na amostra final:",
  n_distinct(amostra_final$Matricula),
  "\n"
)

cat(
  "Matrículas distintas no histórico:",
  n_distinct(tabela_historico$MATRICULA),
  "\n"
)

#=========================================================
# 3. Verificar algumas matrículas da amostra
#=========================================================

cat("\n=========================================================\n")
cat("3. PRIMEIRAS MATRÍCULAS DA AMOSTRA\n")
cat("=========================================================\n")

matriculas_amostra <- amostra_final %>%
  distinct(Matricula) %>%
  slice_head(n = 10)

print(matriculas_amostra)

#=========================================================
# 4. Verificar se essas matrículas existem no histórico
#=========================================================

cat("\n=========================================================\n")
cat("4. VERIFICAÇÃO DAS MATRÍCULAS NO HISTÓRICO\n")
cat("=========================================================\n")

verificacao_matriculas <- matriculas_amostra %>%
  mutate(
    encontrada_historico =
      Matricula %in% tabela_historico$MATRICULA
  )

print(verificacao_matriculas)

#=========================================================
# 5. Cobertura completa da amostra
#=========================================================

cat("\n=========================================================\n")
cat("5. COBERTURA DA AMOSTRA NO HISTÓRICO\n")
cat("=========================================================\n")

matriculas_da_amostra <- amostra_final %>%
  distinct(Matricula)

matriculas_encontradas <- matriculas_da_amostra %>%
  filter(
    Matricula %in% tabela_historico$MATRICULA
  )

matriculas_nao_encontradas <- matriculas_da_amostra %>%
  filter(
    !(Matricula %in% tabela_historico$MATRICULA)
  )

cat(
  "\nTotal de matrículas na amostra:",
  nrow(matriculas_da_amostra),
  "\n"
)

cat(
  "Encontradas no histórico:",
  nrow(matriculas_encontradas),
  "\n"
)

cat(
  "Não encontradas no histórico:",
  nrow(matriculas_nao_encontradas),
  "\n"
)

cat(
  "Cobertura:",
  round(
    100 *
      nrow(matriculas_encontradas) /
      nrow(matriculas_da_amostra),
    2
  ),
  "%\n"
)

#=========================================================
# 6. Matrículas da amostra que NÃO aparecem no histórico
#=========================================================

cat("\n=========================================================\n")
cat("6. MATRÍCULAS NÃO ENCONTRADAS\n")
cat("=========================================================\n")

if (nrow(matriculas_nao_encontradas) > 0) {
  
  print(matriculas_nao_encontradas)
  
} else {
  
  cat("\nTodas as matrículas da amostra foram encontradas no histórico.\n")
  
}

#=========================================================
# 7. Verificar duplicidade de matrícula na amostra
#=========================================================

cat("\n=========================================================\n")
cat("7. DUPLICIDADE DE MATRÍCULA NA AMOSTRA\n")
cat("=========================================================\n")

duplicidade_amostra <- amostra_final %>%
  count(
    Matricula,
    name = "quantidade"
  ) %>%
  filter(
    quantidade > 1
  )

cat(
  "\nNúmero de matrículas com mais de um registro na amostra:",
  nrow(duplicidade_amostra),
  "\n"
)

if (nrow(duplicidade_amostra) > 0) {
  print(duplicidade_amostra)
}

#=========================================================
# 8. Quantidade de registros no histórico para
#    algumas matrículas da amostra
#=========================================================

cat("\n=========================================================\n")
cat("8. REGISTROS NO HISTÓRICO PARA ALGUMAS MATRÍCULAS\n")
cat("=========================================================\n")

registros_exemplo <- tabela_historico %>%
  filter(
    MATRICULA %in%
      matriculas_amostra$Matricula
  ) %>%
  count(
    MATRICULA,
    name = "registros_historico"
  ) %>%
  arrange(MATRICULA)

print(registros_exemplo)

#=========================================================

cat("\n=========================================================\n")
cat("ETAPA 3 CONCLUÍDA\n")
cat("=========================================================\n")