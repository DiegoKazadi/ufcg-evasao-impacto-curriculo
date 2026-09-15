# =========================================================
# VALIDAR COMPATIBILIDADE DA MATRÍCULA
# AMOSTRA FINAL × MATRICULAS
# Seção 5.7.5 - Análise Comparativa das Disciplinas
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
# 2. CARREGAR AS DUAS TABELAS
# =========================================================

amostra <- read_csv2(
  file.path(
    pasta_processados,
    "amostra_final_dissertacao.csv"
  ),
  show_col_types = FALSE
)

matriculas <- read_csv2(
  file.path(
    pasta_dados,
    "matriculas.csv"
  ),
  show_col_types = FALSE
)

# =========================================================
# 3. ESTRUTURA ORIGINAL
# =========================================================

cat("\n=========================================================\n")
cat("ESTRUTURA ORIGINAL\n")
cat("=========================================================\n")

cat("\nAMOSTRA:\n")
cat("Linhas:", nrow(amostra), "\n")
cat("Colunas:", ncol(amostra), "\n")
cat("Classe Matricula:", class(amostra$Matricula), "\n")

cat("\nMATRICULAS:\n")
cat("Linhas:", nrow(matriculas), "\n")
cat("Colunas:", ncol(matriculas), "\n")
cat("Classe MATRICULA:", class(matriculas$MATRICULA), "\n")


# =========================================================
# 4. CRIAR CHAVES PADRONIZADAS
# =========================================================

amostra <- amostra %>%
  mutate(
    MATRICULA_JOIN = str_trim(
      as.character(Matricula)
    )
  )

matriculas <- matriculas %>%
  mutate(
    MATRICULA_JOIN = str_trim(
      as.character(MATRICULA)
    )
  )


# =========================================================
# 5. VERIFICAR VALORES AUSENTES
# =========================================================

cat("\n=========================================================\n")
cat("VALORES AUSENTES NA CHAVE\n")
cat("=========================================================\n")

cat(
  "\nNA na amostra:",
  sum(is.na(amostra$MATRICULA_JOIN)),
  "\n"
)

cat(
  "NA na tabela matriculas:",
  sum(is.na(matriculas$MATRICULA_JOIN)),
  "\n"
)

cat(
  "Vazios na amostra:",
  sum(amostra$MATRICULA_JOIN == "", na.rm = TRUE),
  "\n"
)

cat(
  "Vazios na tabela matriculas:",
  sum(matriculas$MATRICULA_JOIN == "", na.rm = TRUE),
  "\n"
)


# =========================================================
# 6. MATRÍCULAS ÚNICAS NA AMOSTRA
# =========================================================

cat("\n=========================================================\n")
cat("MATRÍCULAS DA AMOSTRA\n")
cat("=========================================================\n")

cat(
  "\nRegistros da amostra:",
  nrow(amostra),
  "\n"
)

cat(
  "Matrículas únicas:",
  n_distinct(amostra$MATRICULA_JOIN),
  "\n"
)

cat(
  "Matrículas duplicadas:",
  nrow(amostra) -
    n_distinct(amostra$MATRICULA_JOIN),
  "\n"
)


# =========================================================
# 7. MATRÍCULAS ÚNICAS NA TABELA DE HISTÓRICO
# =========================================================

cat("\n=========================================================\n")
cat("MATRÍCULAS DA TABELA MATRICULAS\n")
cat("=========================================================\n")

cat(
  "\nRegistros:",
  nrow(matriculas),
  "\n"
)

cat(
  "Matrículas únicas:",
  n_distinct(
    matriculas$MATRICULA_JOIN
  ),
  "\n"
)


# =========================================================
# 8. MATCH EXATO
# =========================================================

chaves_amostra <- amostra %>%
  distinct(
    MATRICULA_JOIN
  )

chaves_matriculas <- matriculas %>%
  distinct(
    MATRICULA_JOIN
  )

match <- chaves_amostra %>%
  mutate(
    encontrada =
      MATRICULA_JOIN %in%
      chaves_matriculas$MATRICULA_JOIN
  )

cat("\n=========================================================\n")
cat("COMPATIBILIDADE DA MATRÍCULA\n")
cat("=========================================================\n")

cat(
  "\nMatrículas únicas da amostra:",
  nrow(match),
  "\n"
)

cat(
  "Encontradas:",
  sum(match$encontrada),
  "\n"
)

cat(
  "Não encontradas:",
  sum(!match$encontrada),
  "\n"
)

cat(
  "Cobertura:",
  round(
    mean(match$encontrada) * 100,
    4
  ),
  "%\n"
)


# =========================================================
# 9. IDENTIFICAR AS NÃO ENCONTRADAS
# =========================================================

nao_encontradas <- match %>%
  filter(
    !encontrada
  )

cat("\n=========================================================\n")
cat("MATRÍCULAS NÃO ENCONTRADAS\n")
cat("=========================================================\n")

print(
  nao_encontradas
)


# =========================================================
# 10. TESTAR SE O JOIN MULTIPLICA A AMOSTRA
# =========================================================

cat("\n=========================================================\n")
cat("TESTE DE MULTIPLICAÇÃO DO JOIN\n")
cat("=========================================================\n")

# Somente para testar a chave:
# não estamos trazendo todas as colunas ainda.

teste_join <- amostra %>%
  left_join(
    matriculas %>%
      select(
        MATRICULA_JOIN
      ),
    by = "MATRICULA_JOIN"
  )

cat(
  "\nLinhas originais da amostra:",
  nrow(amostra),
  "\n"
)

cat(
  "Linhas após LEFT JOIN:",
  nrow(teste_join),
  "\n"
)

cat(
  "Diferença:",
  nrow(teste_join) -
    nrow(amostra),
  "\n"
)


# =========================================================
# 11. QUANTAS LINHAS DE HISTÓRICO POR ALUNO?
# =========================================================

frequencia <- matriculas %>%
  count(
    MATRICULA_JOIN,
    name = "N_REGISTROS_HISTORICO"
  )

cat("\n=========================================================\n")
cat("REGISTROS DE HISTÓRICO POR MATRÍCULA\n")
cat("=========================================================\n")

print(
  frequencia %>%
    summarise(
      minimo = min(
        N_REGISTROS_HISTORICO
      ),
      media = mean(
        N_REGISTROS_HISTORICO
      ),
      mediana = median(
        N_REGISTROS_HISTORICO
      ),
      maximo = max(
        N_REGISTROS_HISTORICO
      )
    )
)


# =========================================================
# 12. COBERTURA POR ALUNO DA AMOSTRA
# =========================================================

cobertura_alunos <- amostra %>%
  distinct(
    MATRICULA_JOIN
  ) %>%
  left_join(
    frequencia,
    by = "MATRICULA_JOIN"
  ) %>%
  mutate(
    possui_historico =
      !is.na(
        N_REGISTROS_HISTORICO
      )
  )

cat("\n=========================================================\n")
cat("COBERTURA DOS ALUNOS\n")
cat("=========================================================\n")

print(
  cobertura_alunos %>%
    count(
      possui_historico
    )
)


# =========================================================
# 13. TESTAR ESPECIFICAMENTE CÁLCULO I
# =========================================================

cat("\n=========================================================\n")
cat("CÁLCULO I / FMCC I\n")
cat("=========================================================\n")

calculo <- matriculas %>%
  filter(
    str_detect(
      str_to_upper(
        NOME
      ),
      "CALCULO I|CÁLCULO I|FMCC I|FMCCI"
    )
  )

cat(
  "\nRegistros encontrados para Cálculo I / FMCC I:",
  nrow(calculo),
  "\n"
)

cat(
  "Matrículas únicas:",
  n_distinct(
    calculo$MATRICULA_JOIN
  ),
  "\n"
)

cat("\nNomes encontrados:\n")

print(
  calculo %>%
    count(
      NOME,
      sort = TRUE
    )
)


# =========================================================
# 14. CÁLCULO I / FMCC I DENTRO DA AMOSTRA
# =========================================================

calculo_amostra <- calculo %>%
  semi_join(
    amostra %>%
      distinct(
        MATRICULA_JOIN
      ),
    by = "MATRICULA_JOIN"
  )

cat("\n=========================================================\n")
cat("CÁLCULO I / FMCC I NA AMOSTRA\n")
cat("=========================================================\n")

cat(
  "\nRegistros:",
  nrow(calculo_amostra),
  "\n"
)

cat(
  "Alunos da amostra com registro:",
  n_distinct(
    calculo_amostra$MATRICULA_JOIN
  ),
  "\n"
)

cat(
  "Alunos da amostra SEM registro:",
  nrow(
    chaves_amostra
  ) -
    n_distinct(
      calculo_amostra$MATRICULA_JOIN
    ),
  "\n"
)

cat("\nDistribuição por disciplina:\n")

print(
  calculo_amostra %>%
    count(
      NOME,
      sort = TRUE
    )
)


# =========================================================
# 15. VERIFICAR STATUS DA DISCIPLINA
# =========================================================

cat("\n=========================================================\n")
cat("STATUS DE CÁLCULO I / FMCC I\n")
cat("=========================================================\n")

print(
  calculo_amostra %>%
    count(
      NOME,
      ESTATUS,
      sort = TRUE
    )
)


# =========================================================
# 16. VERIFICAR SE HÁ REPETIÇÃO DA MESMA DISCIPLINA
# =========================================================

cat("\n=========================================================\n")
cat("REPETIÇÃO DE CÁLCULO I / FMCC I\n")
cat("=========================================================\n")

print(
  calculo_amostra %>%
    count(
      MATRICULA_JOIN,
      NOME,
      name = "N_VEZES"
    ) %>%
    filter(
      N_VEZES > 1
    ) %>%
    arrange(
      desc(N_VEZES)
    )
)


# =========================================================
# 17. RESULTADO FINAL
# =========================================================

cat("\n=========================================================\n")
cat("RESULTADO FINAL DA VALIDAÇÃO\n")
cat("=========================================================\n")

cat(
  "\nAmostra:",
  nrow(amostra),
  "registros\n"
)

cat(
  "Matrículas únicas na amostra:",
  nrow(chaves_amostra),
  "\n"
)

cat(
  "Matrículas encontradas:",
  sum(match$encontrada),
  "\n"
)

cat(
  "Matrículas não encontradas:",
  sum(!match$encontrada),
  "\n"
)

cat(
  "Cobertura:",
  round(
    mean(match$encontrada) * 100,
    4
  ),
  "%\n"
)

cat(
  "\nLinhas após LEFT JOIN:",
  nrow(teste_join),
  "\n"
)

cat(
  "Registros de Cálculo I / FMCC I:",
  nrow(calculo_amostra),
  "\n"
)

cat(
  "Alunos com Cálculo I / FMCC I:",
  n_distinct(
    calculo_amostra$MATRICULA_JOIN
  ),
  "\n"
)


# =========================================================
# VERIFICAÇÃO DA TABELA_HISTORICO
# CÁLCULO I / FMCC I
# =========================================================

cat("\n=========================================================\n")
cat("ANÁLISE - TABELA_HISTORICO\n")
cat("=========================================================\n")

# =========================================================
# 1. ESTRUTURA DA MATRÍCULA
# =========================================================

cat("\nClasse da matrícula na amostra:\n")
print(class(amostra$Matricula))

cat("\nClasse da matrícula no histórico:\n")
print(class(tabela_historico$MATRICULA))

cat("\nExemplos de matrículas da amostra:\n")
print(head(amostra$Matricula, 10))

cat("\nExemplos de matrículas do histórico:\n")
print(head(tabela_historico$MATRICULA, 10))


# =========================================================
# 2. PADRONIZAR MATRÍCULA
# =========================================================

amostra <- amostra %>%
  mutate(
    MATRICULA_JOIN = str_trim(
      as.character(Matricula)
    )
  )

tabela_historico <- tabela_historico %>%
  mutate(
    MATRICULA_JOIN = str_trim(
      as.character(MATRICULA)
    )
  )


# =========================================================
# 3. QUANTIDADE DE DÍGITOS
# =========================================================

cat("\n=========================================================\n")
cat("QUANTIDADE DE DÍGITOS DAS MATRÍCULAS\n")
cat("=========================================================\n")

cat("\nAmostra:\n")

print(
  amostra %>%
    mutate(
      DIGITOS = nchar(MATRICULA_JOIN)
    ) %>%
    count(
      DIGITOS
    )
)

cat("\nTabela histórico:\n")

print(
  tabela_historico %>%
    mutate(
      DIGITOS = nchar(MATRICULA_JOIN)
    ) %>%
    count(
      DIGITOS
    )
)


# =========================================================
# 4. TESTAR COMPATIBILIDADE DAS MATRÍCULAS
# =========================================================

matriculas_amostra <- amostra %>%
  distinct(
    MATRICULA_JOIN
  )

matriculas_historico <- tabela_historico %>%
  distinct(
    MATRICULA_JOIN
  )

match_historico <- matriculas_amostra %>%
  mutate(
    encontrada =
      MATRICULA_JOIN %in%
      matriculas_historico$MATRICULA_JOIN
  )

cat("\n=========================================================\n")
cat("COMPATIBILIDADE DA MATRÍCULA\n")
cat("=========================================================\n")

cat(
  "\nMatrículas únicas na amostra:",
  nrow(matriculas_amostra),
  "\n"
)

cat(
  "Matrículas únicas no histórico:",
  nrow(matriculas_historico),
  "\n"
)

cat(
  "Encontradas:",
  sum(match_historico$encontrada),
  "\n"
)

cat(
  "Não encontradas:",
  sum(!match_historico$encontrada),
  "\n"
)

cat(
  "Cobertura:",
  round(
    mean(match_historico$encontrada) * 100,
    4
  ),
  "%\n"
)


# =========================================================
# 5. PROCURAR CÁLCULO I E FMCC I
# =========================================================

cat("\n=========================================================\n")
cat("DISCIPLINAS DE INTERESSE\n")
cat("=========================================================\n")

disciplinas_interesse <- tabela_historico %>%
  filter(
    str_detect(
      str_to_upper(
        str_trim(DISCIPLINA)
      ),
      "CALCULO|CÁLCULO|FMCC"
    )
  )

cat(
  "\nTotal de registros encontrados:",
  nrow(disciplinas_interesse),
  "\n"
)

cat(
  "Matrículas únicas:",
  n_distinct(
    disciplinas_interesse$MATRICULA_JOIN
  ),
  "\n"
)

cat("\nNomes encontrados:\n")

print(
  disciplinas_interesse %>%
    count(
      DISCIPLINA,
      sort = TRUE
    )
)


# =========================================================
# 6. CÁLCULO I ESPECIFICAMENTE
# =========================================================

calculo_I_historico <- tabela_historico %>%
  filter(
    str_detect(
      str_to_upper(
        str_trim(DISCIPLINA)
      ),
      "CALCULO.*I|CÁLCULO.*I"
    )
  )

cat("\n=========================================================\n")
cat("CÁLCULO I\n")
cat("=========================================================\n")

cat(
  "\nRegistros:",
  nrow(calculo_I_historico),
  "\n"
)

cat(
  "Matrículas únicas:",
  n_distinct(
    calculo_I_historico$MATRICULA_JOIN
  ),
  "\n"
)

print(
  calculo_I_historico %>%
    count(
      DISCIPLINA,
      sort = TRUE
    )
)


# =========================================================
# 7. FMCC I
# =========================================================

fmcc_I_historico <- tabela_historico %>%
  filter(
    str_detect(
      str_to_upper(
        str_trim(DISCIPLINA)
      ),
      "FMCC"
    )
  )

cat("\n=========================================================\n")
cat("FMCC I / FMCC\n")
cat("=========================================================\n")

cat(
  "\nRegistros:",
  nrow(fmcc_I_historico),
  "\n"
)

cat(
  "Matrículas únicas:",
  n_distinct(
    fmcc_I_historico$MATRICULA_JOIN
  ),
  "\n"
)

print(
  fmcc_I_historico %>%
    count(
      DISCIPLINA,
      sort = TRUE
    )
)


# =========================================================
# 8. VERIFICAR DISCIPLINAS DENTRO DA AMOSTRA
# =========================================================

calculo_amostra_historico <- calculo_I_historico %>%
  semi_join(
    matriculas_amostra,
    by = "MATRICULA_JOIN"
  )

fmcc_amostra_historico <- fmcc_I_historico %>%
  semi_join(
    matriculas_amostra,
    by = "MATRICULA_JOIN"
  )

cat("\n=========================================================\n")
cat("DISCIPLINAS DENTRO DA AMOSTRA\n")
cat("=========================================================\n")

cat(
  "\nCálculo I - registros:",
  nrow(calculo_amostra_historico),
  "\n"
)

cat(
  "Cálculo I - alunos:",
  n_distinct(
    calculo_amostra_historico$MATRICULA_JOIN
  ),
  "\n"
)

cat(
  "\nFMCC I - registros:",
  nrow(fmcc_amostra_historico),
  "\n"
)

cat(
  "FMCC I - alunos:",
  n_distinct(
    fmcc_amostra_historico$MATRICULA_JOIN
  ),
  "\n"
)


# =========================================================
# 9. MOSTRAR ALGUNS REGISTROS
# =========================================================

cat("\n=========================================================\n")
cat("EXEMPLOS DE CÁLCULO I\n")
cat("=========================================================\n")

print(
  head(
    calculo_amostra_historico,
    20
  )
)

cat("\n=========================================================\n")
cat("EXEMPLOS DE FMCC I\n")
cat("=========================================================\n")

print(
  head(
    fmcc_amostra_historico,
    20
  )
)


# =========================================================
# 10. RESULTADO FINAL
# =========================================================

cat("\n=========================================================\n")
cat("RESULTADO FINAL - TABELA_HISTORICO\n")
cat("=========================================================\n")

cat(
  "\nAmostra:",
  nrow(amostra),
  "registros\n"
)

cat(
  "Matrículas únicas:",
  nrow(matriculas_amostra),
  "\n"
)

cat(
  "Matrículas encontradas no histórico:",
  sum(match_historico$encontrada),
  "\n"
)

cat(
  "Cobertura da matrícula:",
  round(
    mean(match_historico$encontrada) * 100,
    2
  ),
  "%\n"
)

cat(
  "\nCálculo I - registros na amostra:",
  nrow(calculo_amostra_historico),
  "\n"
)

cat(
  "Cálculo I - alunos na amostra:",
  n_distinct(
    calculo_amostra_historico$MATRICULA_JOIN
  ),
  "\n"
)

cat(
  "\nFMCC I - registros na amostra:",
  nrow(fmcc_amostra_historico),
  "\n"
)

cat(
  "FMCC I - alunos na amostra:",
  n_distinct(
    fmcc_amostra_historico$MATRICULA_JOIN
  ),
  "\n"
)

cat("\n=========================================================\n")
cat("FIM\n")
cat("=========================================================\n")
