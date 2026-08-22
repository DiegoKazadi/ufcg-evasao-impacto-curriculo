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
# VERIFICAR CÁLCULO I E FMCC I NA TABELA MATRICULAS
# =========================================================

cat("\n=========================================================\n")
cat("VERIFICAÇÃO: CÁLCULO I / FMCC I\n")
cat("=========================================================\n")

# ---------------------------------------------------------
# 1. Verificar os nomes das disciplinas
# ---------------------------------------------------------

disciplinas_alvo <- matriculas %>%
  filter(
    str_detect(
      str_to_upper(
        str_trim(NOME)
      ),
      "CALCULO|CÁLCULO|FMCC"
    )
  )

cat(
  "\nTotal de registros encontrados:",
  nrow(disciplinas_alvo),
  "\n"
)

cat(
  "Total de matrículas únicas:",
  n_distinct(
    disciplinas_alvo$MATRICULA
  ),
  "\n"
)

# ---------------------------------------------------------
# 2. Mostrar os nomes exatos encontrados
# ---------------------------------------------------------

cat("\n=========================================================\n")
cat("NOMES EXATOS ENCONTRADOS\n")
cat("=========================================================\n")

print(
  disciplinas_alvo %>%
    count(
      NOME,
      sort = TRUE
    )
)

# ---------------------------------------------------------
# 3. Procurar especificamente CÁLCULO I
# ---------------------------------------------------------

calculo_I <- matriculas %>%
  filter(
    str_detect(
      str_to_upper(
        str_trim(NOME)
      ),
      "^C[ÁA]LCULO I$|^CALCULO I$"
    )
  )

cat("\n=========================================================\n")
cat("CÁLCULO I\n")
cat("=========================================================\n")

cat(
  "\nRegistros:",
  nrow(calculo_I),
  "\n"
)

cat(
  "Matrículas únicas:",
  n_distinct(
    calculo_I$MATRICULA
  ),
  "\n"
)

print(calculo_I)

# ---------------------------------------------------------
# 4. Procurar especificamente FMCC I
# ---------------------------------------------------------

fmcc_I <- matriculas %>%
  filter(
    str_detect(
      str_to_upper(
        str_trim(NOME)
      ),
      "FMCC"
    )
  )

cat("\n=========================================================\n")
cat("FMCC I / FMCC\n")
cat("=========================================================\n")

cat(
  "\nRegistros:",
  nrow(fmcc_I),
  "\n"
)

cat(
  "Matrículas únicas:",
  n_distinct(
    fmcc_I$MATRICULA
  ),
  "\n"
)

print(fmcc_I)

# ---------------------------------------------------------
# 5. Criar tabela consolidada
# ---------------------------------------------------------

disciplinas_5_7_5 <- bind_rows(
  calculo_I %>%
    mutate(
      DISCIPLINA_ANALISE = "CALCULO I"
    ),
  
  fmcc_I %>%
    mutate(
      DISCIPLINA_ANALISE = "FMCC I"
    )
) %>%
  distinct()

# ---------------------------------------------------------
# 6. Resumo
# ---------------------------------------------------------

cat("\n=========================================================\n")
cat("RESUMO FINAL\n")
cat("=========================================================\n")

print(
  disciplinas_5_7_5 %>%
    count(
      DISCIPLINA_ANALISE,
      name = "REGISTROS"
    )
)

cat(
  "\nMatrículas únicas envolvidas:",
  n_distinct(
    disciplinas_5_7_5$MATRICULA
  ),
  "\n"
)

# ---------------------------------------------------------
# 7. Salvar resultado
# ---------------------------------------------------------

arquivo_saida <- file.path(
  pasta_dados,
  "verificacao_calculo_fmcc_matriculas.csv"
)

write_csv2(
  disciplinas_5_7_5,
  arquivo_saida
)

cat("\n=========================================================\n")
cat("ARQUIVO GERADO\n")
cat("=========================================================\n")

cat(
  "\nArquivo:",
  arquivo_saida,
  "\n"
)

