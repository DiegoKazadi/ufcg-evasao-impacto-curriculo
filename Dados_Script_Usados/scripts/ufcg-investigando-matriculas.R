# =========================================================
# INVESTIGAÇÃO PROFUNDA DA TABELA MATRICULAS
# =========================================================

rm(list = ls())

library(readr)
library(dplyr)

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
# 2. CARREGAR DADOS
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

tabela_historico <- read_csv2(
  file.path(
    pasta_dados,
    "tabela_historico.csv"
  ),
  show_col_types = FALSE
)

# =========================================================
# 3. PADRONIZAR MATRÍCULA
# =========================================================

amostra <- amostra %>%
  mutate(
    MATRICULA = as.character(Matricula)
  )

matriculas <- matriculas %>%
  mutate(
    MATRICULA = as.character(MATRICULA)
  )

tabela_historico <- tabela_historico %>%
  mutate(
    MATRICULA = as.character(MATRICULA)
  )

# =========================================================
# 4. MATRÍCULAS DA AMOSTRA
# =========================================================

matriculas_amostra <- amostra %>%
  distinct(
    MATRICULA
  )

# =========================================================
# 5. MATCH
# =========================================================

match_matriculas <- matriculas_amostra %>%
  mutate(
    encontrada =
      MATRICULA %in%
      matriculas$MATRICULA
  )

cat("\n=========================================================\n")
cat("COBERTURA\n")
cat("=========================================================\n")

cat(
  "\nTotal da amostra:",
  nrow(matriculas_amostra),
  "\n"
)

cat(
  "Encontradas:",
  sum(match_matriculas$encontrada),
  "\n"
)

cat(
  "Não encontradas:",
  sum(!match_matriculas$encontrada),
  "\n"
)

# =========================================================
# 6. AS 5 MATRÍCULAS NÃO ENCONTRADAS
# =========================================================

cat("\n=========================================================\n")
cat("MATRÍCULAS DA AMOSTRA NÃO ENCONTRADAS\n")
cat("=========================================================\n")

nao_encontradas <- match_matriculas %>%
  filter(
    !encontrada
  )

print(
  nao_encontradas
)

# =========================================================
# 7. DISTRIBUIÇÃO DE REGISTROS POR ALUNO
# =========================================================

cat("\n=========================================================\n")
cat("REGISTROS POR MATRÍCULA\n")
cat("=========================================================\n")

resumo_registros <- matriculas %>%
  semi_join(
    matriculas_amostra,
    by = "MATRICULA"
  ) %>%
  count(
    MATRICULA,
    name = "N_REGISTROS"
  )

print(
  resumo_registros %>%
    summarise(
      minimo = min(N_REGISTROS),
      media = mean(N_REGISTROS),
      mediana = median(N_REGISTROS),
      q1 = quantile(N_REGISTROS, 0.25),
      q3 = quantile(N_REGISTROS, 0.75),
      maximo = max(N_REGISTROS)
    )
)

# =========================================================
# 8. VALORES DE TERMO
# =========================================================

cat("\n=========================================================\n")
cat("VALORES DE TERMO\n")
cat("=========================================================\n")

print(
  matriculas %>%
    semi_join(
      matriculas_amostra,
      by = "MATRICULA"
    ) %>%
    count(
      TERMO,
      sort = TRUE
    ) %>%
    head(50)
)

# =========================================================
# 9. VALORES ÚNICOS DE TERMO
# =========================================================

cat("\n=========================================================\n")
cat("QUANTIDADE DE TERMOS DISTINTOS\n")
cat("=========================================================\n")

cat(
  "Termos distintos:",
  n_distinct(
    matriculas$TERMO
  ),
  "\n"
)

print(
  sort(
    unique(
      matriculas$TERMO
    )
  )
)

# =========================================================
# 10. QUANTIDADE DE DISCIPLINAS
# =========================================================

cat("\n=========================================================\n")
cat("DISCIPLINAS\n")
cat("=========================================================\n")

cat(
  "Códigos de disciplinas distintos:",
  n_distinct(
    matriculas$CODIGO_DISCIPLINA
  ),
  "\n"
)

cat(
  "Nomes de disciplinas distintos:",
  n_distinct(
    matriculas$NOME
  ),
  "\n"
)

# =========================================================
# 11. SITUAÇÕES
# =========================================================

cat("\n=========================================================\n")
cat("ESTATUS\n")
cat("=========================================================\n")

print(
  matriculas %>%
    semi_join(
      matriculas_amostra,
      by = "MATRICULA"
    ) %>%
    count(
      ESTATUS,
      sort = TRUE
    )
)

# =========================================================
# 12. TIPOS
# =========================================================

cat("\n=========================================================\n")
cat("TIPO\n")
cat("=========================================================\n")

print(
  matriculas %>%
    semi_join(
      matriculas_amostra,
      by = "MATRICULA"
    ) %>%
    count(
      TIPO,
      sort = TRUE
    )
)

# =========================================================
# 13. ESCOLHER 5 ALUNOS DA AMOSTRA
# =========================================================

alunos_teste <- matriculas_amostra %>%
  slice_head(
    n = 5
  )

# =========================================================
# 14. MOSTRAR HISTÓRICO COMPLETO DOS 5
# =========================================================

cat("\n=========================================================\n")
cat("HISTÓRICO DE 5 ALUNOS\n")
cat("=========================================================\n")

for (mat in alunos_teste$MATRICULA) {
  
  cat("\n\n---------------------------------------------\n")
  cat("MATRÍCULA:", mat, "\n")
  cat("---------------------------------------------\n")
  
  print(
    matriculas %>%
      filter(
        MATRICULA == mat
      ) %>%
      arrange(
        TERMO
      )
  )
}

# =========================================================
# 15. COMPARAR COM TABELA_HISTORICO
# =========================================================

cat("\n=========================================================\n")
cat("COMPARAÇÃO MATRICULAS × TABELA_HISTORICO\n")
cat("=========================================================\n")

cat(
  "\nMatrículas únicas em matriculas:",
  n_distinct(
    matriculas$MATRICULA
  ),
  "\n"
)

cat(
  "Matrículas únicas em tabela_historico:",
  n_distinct(
    tabela_historico$MATRICULA
  ),
  "\n"
)

# =========================================================
# 16. VERIFICAR INTERSEÇÃO
# =========================================================

intersecao <- intersect(
  unique(
    matriculas$MATRICULA
  ),
  unique(
    tabela_historico$MATRICULA
  )
)

cat(
  "\nMatrículas presentes nas duas tabelas:",
  length(intersecao),
  "\n"
)

# =========================================================
# 17. EXEMPLO DE INTERSEÇÃO
# =========================================================

cat(
  "\nPrimeiras matrículas presentes nas duas:\n"
)

print(
  head(
    intersecao,
    10
  )
)

# =========================================================
# 18. CONCLUSÃO
# =========================================================

cat("\n=========================================================\n")
cat("INVESTIGAÇÃO CONCLUÍDA\n")
cat("=========================================================\n")