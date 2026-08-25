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
# 4. CARREGAR CURRÍCULO 1999
# =========================================================

disciplinas_1999 <- read_csv2(
  file.path(
    pasta_processados,
    "disciplinas_curriculo_1999_processado.csv"
  ),
  show_col_types = FALSE
)

# =========================================================
# 5. CARREGAR CURRÍCULO 2017
# =========================================================

disciplinas_2017 <- read_csv2(
  file.path(
    pasta_processados,
    "disciplinas_curriculo_2017_processado.csv"
  ),
  show_col_types = FALSE
)

# =========================================================
# 6. CONFERÊNCIA INICIAL DAS TABELAS
# =========================================================

cat("\n=========================================================\n")
cat("DIMENSÕES DAS TABELAS\n")
cat("=========================================================\n")

cat("\nAmostra final:\n")
cat("Linhas:", nrow(amostra), "\n")
cat("Colunas:", ncol(amostra), "\n")

cat("\nEnrollments:\n")
cat("Linhas:", nrow(enrollments), "\n")
cat("Colunas:", ncol(enrollments), "\n")

cat("\nDisciplinas currículo 1999:\n")
cat("Linhas:", nrow(disciplinas_1999), "\n")
cat("Colunas:", ncol(disciplinas_1999), "\n")

cat("\nDisciplinas currículo 2017:\n")
cat("Linhas:", nrow(disciplinas_2017), "\n")
cat("Colunas:", ncol(disciplinas_2017), "\n")

# =========================================================
# 7. CONFERIR NOMES DAS COLUNAS
# =========================================================

cat("\n=========================================================\n")
cat("COLUNAS DA AMOSTRA\n")
cat("=========================================================\n")
print(names(amostra))

cat("\n=========================================================\n")
cat("COLUNAS DA ENROLLMENTS\n")
cat("=========================================================\n")
print(names(enrollments))

cat("\n=========================================================\n")
cat("COLUNAS DO CURRÍCULO 1999\n")
cat("=========================================================\n")
print(names(disciplinas_1999))

cat("\n=========================================================\n")
cat("COLUNAS DO CURRÍCULO 2017\n")
cat("=========================================================\n")
print(names(disciplinas_2017))


# =========================================================
# 8. PADRONIZAR A MATRÍCULA
# =========================================================
# A matrícula será a chave utilizada para relacionar
# amostra_final_dissertacao com enrollments.

amostra <- amostra %>%
  mutate(
    Matricula = str_trim(as.character(Matricula)),
    Curriculo = str_trim(as.character(Curriculo))
  )

enrollments <- enrollments %>%
  mutate(
    registration = str_trim(as.character(registration)),
    subjectCode = str_trim(as.character(subjectCode))
  )


# =========================================================
# 9. IDENTIFICAR OS CINCO ALUNOS SEM HISTÓRICO
# =========================================================

matriculas_amostra <- amostra %>%
  distinct(Matricula)

matriculas_enrollments <- enrollments %>%
  distinct(registration)

alunos_sem_historico <- matriculas_amostra %>%
  anti_join(
    matriculas_enrollments,
    by = c("Matricula" = "registration")
  )

cat("\n=========================================================\n")
cat("ALUNOS DA AMOSTRA SEM REGISTRO EM ENROLLMENTS\n")
cat("=========================================================\n")

print(alunos_sem_historico)

cat("\nQuantidade:", nrow(alunos_sem_historico), "\n")


# =========================================================
# 10. SEPARAR OS CINCO ALUNOS PARA ANÁLISE
# =========================================================

alunos_sem_historico_detalhes <- amostra %>%
  semi_join(
    alunos_sem_historico,
    by = "Matricula"
  ) %>%
  arrange(Curriculo, Matricula)

cat("\n=========================================================\n")
cat("DETALHES DOS ALUNOS SEM HISTÓRICO\n")
cat("=========================================================\n")

print(alunos_sem_historico_detalhes)


# =========================================================
# 11. VERIFICAR A COBERTURA DA ENROLLMENTS
# =========================================================

amostra_com_historico <- amostra %>%
  mutate(
    possui_historico = Matricula %in% enrollments$registration
  )

cat("\n=========================================================\n")
cat("COBERTURA DO HISTÓRICO\n")
cat("=========================================================\n")

print(
  amostra_com_historico %>%
    count(possui_historico)
)


# =========================================================
# 12. SEPARAR A AMOSTRA POR CURRÍCULO
# =========================================================

amostra_1999 <- amostra %>%
  filter(Curriculo == "1999")

amostra_2017 <- amostra %>%
  filter(Curriculo == "2017")

cat("\n=========================================================\n")
cat("AMOSTRA POR CURRÍCULO\n")
cat("=========================================================\n")

cat("\nCurrículo 1999:", nrow(amostra_1999), "\n")
cat("Currículo 2017:", nrow(amostra_2017), "\n")


# =========================================================
# 13. LOCALIZAR AS DISCIPLINAS DE MATEMÁTICA
# =========================================================
#
# Primeiro vamos procurar pelos nomes das disciplinas.
# Não vamos assumir ainda os códigos.
#
# Isso é importante porque o código da disciplina é justamente
# o que posteriormente será relacionado com subjectCode.

cat("\n=========================================================\n")
cat("DISCIPLINAS RELACIONADAS A CÁLCULO - CURRÍCULO 1999\n")
cat("=========================================================\n")

disciplinas_1999 %>%
  filter(
    str_detect(
      str_to_upper(NOME_DISCIPLINA),
      "CALC"
    )
  ) %>%
  select(
    everything()
  ) %>%
  print(n = Inf)


cat("\n=========================================================\n")
cat("DISCIPLINAS RELACIONADAS A CÁLCULO - CURRÍCULO 2017\n")
cat("=========================================================\n")

disciplinas_2017 %>%
  filter(
    str_detect(
      str_to_upper(NOME_DISCIPLINA),
      "CALC"
    )
  ) %>%
  select(
    everything()
  ) %>%
  print(n = Inf)


# =========================================================
# 14. LOCALIZAR FMMC
# =========================================================

cat("\n=========================================================\n")
cat("DISCIPLINAS RELACIONADAS A FMMC - CURRÍCULO 1999\n")
cat("=========================================================\n")

disciplinas_1999 %>%
  filter(
    str_detect(
      str_to_upper(NOME_DISCIPLINA),
      "FMMC"
    )
  ) %>%
  print(n = Inf)


cat("\n=========================================================\n")
cat("DISCIPLINAS RELACIONADAS A FMMC - CURRÍCULO 2017\n")
cat("=========================================================\n")

disciplinas_2017 %>%
  filter(
    str_detect(
      str_to_upper(NOME_DISCIPLINA),
      "FMMC"
    )
  ) %>%
  print(n = Inf)