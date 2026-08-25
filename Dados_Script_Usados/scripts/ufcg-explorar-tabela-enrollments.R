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


# =========================================================
# 5. VALIDAR REGISTRATION
# =========================================================

cat("\n=========================================================\n")
cat("VALIDAÇÃO DA MATRÍCULA - REGISTRATION\n")
cat("=========================================================\n")

# Quantidade de registros
cat("\nTotal de registros:", nrow(enrollments), "\n")

# Quantidade de matrículas distintas
cat(
  "Matrículas distintas:",
  n_distinct(enrollments$registration),
  "\n"
)

# Quantidade de valores NA
cat(
  "Registration NA:",
  sum(is.na(enrollments$registration)),
  "\n"
)

# Distribuição do tamanho da matrícula
tamanho_registration <- enrollments %>%
  mutate(
    tamanho = nchar(str_trim(registration))
  ) %>%
  count(tamanho, sort = TRUE)

cat("\nDistribuição do tamanho de registration:\n")
print(tamanho_registration)

# Verificar especificamente matrículas com 9 dígitos
registration_9 <- enrollments %>%
  filter(
    !is.na(registration),
    str_detect(str_trim(registration), "^[0-9]{9}$")
  )

cat(
  "\nRegistros com registration de exatamente 9 dígitos:",
  nrow(registration_9),
  "\n"
)

cat(
  "Matrículas distintas com 9 dígitos:",
  n_distinct(registration_9$registration),
  "\n"
)

# Exemplos
cat("\nPrimeiras matrículas encontradas:\n")

print(
  registration_9 %>%
    distinct(registration) %>%
    head(20)
)


# =========================================================
# 6. INSPECIONAR A AMOSTRA FINAL
# =========================================================

cat("\n=========================================================\n")
cat("AMOSTRA FINAL DA DISSERTAÇÃO\n")
cat("=========================================================\n")

cat("\nLinhas:", nrow(amostra), "\n")
cat("Colunas:", ncol(amostra), "\n")

cat("\nNomes das colunas:\n")
print(names(amostra))

cat("\nPrimeiras linhas:\n")
print(head(amostra))

cat("\nEstrutura:\n")
glimpse(amostra)


# =========================================================
# 7. VALIDAR MATRÍCULAS
# AMOSTRA × ENROLLMENTS
# =========================================================

cat("\n=========================================================\n")
cat("VALIDAÇÃO DAS MATRÍCULAS\n")
cat("AMOSTRA × ENROLLMENTS\n")
cat("=========================================================\n")

# ---------------------------------------------------------
# 7.1 Padronizar matrícula da amostra
# ---------------------------------------------------------

amostra <- amostra %>%
  mutate(
    matricula_padronizada = as.character(Matricula)
  )

# ---------------------------------------------------------
# 7.2 Padronizar matrícula de enrollments
# ---------------------------------------------------------

enrollments <- enrollments %>%
  mutate(
    registration_padronizada = str_trim(registration)
  )

# ---------------------------------------------------------
# 7.3 Quantidades básicas
# ---------------------------------------------------------

cat(
  "\nAlunos na amostra:",
  nrow(amostra),
  "\n"
)

cat(
  "Matrículas distintas na amostra:",
  n_distinct(amostra$matricula_padronizada),
  "\n"
)

cat(
  "Matrículas distintas em enrollments:",
  n_distinct(enrollments$registration_padronizada),
  "\n"
)

# ---------------------------------------------------------
# 7.4 Matrículas da amostra encontradas em enrollments
# ---------------------------------------------------------

matriculas_match <- amostra %>%
  distinct(matricula_padronizada) %>%
  inner_join(
    enrollments %>%
      distinct(registration_padronizada),
    by = c(
      "matricula_padronizada" =
        "registration_padronizada"
    )
  )

cat(
  "\nMatrículas da amostra encontradas:",
  nrow(matriculas_match),
  "\n"
)

cat(
  "Percentual encontrado:",
  round(
    100 * nrow(matriculas_match) /
      n_distinct(amostra$matricula_padronizada),
    2
  ),
  "%\n"
)

# ---------------------------------------------------------
# 7.5 Matrículas SEM correspondência
# ---------------------------------------------------------

matriculas_sem_match <- amostra %>%
  distinct(matricula_padronizada) %>%
  anti_join(
    enrollments %>%
      distinct(registration_padronizada),
    by = c(
      "matricula_padronizada" =
        "registration_padronizada"
    )
  )

cat(
  "\nMatrículas da amostra SEM correspondência:",
  nrow(matriculas_sem_match),
  "\n"
)

cat("\nPrimeiras matrículas sem correspondência:\n")

print(
  head(
    matriculas_sem_match,
    30
  )
)


# =========================================================
# 9. INVESTIGAR AS 5 MATRÍCULAS SEM CORRESPONDÊNCIA
# =========================================================

amostra %>%
  filter(
    matricula_padronizada %in%
      matriculas_sem_match$matricula_padronizada
  ) %>%
  select(
    Matricula,
    `Periodo de Ingresso`,
    `Curriculo`,
    `Curriculo Entrada`,
    Status,
    `Tipo de Evasao`,
    `Periodo de Evasao`
  ) %>%
  arrange(Matricula) %>%
  print()

# =========================================================
# 10. PROCURAR POSSÍVEIS VARIAÇÕES
# =========================================================

matriculas_problema <- matriculas_sem_match$matricula_padronizada

for (mat in matriculas_problema) {
  
  cat("\n---------------------------------------------\n")
  cat("Matrícula:", mat, "\n")
  
  encontrados <- enrollments %>%
    filter(
      str_detect(
        registration,
        fixed(mat)
      )
    ) %>%
    distinct(registration)
  
  print(encontrados)
}


# =========================================================
# 11. QUANTIDADE DE REGISTROS DE DISCIPLINAS POR ALUNO
# =========================================================

disciplinas_por_aluno <- enrollments %>%
  semi_join(
    amostra,
    by = c(
      "registration_padronizada" =
        "matricula_padronizada"
    )
  ) %>%
  group_by(registration_padronizada) %>%
  summarise(
    qtd_registros = n(),
    qtd_disciplinas = n_distinct(subjectCode),
    qtd_periodos = n_distinct(term),
    .groups = "drop"
  )

cat("\n=========================================================\n")
cat("HISTÓRICO ACADÊMICO DOS ALUNOS DA AMOSTRA\n")
cat("=========================================================\n")

cat(
  "\nAlunos com registros em enrollments:",
  nrow(disciplinas_por_aluno),
  "\n"
)

cat(
  "\nResumo dos registros por aluno:\n"
)

print(
  summary(
    disciplinas_por_aluno$qtd_registros
  )
)

cat(
  "\nResumo das disciplinas distintas por aluno:\n"
)

print(
  summary(
    disciplinas_por_aluno$qtd_disciplinas
  )
)

# =========================================================
# 12. INTERVALO TEMPORAL DE ENROLLMENTS
# =========================================================

cat("\n=========================================================\n")
cat("INTERVALO TEMPORAL DE ENROLLMENTS\n")
cat("=========================================================\n")

enrollments %>%
  summarise(
    menor_term = min(term, na.rm = TRUE),
    maior_term = max(term, na.rm = TRUE),
    termos_distintos = n_distinct(term)
  ) %>%
  print()

cat("\nDistribuição dos termos:\n")

enrollments %>%
  count(term) %>%
  arrange(term) %>%
  print(n = 100)


# =========================================================
# 13. TERM × PERÍODO DE INGRESSO
# =========================================================

historico_amostra <- enrollments %>%
  inner_join(
    amostra %>%
      select(
        matricula_padronizada,
        `Periodo de Ingresso`,
        Curriculo,
        `Curriculo Entrada`
      ),
    by = c(
      "registration_padronizada" =
        "matricula_padronizada"
    )
  )

cat("\n=========================================================\n")
cat("HISTÓRICO DOS ALUNOS DA AMOSTRA\n")
cat("=========================================================\n")

cat(
  "\nRegistros de enrollments pertencentes à amostra:",
  nrow(historico_amostra),
  "\n"
)

cat(
  "Alunos encontrados:",
  n_distinct(
    historico_amostra$registration_padronizada
  ),
  "\n"
)

# =========================================================
# 14. PADRONIZAR PERÍODO
# =========================================================

historico_amostra <- historico_amostra %>%
  mutate(
    periodo_ingresso = paste0(
      substr(
        as.character(`Periodo de Ingresso`),
        1,
        4
      ),
      ".",
      substr(
        as.character(`Periodo de Ingresso`),
        5,
        5
      )
    )
  )

# =========================================================
# 15. VERIFICAR DISCIPLINAS ANTES DO INGRESSO
# =========================================================

historico_amostra <- historico_amostra %>%
  mutate(
    ano_term = as.numeric(substr(term, 1, 4)),
    semestre_term = as.numeric(substr(term, 6, 6)),
    
    ano_ingresso = as.numeric(
      substr(periodo_ingresso, 1, 4)
    ),
    
    semestre_ingresso = as.numeric(
      substr(periodo_ingresso, 6, 6)
    )
  )

historico_amostra <- historico_amostra %>%
  mutate(
    antes_ingresso =
      ano_term < ano_ingresso |
      (
        ano_term == ano_ingresso &
          semestre_term < semestre_ingresso
      )
  )

cat("\n=========================================================\n")
cat("DISCIPLINAS ANTES DO INGRESSO\n")
cat("=========================================================\n")

print(
  table(
    historico_amostra$antes_ingresso,
    useNA = "ifany"
  )
)

# =========================================================
# 16. CARREGAR DISCIPLINAS
# =========================================================

disciplinas_1999 <- read_csv2(
  file.path(
    pasta_dados,
    "disciplinas_1999.csv"
  ),
  show_col_types = FALSE
)

disciplinas_2017 <- read_csv2(
  file.path(
    pasta_dados,
    "disciplinas_2017.csv"
  ),
  show_col_types = FALSE
)

cat("\n=========================================================\n")
cat("DISCIPLINAS 1999\n")
cat("=========================================================\n")

cat(
  "\nLinhas:",
  nrow(disciplinas_1999),
  "\n"
)

cat(
  "Colunas:",
  ncol(disciplinas_1999),
  "\n"
)

print(names(disciplinas_1999))

glimpse(disciplinas_1999)

cat("\n=========================================================\n")
cat("DISCIPLINAS 2017\n")
cat("=========================================================\n")

cat(
  "\nLinhas:",
  nrow(disciplinas_2017),
  "\n"
)

cat(
  "Colunas:",
  ncol(disciplinas_2017),
  "\n"
)

print(names(disciplinas_2017))

glimpse(disciplinas_2017)


# =========================================================
# 17. INVESTIGAR TERM ANTERIOR AO INGRESSO
# =========================================================

cat("\n=========================================================\n")
cat("REGISTROS COM TERM ANTERIOR AO INGRESSO\n")
cat("=========================================================\n")

antes_ingresso <- historico_amostra %>%
  filter(antes_ingresso == TRUE) %>%
  select(
    registration_padronizada,
    `Periodo de Ingresso`,
    periodo_ingresso,
    term,
    subjectCode,
    classId,
    credits,
    grade,
    status,
    Curriculo,
    `Curriculo Entrada`
  ) %>%
  arrange(
    registration_padronizada,
    term
  )

cat(
  "\nTotal de registros:",
  nrow(antes_ingresso),
  "\n"
)

cat(
  "Alunos distintos:",
  n_distinct(
    antes_ingresso$registration_padronizada
  ),
  "\n"
)

print(
  head(
    antes_ingresso,
    50
  )
)

# Quantidade de registros anteriores ao ingresso por aluno

antes_ingresso %>%
  count(
    registration_padronizada,
    `Periodo de Ingresso`,
    Curriculo,
    sort = TRUE
  ) %>%
  print(n = 50)


# =========================================================
# 18. TERM FORA DO PADRÃO .1 / .2
# =========================================================

enrollments %>%
  filter(
    !str_detect(
      term,
      "^[0-9]{4}\\.[12]$"
    )
  ) %>%
  count(term, sort = TRUE) %>%
  print()

# =========================================================
# 19. INVESTIGAR DUPLICIDADE DOS CÓDIGOS - CURRÍCULO 1999
# =========================================================

cat("\n=========================================================\n")
cat("CÓDIGOS DE DISCIPLINA - CURRÍCULO 1999\n")
cat("=========================================================\n")

disciplinas_1999 %>%
  select(
    CODIGO_DISCIPLINA...3,
    CODIGO_DISCIPLINA...5
  ) %>%
  mutate(
    codigo_3 = as.character(CODIGO_DISCIPLINA...3),
    codigo_5 = as.character(CODIGO_DISCIPLINA...5),
    iguais = codigo_3 == codigo_5
  ) %>%
  count(iguais, useNA = TRUE) %>%
  print()


# =========================================================
# 20. CLASSIFICAR O TIPO DE RELAÇÃO ENTRE TERM E INGRESSO
# =========================================================

historico_amostra <- historico_amostra %>%
  mutate(
    relacao_term_ingresso = case_when(
      
      str_detect(term, "\\.0$") ~
        "TERM_ESPECIAL_0",
      
      antes_ingresso ~
        "ANTES_DO_INGRESSO",
      
      TRUE ~
        "IGUAL_OU_APOS_INGRESSO"
    )
  )

cat("\n=========================================================\n")
cat("RELAÇÃO ENTRE TERM E PERÍODO DE INGRESSO\n")
cat("=========================================================\n")

print(
  table(
    historico_amostra$relacao_term_ingresso,
    useNA = "ifany"
  )
)

# =========================================================
# 20. INVESTIGAR OS 17 CASOS REALMENTE ANTERIORES
# =========================================================

historico_amostra %>%
  filter(
    antes_ingresso == TRUE,
    !str_detect(term, "\\.0$")
  ) %>%
  select(
    registration_padronizada,
    `Periodo de Ingresso`,
    periodo_ingresso,
    term,
    subjectCode,
    classId,
    credits,
    grade,
    status,
    Curriculo,
    `Curriculo Entrada`
  ) %>%
  arrange(
    registration_padronizada,
    term
  ) %>%
  print(n = 50)

# =========================================================
# 21. COMPARAR OS DOIS CÓDIGOS - CURRÍCULO 1999
# =========================================================

disciplinas_1999 %>%
  select(
    CODIGO_CURSO,
    CODIGO_CURRICULAR,
    CODIGO_DISCIPLINA...3,
    NOME_DISCIPLINA,
    CODIGO_DISCIPLINA...5,
    HORAS_DISCIPLIN,
    TIPO,
    SEMESTRE_IDEAL
  ) %>%
  mutate(
    codigo_3 = as.character(CODIGO_DISCIPLINA...3),
    codigo_5 = as.character(CODIGO_DISCIPLINA...5)
  ) %>%
  filter(
    !is.na(codigo_3) | !is.na(codigo_5)
  ) %>%
  head(50) %>%
  print(n = 50)


# =========================================================
# 22. QUANTIDADE DE NA NOS DOIS CÓDIGOS
# =========================================================

cat("\nNA em CODIGO_DISCIPLINA...3:",
    sum(is.na(disciplinas_1999$CODIGO_DISCIPLINA...3)),
    "\n")

cat("NA em CODIGO_DISCIPLINA...5:",
    sum(is.na(disciplinas_1999$CODIGO_DISCIPLINA...5)),
    "\n")
