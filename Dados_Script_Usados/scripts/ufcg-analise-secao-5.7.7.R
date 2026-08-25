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
      "FMCC"
    )
  ) %>%
  print(n = Inf)


# =========================================================
# 15. VERIFICAR VALORES DA VARIÁVEL CURRICULO
# =========================================================

cat("\n=========================================================\n")
cat("DISTRIBUIÇÃO DA VARIÁVEL CURRICULO\n")
cat("=========================================================\n")

amostra %>%
  count(Curriculo, sort = TRUE) %>%
  print(n = Inf)

# =========================================================
# 16. LOCALIZAR FMCC
# =========================================================

cat("\n=========================================================\n")
cat("BUSCA POR FMCC - CURRÍCULO 1999\n")
cat("=========================================================\n")

disciplinas_1999 %>%
  filter(
    str_detect(
      str_to_upper(NOME_DISCIPLINA),
      "FMCC"
    )
  ) %>%
  print(n = Inf)


cat("\n=========================================================\n")
cat("BUSCA POR FMCC - CURRÍCULO 2017\n")
cat("=========================================================\n")

disciplinas_2017 %>%
  filter(
    str_detect(
      str_to_upper(NOME_DISCIPLINA),
      "FMCC"
    )
  ) %>%
  print(n = Inf)

# =========================================================
# 17. BUSCA AMPLA POR DISCIPLINAS RELACIONADAS À FMCC
# =========================================================

cat("\n=========================================================\n")
cat("BUSCA AMPLA - CURRÍCULO 1999\n")
cat("=========================================================\n")

disciplinas_1999 %>%
  filter(
    str_detect(
      str_to_upper(NOME_DISCIPLINA),
      "FUND|MAT|COMPUT|DISCRETA|FMCC"
    )
  ) %>%
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
  print(n = Inf)


cat("\n=========================================================\n")
cat("BUSCA AMPLA - CURRÍCULO 2017\n")
cat("=========================================================\n")

disciplinas_2017 %>%
  filter(
    str_detect(
      str_to_upper(NOME_DISCIPLINA),
      "FUND|MAT|COMPUT|DISCRETA|FMCC"
    )
  ) %>%
  print(n = Inf)

# =========================================================
# 18. LISTAR TODAS AS DISCIPLINAS DE CÁLCULO
# =========================================================

cat("\n=========================================================\n")
cat("TODOS OS CÁLCULOS - CURRÍCULO 1999\n")
cat("=========================================================\n")

disciplinas_1999 %>%
  filter(
    str_detect(
      str_to_upper(NOME_DISCIPLINA),
      "CALCULO"
    )
  ) %>%
  select(
    CODIGO_CURRICULAR,
    CODIGO_DISCIPLINA...3,
    NOME_DISCIPLINA,
    HORAS_DISCIPLIN,
    TIPO,
    SEMESTRE_IDEAL
  ) %>%
  arrange(SEMESTRE_IDEAL, NOME_DISCIPLINA) %>%
  print(n = Inf)


cat("\n=========================================================\n")
cat("TODOS OS CÁLCULOS - CURRÍCULO 2017\n")
cat("=========================================================\n")

disciplinas_2017 %>%
  filter(
    str_detect(
      str_to_upper(NOME_DISCIPLINA),
      "CALCULO"
    )
  ) %>%
  select(
    CODIGO_CURRICULAR,
    CODIGO_DISCIPLINA,
    NOME_DISCIPLINA,
    CREDITO_DISCIPLINA,
    HORAS_DISCIPLINA,
    TIPO,
    SEMESTRE_IDEAL
  ) %>%
  arrange(SEMESTRE_IDEAL, NOME_DISCIPLINA) %>%
  print(n = Inf)


# =========================================================
# 19. TESTAR OS CÓDIGOS DE CÁLCULO ENCONTRADOS
# =========================================================

codigos_calculo_1999 <- c(
  "1109050",
  "1109103"
)

codigos_calculo_2017 <- c(
  "1109105"
)

cat("\n=========================================================\n")
cat("CÓDIGOS DE CÁLCULO ENCONTRADOS NA ENROLLMENTS\n")
cat("=========================================================\n")

enrollments %>%
  filter(
    subjectCode %in% c(
      codigos_calculo_1999,
      codigos_calculo_2017
    )
  ) %>%
  count(subjectCode, sort = TRUE) %>%
  print()


# =========================================================
# 20. ALUNOS DA AMOSTRA QUE CURSARAM OS CÓDIGOS DE CÁLCULO
# =========================================================

calculos_encontrados <- enrollments %>%
  filter(
    subjectCode %in% c(
      codigos_calculo_1999,
      codigos_calculo_2017
    )
  ) %>%
  inner_join(
    amostra %>%
      select(
        Matricula,
        Curriculo,
        `Periodo de Ingresso`,
        Status,
        `Tipo de Evasao`,
        `Periodo de Evasao`
      ),
    by = c("registration" = "Matricula")
  )

cat("\n=========================================================\n")
cat("CÁLCULOS CURSADOS POR ALUNOS DA AMOSTRA\n")
cat("=========================================================\n")

calculos_encontrados %>%
  count(
    Curriculo,
    subjectCode,
    sort = TRUE
  ) %>%
  print()

# =========================================================
# 21. DETALHAMENTO DOS REGISTROS DE CÁLCULO
# =========================================================

calculos_encontrados %>%
  arrange(
    Curriculo,
    registration,
    term
  ) %>%
  select(
    registration,
    Curriculo,
    `Periodo de Ingresso`,
    subjectCode,
    term,
    classId,
    credits,
    grade,
    status
  ) %>%
  print(n = 100)

# =========================================================
# 22. INVESTIGAR FUNDAMENTOS DE MATEMÁTICA
# =========================================================

cat("\n=========================================================\n")
cat("FUNDAMENTOS DE MATEMÁTICA - CURRÍCULO 2017\n")
cat("=========================================================\n")

disciplinas_2017 %>%
  filter(
    CODIGO_DISCIPLINA %in% c(
      1411311,
      1411312
    )
  ) %>%
  print(width = Inf)


cat("\n=========================================================\n")
cat("FUNDAMENTOS DE MATEMÁTICA - CURRÍCULO 1999\n")
cat("=========================================================\n")

disciplinas_1999 %>%
  filter(
    str_detect(
      str_to_upper(NOME_DISCIPLINA),
      "MATEM"
    )
  ) %>%
  select(
    CODIGO_CURRICULAR,
    CODIGO_DISCIPLINA...3,
    CODIGO_DISCIPLINA...5,
    NOME_DISCIPLINA,
    HORAS_DISCIPLIN,
    TIPO,
    SEMESTRE_IDEAL
  ) %>%
  print(n = Inf, width = Inf)

# =========================================================
# 23. BASE ANALÍTICA - CÁLCULO I
# =========================================================

calculo_I <- enrollments %>%
  filter(
    subjectCode == "1109103"
  ) %>%
  inner_join(
    amostra %>%
      filter(
        Curriculo %in% c("1999", "2017")
      ) %>%
      select(
        Matricula,
        Curriculo,
        `Periodo de Ingresso`,
        `Forma de Ingresso`,
        Status,
        `Tipo de Evasao`,
        `Periodo de Evasao`
      ),
    by = c("registration" = "Matricula")
  )

cat("\n=========================================================\n")
cat("BASE ANALÍTICA - CÁLCULO I\n")
cat("=========================================================\n")

cat("\nRegistros:", nrow(calculo_I), "\n")

cat("\nAlunos distintos:", n_distinct(calculo_I$registration), "\n")

cat("\nPor currículo:\n")

calculo_I %>%
  count(Curriculo) %>%
  print()

# =========================================================
# 24. ALUNOS DISTINTOS DE CÁLCULO I POR CURRÍCULO
# =========================================================

calculo_I %>%
  distinct(
    registration,
    Curriculo
  ) %>%
  count(Curriculo) %>%
  print()

# =========================================================
# 25. NÚMERO DE REGISTROS/TENTATIVAS EM CÁLCULO I
# =========================================================

calculo_I %>%
  count(
    Curriculo,
    registration,
    name = "tentativas_calculo"
  ) %>%
  count(
    Curriculo,
    tentativas_calculo,
    name = "alunos"
  ) %>%
  arrange(
    Curriculo,
    tentativas_calculo
  ) %>%
  print(n = Inf)

# =========================================================
# 26. SITUAÇÃO ACADÊMICA EM CÁLCULO I
# =========================================================

calculo_I %>%
  count(
    Curriculo,
    status
  ) %>%
  group_by(Curriculo) %>%
  mutate(
    percentual = 100 * n / sum(n)
  ) %>%
  ungroup() %>%
  arrange(
    Curriculo,
    desc(n)
  ) %>%
  print()

# =========================================================
# 27. CONVERTER NOTA PARA NUMÉRICO
# =========================================================

calculo_I <- calculo_I %>%
  mutate(
    nota = parse_number(
      grade,
      locale = locale(decimal_mark = ",")
    )
  )


cat("\n=========================================================\n")
cat("RESUMO DAS NOTAS EM CÁLCULO I\n")
cat("=========================================================\n")

calculo_I %>%
  group_by(Curriculo) %>%
  summarise(
    registros = n(),
    notas_validas = sum(!is.na(nota)),
    media = mean(nota, na.rm = TRUE),
    mediana = median(nota, na.rm = TRUE),
    desvio_padrao = sd(nota, na.rm = TRUE),
    minimo = min(nota, na.rm = TRUE),
    maximo = max(nota, na.rm = TRUE)
  ) %>%
  print()

# =========================================================
# 28. RESULTADO FINAL DE CADA ALUNO EM CÁLCULO I
# =========================================================

resultado_calculo_aluno <- calculo_I %>%
  arrange(
    registration,
    term
  ) %>%
  group_by(
    registration,
    Curriculo
  ) %>%
  summarise(
    tentativas = n(),
    
    aprovado = any(
      status == "Aprovado"
    ),
    
    reprovado = any(
      status %in% c(
        "Reprovado",
        "Reprovado por Falta"
      )
    ),
    
    dispensado = any(
      status == "Dispensa"
    ),
    
    ultima_situacao = last(status),
    
    melhor_nota = ifelse(
      all(is.na(nota)),
      NA_real_,
      max(nota, na.rm = TRUE)
    ),
    
    media_notas = ifelse(
      all(is.na(nota)),
      NA_real_,
      mean(nota, na.rm = TRUE)
    ),
    
    .groups = "drop"
  )

cat("\n=========================================================\n")
cat("RESULTADO POR ALUNO - CÁLCULO I\n")
cat("=========================================================\n")

print(
  resultado_calculo_aluno %>%
    count(
      Curriculo,
      aprovado,
      sort = TRUE
    )
)


# =========================================================
# 29. DESEMPENHO DE CÁLCULO I POR ALUNO
# =========================================================

desempenho_calculo <- resultado_calculo_aluno %>%
  group_by(Curriculo) %>%
  summarise(
    
    alunos = n(),
    
    aprovados = sum(aprovado),
    
    percentual_aprovados =
      100 * aprovados / alunos,
    
    reprovados =
      sum(reprovado & !aprovado),
    
    percentual_reprovados =
      100 * reprovados / alunos,
    
    media_tentativas =
      mean(tentativas),
    
    mediana_tentativas =
      median(tentativas),
    
    .groups = "drop"
  )

cat("\n=========================================================\n")
cat("DESEMPENHO DE CÁLCULO I POR ALUNO\n")
cat("=========================================================\n")

print(desempenho_calculo)


# =========================================================
# 30. LOCALIZAR DISCIPLINAS DE CÁLCULO NO CURRÍCULO 2017
# =========================================================

disciplinas_2017 %>%
  filter(
    str_detect(
      str_to_upper(NOME_DISCIPLINA),
      "DIFERENCIAL|INTEGRAL|CALC"
    )
  ) %>%
  select(
    CODIGO_CURRICULAR,
    CODIGO_DISCIPLINA,
    NOME_DISCIPLINA,
    CREDITO_DISCIPLINA,
    HORAS_DISCIPLINA,
    TIPO,
    SEMESTRE_IDEAL
  ) %>%
  arrange(
    SEMESTRE_IDEAL
  ) %>%
  print(
    n = Inf,
    width = Inf
  )

# =========================================================
# 31. CÓDIGOS DAS DISCIPLINAS DE CÁLCULO I
# =========================================================

codigo_calculo_I_1999 <- "1109103"
codigo_calculo_I_2017 <- "1109126"

# =========================================================
# 32. BASE DE CÁLCULO I - 1999 E 2017
# =========================================================

calculo_I <- enrollments %>%
  inner_join(
    amostra %>%
      filter(
        Curriculo %in% c("1999", "2017")
      ) %>%
      select(
        Matricula,
        Curriculo,
        `Periodo de Ingresso`,
        `Forma de Ingresso`,
        Status,
        `Tipo de Evasao`,
        `Periodo de Evasao`
      ),
    by = c("registration" = "Matricula")
  ) %>%
  filter(
    (Curriculo == "1999" & subjectCode == codigo_calculo_I_1999) |
      (Curriculo == "2017" & subjectCode == codigo_calculo_I_2017)
  )
