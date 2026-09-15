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

# =========================================================
# 33. COBERTURA DE CÁLCULO I
# =========================================================

cobertura_calculo_I <- calculo_I %>%
  distinct(
    registration,
    Curriculo
  ) %>%
  count(
    Curriculo,
    name = "alunos_calculo_I"
  ) %>%
  left_join(
    amostra %>%
      filter(Curriculo %in% c("1999", "2017")) %>%
      count(
        Curriculo,
        name = "alunos_amostra"
      ),
    by = "Curriculo"
  ) %>%
  mutate(
    percentual_cobertura =
      100 * alunos_calculo_I / alunos_amostra
  )

print(cobertura_calculo_I)


# =========================================================
# 34. TRATAMENTO DAS NOTAS
# =========================================================

calculo_I <- calculo_I %>%
  mutate(
    nota = if_else(
      str_trim(grade) == "-" | is.na(grade),
      NA_real_,
      parse_number(
        grade,
        locale = locale(decimal_mark = ",")
      )
    )
  )


calculo_I %>%
  group_by(Curriculo) %>%
  summarise(
    registros = n(),
    notas_validas = sum(!is.na(nota)),
    percentual_notas_validas =
      100 * notas_validas / registros,
    media = mean(nota, na.rm = TRUE),
    mediana = median(nota, na.rm = TRUE),
    desvio_padrao = sd(nota, na.rm = TRUE),
    minimo = min(nota, na.rm = TRUE),
    maximo = max(nota, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print()


# =========================================================
# 35. TENTATIVAS DE CÁLCULO I
# =========================================================

tentativas_calculo <- calculo_I %>%
  count(
    registration,
    Curriculo,
    name = "tentativas_calculo"
  )

tentativas_calculo %>%
  count(
    Curriculo,
    tentativas_calculo,
    name = "alunos"
  ) %>%
  group_by(Curriculo) %>%
  mutate(
    percentual =
      100 * alunos / sum(alunos)
  ) %>%
  ungroup() %>%
  arrange(
    Curriculo,
    tentativas_calculo
  ) %>%
  print()


# =========================================================
# 36. SITUAÇÃO ACADÊMICA - CÁLCULO I
# =========================================================

calculo_I %>%
  count(
    Curriculo,
    status,
    name = "registros"
  ) %>%
  group_by(Curriculo) %>%
  mutate(
    percentual =
      100 * registros / sum(registros)
  ) %>%
  ungroup() %>%
  arrange(
    Curriculo,
    desc(registros)
  ) %>%
  print()


# =========================================================
# 37. RESULTADO DE CADA ALUNO EM CÁLCULO I
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
    
    teve_aprovacao = any(
      status == "Aprovado"
    ),
    
    teve_reprovacao = any(
      status %in% c(
        "Reprovado",
        "Reprovado por Falta"
      )
    ),
    
    teve_dispensa = any(
      status == "Dispensa"
    ),
    
    nota_maxima = ifelse(
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

resultado_calculo_aluno <- resultado_calculo_aluno %>%
  mutate(
    resultado_calculo = case_when(
      teve_aprovacao ~ "Aprovado",
      teve_dispensa ~ "Dispensa",
      teve_reprovacao ~ "Sem aprovação",
      TRUE ~ "Outro"
    )
  )


resultado_calculo_aluno %>%
  count(
    Curriculo,
    resultado_calculo
  ) %>%
  group_by(Curriculo) %>%
  mutate(
    percentual =
      100 * n / sum(n)
  ) %>%
  ungroup() %>%
  print()

# =========================================================
# 38. CÁLCULO I × EVASÃO
# =========================================================

calculo_evasao <- resultado_calculo_aluno %>%
  left_join(
    amostra %>%
      select(
        Matricula,
        Curriculo,
        Status,
        `Tipo de Evasao`,
        `Periodo de Evasao`
      ),
    by = c(
      "registration" = "Matricula",
      "Curriculo" = "Curriculo"
    )
  )

calculo_evasao %>%
  count(
    Curriculo,
    resultado_calculo,
    Status
  ) %>%
  group_by(
    Curriculo,
    resultado_calculo
  ) %>%
  mutate(
    percentual =
      100 * n / sum(n)
  ) %>%
  ungroup() %>%
  print()

# =========================================================
# 39. MÉDIA DE TENTATIVAS - CÁLCULO I
# =========================================================

tentativas_calculo %>%
  group_by(Curriculo) %>%
  summarise(
    alunos = n(),
    media_tentativas = mean(tentativas_calculo),
    mediana_tentativas = median(tentativas_calculo),
    max_tentativas = max(tentativas_calculo),
    .groups = "drop"
  ) %>%
  print()


# =========================================================
# 40. RESULTADO NA PRIMEIRA TENTATIVA
# =========================================================

primeira_tentativa_calculo <- calculo_I %>%
  arrange(
    registration,
    term,
    classId
  ) %>%
  group_by(
    registration,
    Curriculo
  ) %>%
  slice(1) %>%
  ungroup()

primeira_tentativa_calculo %>%
  count(
    Curriculo,
    status
  ) %>%
  group_by(Curriculo) %>%
  mutate(
    percentual = 100 * n / sum(n)
  ) %>%
  ungroup() %>%
  print()

primeira_tentativa_calculo <- primeira_tentativa_calculo %>%
  mutate(
    resultado_primeira_tentativa = case_when(
      status == "Aprovado" ~ "Aprovado",
      status == "Dispensa" ~ "Dispensa",
      status %in% c(
        "Reprovado",
        "Reprovado por Falta"
      ) ~ "Reprovado",
      TRUE ~ "Outro"
    )
  )

primeira_tentativa_calculo %>%
  count(
    Curriculo,
    resultado_primeira_tentativa
  ) %>%
  group_by(Curriculo) %>%
  mutate(
    percentual = 100 * n / sum(n)
  ) %>%
  ungroup() %>%
  print()

calculo_evasao <- calculo_evasao %>%
  mutate(
    evadiu = if_else(
      Status == "INATIVO" &
        `Tipo de Evasao` != "GRADUADO",
      "Evadido",
      "Não evadido"
    )
  )


calculo_evasao %>%
  count(
    Curriculo,
    resultado_calculo,
    evadiu
  ) %>%
  group_by(
    Curriculo,
    resultado_calculo
  ) %>%
  mutate(
    percentual =
      100 * n / sum(n)
  ) %>%
  ungroup() %>%
  print(n = Inf)


# =========================================================
# 42. TESTE QUI-QUADRADO:
# RESULTADO EM CÁLCULO I × EVASÃO
# =========================================================

teste_calculo_evasao <- calculo_evasao %>%
  filter(
    resultado_calculo %in% c(
      "Aprovado",
      "Sem aprovação"
    )
  ) %>%
  count(
    Curriculo,
    resultado_calculo,
    evadiu
  )

print(teste_calculo_evasao)

# =========================================================
# 43. QUI-QUADRADO POR CURRÍCULO
# =========================================================

tabela_1999 <- teste_calculo_evasao %>%
  filter(Curriculo == "1999") %>%
  select(
    resultado_calculo,
    evadiu,
    n
  ) %>%
  tidyr::pivot_wider(
    names_from = evadiu,
    values_from = n,
    values_fill = 0
  )

tabela_2017 <- teste_calculo_evasao %>%
  filter(Curriculo == "2017") %>%
  select(
    resultado_calculo,
    evadiu,
    n
  ) %>%
  tidyr::pivot_wider(
    names_from = evadiu,
    values_from = n,
    values_fill = 0
  )

print(tabela_1999)
print(tabela_2017)


# =========================================================
# 44. TESTES
# =========================================================

matriz_1999 <- as.matrix(
  tabela_1999[, -1]
)

rownames(matriz_1999) <- tabela_1999$resultado_calculo

matriz_2017 <- as.matrix(
  tabela_2017[, -1]
)

rownames(matriz_2017) <- tabela_2017$resultado_calculo


cat("\n=========================================================\n")
cat("QUI-QUADRADO - CURRÍCULO 1999\n")
cat("=========================================================\n")

teste_1999 <- chisq.test(matriz_1999)

print(teste_1999)


cat("\n=========================================================\n")
cat("QUI-QUADRADO - CURRÍCULO 2017\n")
cat("=========================================================\n")

teste_2017 <- chisq.test(matriz_2017)

print(teste_2017)

# =========================================================
# 45. V DE CRAMÉR
# =========================================================

calcular_v_cramer <- function(tabela) {
  
  teste <- chisq.test(tabela)
  
  n <- sum(tabela)
  
  r <- nrow(tabela)
  k <- ncol(tabela)
  
  sqrt(
    as.numeric(teste$statistic) /
      (n * min(r - 1, k - 1))
  )
}


v_1999 <- calcular_v_cramer(matriz_1999)
v_2017 <- calcular_v_cramer(matriz_2017)

cat("\nV de Cramér - 1999:", v_1999, "\n")
cat("V de Cramér - 2017:", v_2017, "\n")


# =========================================================
# 46. ALUNOS 2017 SEM REGISTRO DE CÁLCULO I
# =========================================================

alunos_2017_sem_calculo <- amostra %>%
  filter(
    Curriculo == "2017"
  ) %>%
  anti_join(
    calculo_I %>%
      filter(
        Curriculo == "2017"
      ) %>%
      distinct(registration),
    by = c(
      "Matricula" = "registration"
    )
  )

cat("\n=========================================================\n")
cat("ALUNOS 2017 SEM REGISTRO DE CÁLCULO I\n")
cat("=========================================================\n")

cat("\nQuantidade:", nrow(alunos_2017_sem_calculo), "\n")

alunos_2017_sem_calculo %>%
  count(
    `Periodo de Ingresso`
  ) %>%
  arrange(
    `Periodo de Ingresso`
  ) %>%
  print(n = Inf)


# =========================================================
# 47. SITUAÇÃO DOS ALUNOS 2017 SEM CÁLCULO I
# =========================================================

alunos_2017_sem_calculo %>%
  count(
    `Periodo de Ingresso`,
    Status,
    `Tipo de Evasao`
  ) %>%
  arrange(
    `Periodo de Ingresso`
  ) %>%
  print(n = Inf)

# =========================================================
# 48. HISTÓRICO DOS ALUNOS 2017 SEM CÁLCULO I 1109126
# =========================================================

historico_2017_sem_calculo <- enrollments %>%
  inner_join(
    alunos_2017_sem_calculo %>%
      select(
        Matricula,
        `Periodo de Ingresso`,
        Status,
        `Tipo de Evasao`
      ),
    by = c(
      "registration" = "Matricula"
    )
  )

cat("\n=========================================================\n")
cat("HISTÓRICO DOS ALUNOS 2017 SEM 1109126\n")
cat("=========================================================\n")

cat("\nRegistros:", nrow(historico_2017_sem_calculo), "\n")

cat("\nDisciplinas mais frequentes:\n")

historico_2017_sem_calculo %>%
  count(
    subjectCode,
    sort = TRUE
  ) %>%
  print(n = 50)


# =========================================================
# 49. POSSÍVEIS CÓDIGOS ALTERNATIVOS DE CÁLCULO
# =========================================================

historico_2017_sem_calculo %>%
  filter(
    subjectCode %in% c(
      "1109103",
      "1109050",
      "1109126",
      "1109131",
      "1109105",
      "1109128"
    )
  ) %>%
  count(
    subjectCode,
    sort = TRUE
  ) %>%
  print()

# =========================================================
# 50. TODOS OS CÓDIGOS DE CÁLCULO NO HISTÓRICO
# =========================================================

codigos_calculo <- disciplinas_2017 %>%
  filter(
    str_detect(
      str_to_upper(NOME_DISCIPLINA),
      "CALC|C.LCULO|DIFERENCIAL|INTEGRAL"
    )
  ) %>%
  pull(CODIGO_DISCIPLINA) %>%
  as.character()

cat("\nCódigos encontrados no currículo 2017:\n")
print(codigos_calculo)

cat("\nOcorrências desses códigos nos 216 alunos:\n")

historico_2017_sem_calculo %>%
  filter(
    subjectCode %in% codigos_calculo
  ) %>%
  count(
    subjectCode,
    sort = TRUE
  ) %>%
  print()

# =========================================================
# 51. CURRÍCULO 2017 POR PERÍODO DE INGRESSO
# =========================================================

amostra %>%
  filter(
    Curriculo == "2017"
  ) %>%
  count(
    `Periodo de Ingresso`
  ) %>%
  arrange(
    `Periodo de Ingresso`
  ) %>%
  print(n = Inf)


# =========================================================
# 52. CURRÍCULO × PERÍODO DE INGRESSO
# =========================================================

amostra %>%
  count(
    Curriculo,
    `Periodo de Ingresso`
  ) %>%
  arrange(
    Curriculo,
    `Periodo de Ingresso`
  ) %>%
  print(n = Inf)

# =========================================================
# 53. MATRIZ DE INGRESSO × CURRÍCULO
# =========================================================

matriz_ingresso_curriculo <- amostra %>%
  count(
    `Periodo de Ingresso`,
    Curriculo
  ) %>%
  tidyr::pivot_wider(
    names_from = Curriculo,
    values_from = n,
    values_fill = 0
  ) %>%
  arrange(
    `Periodo de Ingresso`
  )

print(
  matriz_ingresso_curriculo,
  n = Inf
)

# =========================================================
# 54. ALUNOS QUE INGRESSARAM ANTES DE 2018
#     MAS ESTÃO CLASSIFICADOS NO CURRÍCULO 2017
# =========================================================

alunos_transicao_2017 <- amostra %>%
  filter(
    Curriculo == "2017",
    `Periodo de Ingresso` < 20181
  ) %>%
  arrange(
    `Periodo de Ingresso`,
    Matricula
  )

cat("\n=========================================================\n")
cat("ALUNOS DE INGRESSO ANTERIOR A 2018 CLASSIFICADOS NO 2017\n")
cat("=========================================================\n")

cat("\nQuantidade:", nrow(alunos_transicao_2017), "\n")

alunos_transicao_2017 %>%
  count(
    `Periodo de Ingresso`
  ) %>%
  print(n = Inf)


# =========================================================
# 55. ALUNOS 2017 SEM 1109126 QUE CURSARAM 1109103
# =========================================================

historico_1109103_transicao <- historico_2017_sem_calculo %>%
  filter(
    subjectCode == "1109103"
  ) %>%
  select(
    registration,
    `Periodo de Ingresso`,
    term,
    subjectCode,
    grade,
    status,
    `Tipo de Evasao`
  ) %>%
  arrange(
    `Periodo de Ingresso`,
    registration,
    term
  )

cat("\n=========================================================\n")
cat("1109103 ENTRE ALUNOS 2017 SEM 1109126\n")
cat("=========================================================\n")

cat(
  "\nAlunos distintos:",
  n_distinct(historico_1109103_transicao$registration),
  "\n"
)

historico_1109103_transicao %>%
  count(
    `Periodo de Ingresso`
  ) %>%
  print(n = Inf)

historico_1109103_transicao %>%
  count(
    `Periodo de Ingresso`,
    term,
    status
  ) %>%
  arrange(
    `Periodo de Ingresso`,
    term
  ) %>%
  print(n = Inf)


# =========================================================
# 56. DEFINIÇÃO DA POPULAÇÃO COMPARÁVEL
# =========================================================

amostra_comparavel <- amostra %>%
  filter(
    (Curriculo == "1999" &
       `Periodo de Ingresso` >= 20111 &
       `Periodo de Ingresso` <= 20152) |
      
      (Curriculo == "2017" &
         `Periodo de Ingresso` >= 20181 &
         `Periodo de Ingresso` <= 20222)
  )

cat("\n=========================================================\n")
cat("POPULAÇÃO COMPARÁVEL - CURRÍCULOS 1999 × 2017\n")
cat("=========================================================\n")

amostra_comparavel %>%
  count(Curriculo) %>%
  print()

# =========================================================
# 57. CASOS DE TRANSIÇÃO / FORA DA JANELA COMPARÁVEL
# =========================================================

casos_transicao <- amostra %>%
  anti_join(
    amostra_comparavel %>%
      select(Matricula),
    by = "Matricula"
  )

cat("\n=========================================================\n")
cat("CASOS FORA DA JANELA COMPARÁVEL\n")
cat("=========================================================\n")

casos_transicao %>%
  count(
    Curriculo,
    `Periodo de Ingresso`
  ) %>%
  arrange(
    Curriculo,
    `Periodo de Ingresso`
  ) %>%
  print(n = Inf)


# =========================================================
# 58. ALUNOS 2017 DA JANELA COMPARÁVEL SEM CÁLCULO I
# =========================================================

calculo_I_2017_comparavel <- enrollments %>%
  inner_join(
    amostra_comparavel %>%
      filter(Curriculo == "2017") %>%
      select(
        Matricula,
        `Periodo de Ingresso`,
        Curriculo,
        Status,
        `Tipo de Evasao`
      ),
    by = c("registration" = "Matricula")
  ) %>%
  filter(
    subjectCode == codigo_calculo_I_2017
  ) %>%
  distinct(
    registration
  )

alunos_2017_comparavel_sem_calculo <- amostra_comparavel %>%
  filter(
    Curriculo == "2017"
  ) %>%
  anti_join(
    calculo_I_2017_comparavel,
    by = c(
      "Matricula" = "registration"
    )
  )

cat("\n=========================================================\n")
cat("2017 COMPARÁVEL SEM CÁLCULO I\n")
cat("=========================================================\n")

cat(
  "\nQuantidade:",
  nrow(alunos_2017_comparavel_sem_calculo),
  "\n"
)

alunos_2017_comparavel_sem_calculo %>%
  count(
    `Periodo de Ingresso`
  ) %>%
  arrange(
    `Periodo de Ingresso`
  ) %>%
  print(n = Inf)


# =========================================================
# 59. FUNDAMENTOS DE MATEMÁTICA PARA CIÊNCIA DA COMPUTAÇÃO
# =========================================================

codigo_fmcc_I_2017 <- "1411311"
codigo_fmcc_II_2017 <- "1411312"

fmcc <- enrollments %>%
  inner_join(
    amostra_comparavel %>%
      filter(Curriculo == "2017") %>%
      select(
        Matricula,
        Curriculo,
        `Periodo de Ingresso`,
        Status,
        `Tipo de Evasao`,
        `Periodo de Evasao`
      ),
    by = c("registration" = "Matricula")
  ) %>%
  filter(
    subjectCode %in% c(
      codigo_fmcc_I_2017,
      codigo_fmcc_II_2017
    )
  ) %>%
  mutate(
    disciplina = case_when(
      subjectCode == codigo_fmcc_I_2017 ~ "FMCC I",
      subjectCode == codigo_fmcc_II_2017 ~ "FMCC II",
      TRUE ~ "Outro"
    )
  )

# =========================================================
# 60. COBERTURA DE FMCC
# =========================================================

fmcc %>%
  distinct(
    registration,
    Curriculo,
    disciplina
  ) %>%
  count(
    disciplina,
    name = "alunos"
  ) %>%
  print()


# =========================================================
# AUDITORIA DOS CÓDIGOS DE FMCC
# =========================================================

fmcc %>%
  count(
    subjectCode,
    disciplina,
    sort = TRUE
  ) %>%
  print(n = Inf)


enrollments %>%
  filter(
    subjectCode %in% c(
      "1411311",
      "1411312"
    )
  ) %>%
  count(
    subjectCode,
    sort = TRUE
  )


enrollments %>%
  inner_join(
    amostra_comparavel %>%
      filter(Curriculo == "2017") %>%
      select(Matricula, Curriculo),
    by = c("registration" = "Matricula")
  ) %>%
  filter(
    subjectCode %in% c(
      "1411311",
      "1411312"
    )
  ) %>%
  count(
    subjectCode,
    name = "registros"
  )


# Alunos que aparecem nas duas disciplinas

fmcc %>%
  distinct(
    registration,
    disciplina
  ) %>%
  count(
    registration
  ) %>%
  count(
    n,
    name = "alunos"
  ) %>%
  arrange(n)

disciplinas_2017 %>%
  filter(
    CODIGO_DISCIPLINA %in% c(
      1411311,
      1411312
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
  print()

# ==


fmcc %>%
  distinct(
    registration,
    disciplina
  ) %>%
  group_by(registration) %>%
  summarise(
    fmcc_I = any(disciplina == "FMCC I"),
    fmcc_II = any(disciplina == "FMCC II"),
    .groups = "drop"
  ) %>%
  mutate(
    trajeto = case_when(
      fmcc_I & fmcc_II ~ "FMCC I + FMCC II",
      fmcc_I & !fmcc_II ~ "Somente FMCC I",
      !fmcc_I & fmcc_II ~ "Somente FMCC II",
      TRUE ~ "Nenhuma"
    )
  ) %>%
  count(trajeto)

# =========================================================
# 61. SITUAÇÃO ACADÊMICA - FMCC
# =========================================================

fmcc %>%
  count(
    disciplina,
    status
  ) %>%
  group_by(disciplina) %>%
  mutate(
    percentual = 100 * n / sum(n)
  ) %>%
  ungroup() %>%
  arrange(
    disciplina,
    desc(n)
  ) %>%
  print(n = Inf)

# =========================================================
# 62. TENTATIVAS POR DISCIPLINA
# =========================================================

tentativas_fmcc <- fmcc %>%
  count(
    registration,
    Curriculo,
    disciplina,
    name = "tentativas"
  )

tentativas_fmcc %>%
  count(
    disciplina,
    tentativas,
    name = "alunos"
  ) %>%
  group_by(disciplina) %>%
  mutate(
    percentual = 100 * alunos / sum(alunos)
  ) %>%
  ungroup() %>%
  arrange(
    disciplina,
    tentativas
  ) %>%
  print(n = Inf)


# =========================================================
# 62. TENTATIVAS POR DISCIPLINA
# =========================================================

tentativas_fmcc <- fmcc %>%
  count(
    registration,
    Curriculo,
    disciplina,
    name = "tentativas"
  )

tentativas_fmcc %>%
  count(
    disciplina,
    tentativas,
    name = "alunos"
  ) %>%
  group_by(disciplina) %>%
  mutate(
    percentual = 100 * alunos / sum(alunos)
  ) %>%
  ungroup() %>%
  arrange(
    disciplina,
    tentativas
  ) %>%
  print(n = Inf)


tentativas_fmcc %>%
  group_by(disciplina) %>%
  summarise(
    alunos = n(),
    media_tentativas = mean(tentativas),
    mediana_tentativas = median(tentativas),
    max_tentativas = max(tentativas),
    .groups = "drop"
  ) %>%
  print()


# =========================================================
# 63. NOTAS - FMCC
# =========================================================

fmcc <- fmcc %>%
  mutate(
    nota = if_else(
      str_trim(grade) == "-" | is.na(grade),
      NA_real_,
      parse_number(
        grade,
        locale = locale(decimal_mark = ",")
      )
    )
  )

fmcc %>%
  group_by(disciplina) %>%
  summarise(
    registros = n(),
    notas_validas = sum(!is.na(nota)),
    percentual_notas_validas =
      100 * notas_validas / registros,
    media = mean(nota, na.rm = TRUE),
    mediana = median(nota, na.rm = TRUE),
    desvio_padrao = sd(nota, na.rm = TRUE),
    minimo = min(nota, na.rm = TRUE),
    maximo = max(nota, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print()

# =========================================================
# 64. ALUNOS COM FMCC I + FMCC II
# =========================================================

alunos_fmcc_completo <- fmcc %>%
  distinct(
    registration,
    disciplina
  ) %>%
  group_by(registration) %>%
  summarise(
    tem_fmcc_I = any(disciplina == "FMCC I"),
    tem_fmcc_II = any(disciplina == "FMCC II"),
    .groups = "drop"
  ) %>%
  filter(
    tem_fmcc_I,
    tem_fmcc_II
  )

cat(
  "\nAlunos com FMCC I + FMCC II:",
  nrow(alunos_fmcc_completo),
  "\n"
)


# =========================================================
# 64. ALUNOS COM FMCC I + FMCC II
# =========================================================

alunos_fmcc_completo <- fmcc %>%
  distinct(
    registration,
    disciplina
  ) %>%
  group_by(registration) %>%
  summarise(
    tem_fmcc_I = any(disciplina == "FMCC I"),
    tem_fmcc_II = any(disciplina == "FMCC II"),
    .groups = "drop"
  ) %>%
  filter(
    tem_fmcc_I,
    tem_fmcc_II
  )

cat(
  "\nAlunos com FMCC I + FMCC II:",
  nrow(alunos_fmcc_completo),
  "\n"
)

# =========================================================
# 65. TRAJETÓRIA FMCC I + FMCC II → CÁLCULO I
#     CURRÍCULO 2017
# =========================================================

# ---------------------------------------------------------
# 65.1. CÁLCULO I - CURRÍCULO 2017
# ---------------------------------------------------------

calculo_I_2017 <- enrollments %>%
  inner_join(
    amostra_comparavel %>%
      filter(
        Curriculo == "2017"
      ) %>%
      select(
        Matricula,
        Curriculo,
        Status,
        `Tipo de Evasao`,
        `Periodo de Evasao`
      ),
    by = c(
      "registration" = "Matricula"
    )
  ) %>%
  filter(
    subjectCode == codigo_calculo_I_2017
  ) %>%
  mutate(
    nota = if_else(
      str_trim(grade) == "-" | is.na(grade),
      NA_real_,
      parse_number(
        grade,
        locale = locale(decimal_mark = ",")
      )
    )
  )


# ---------------------------------------------------------
# 65.2. RESULTADO DE CÁLCULO I POR ALUNO
# ---------------------------------------------------------

resultado_calculo_2017 <- calculo_I_2017 %>%
  arrange(
    registration,
    term,
    classId
  ) %>%
  group_by(
    registration
  ) %>%
  summarise(
    tentativas_calculo = n(),
    
    teve_aprovacao = any(
      status == "Aprovado"
    ),
    
    teve_reprovacao = any(
      status %in% c(
        "Reprovado",
        "Reprovado por Falta"
      )
    ),
    
    teve_dispensa = any(
      status == "Dispensa"
    ),
    
    nota_maxima = ifelse(
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
  ) %>%
  mutate(
    resultado_calculo = case_when(
      teve_aprovacao ~ "Aprovado",
      teve_dispensa ~ "Dispensa",
      teve_reprovacao ~ "Sem aprovação",
      TRUE ~ "Outro"
    )
  )


# ---------------------------------------------------------
# 65.3. TRAJETÓRIA COMPLETA
#     FMCC I + FMCC II → CÁLCULO I
# ---------------------------------------------------------

trajetoria_fmcc_calculo <- alunos_fmcc_completo %>%
  left_join(
    resultado_calculo_2017,
    by = "registration"
  )


# ---------------------------------------------------------
# 65.4. RESULTADO DE CÁLCULO I
#     ENTRE OS ALUNOS QUE FIZERAM FMCC I + II
# ---------------------------------------------------------

cat("\n=========================================================\n")
cat("RESULTADO DE CÁLCULO I ENTRE ALUNOS COM FMCC I + FMCC II\n")
cat("=========================================================\n")

resultado_trajetoria <- trajetoria_fmcc_calculo %>%
  count(
    resultado_calculo
  ) %>%
  mutate(
    percentual = 100 * n / sum(n)
  )

print(
  resultado_trajetoria
)


# =========================================================
# 65.5. COBERTURA DE CÁLCULO I NA TRAJETÓRIA FMCC
# =========================================================

trajetoria_fmcc_calculo %>%
  mutate(
    cursou_calculo_I = !is.na(resultado_calculo)
  ) %>%
  count(
    cursou_calculo_I
  ) %>%
  mutate(
    percentual = 100 * n / sum(n)
  ) %>%
  print()


# =========================================================
# 65.6. TRAJETÓRIA FMCC → CÁLCULO I
# =========================================================

trajetoria_fmcc_calculo %>%
  mutate(
    cursou_calculo_I = !is.na(resultado_calculo)
  ) %>%
  count(
    cursou_calculo_I,
    resultado_calculo
  ) %>%
  print()

# Agora eu faria o cruzamento que realmente interessa

# =========================================================
# 66. RESULTADO POR ALUNO - FMCC
# =========================================================

resultado_fmcc_aluno <- fmcc %>%
  arrange(
    registration,
    disciplina,
    term,
    classId
  ) %>%
  group_by(
    registration,
    disciplina
  ) %>%
  summarise(
    tentativas = n(),
    
    teve_aprovacao = any(
      status == "Aprovado"
    ),
    
    teve_reprovacao = any(
      status %in% c(
        "Reprovado",
        "Reprovado por Falta"
      )
    ),
    
    teve_dispensa = any(
      status == "Dispensa"
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    resultado_fmcc = case_when(
      teve_aprovacao ~ "Aprovado",
      teve_dispensa ~ "Dispensa",
      teve_reprovacao ~ "Sem aprovação",
      TRUE ~ "Outro"
    )
  )

# =========================================================
# 67. TRAJETÓRIA FMCC I + FMCC II POR ALUNO
# =========================================================

trajetoria_fmcc <- resultado_fmcc_aluno %>%
  mutate(
    disciplina_codigo = case_when(
      disciplina == "FMCC I"  ~ "FMCC_I",
      disciplina == "FMCC II" ~ "FMCC_II",
      TRUE ~ NA_character_
    )
  ) %>%
  select(
    registration,
    disciplina_codigo,
    resultado_fmcc,
    tentativas
  ) %>%
  tidyr::pivot_wider(
    names_from = disciplina_codigo,
    values_from = c(
      resultado_fmcc,
      tentativas
    ),
    names_glue = "{.value}_{disciplina_codigo}"
  )

cat("\n=========================================================\n")
cat("COLUNAS DA TRAJETÓRIA FMCC\n")
cat("=========================================================\n")

print(names(trajetoria_fmcc))


# =========================================================
# 68. FMCC I + FMCC II → CÁLCULO I
# =========================================================

trajetoria_completa <- trajetoria_fmcc %>%
  inner_join(
    resultado_calculo_2017,
    by = "registration"
  )

cat("\n=========================================================\n")
cat("TRAJETÓRIA COMPLETA\n")
cat("=========================================================\n")

print(
  trajetoria_completa %>%
    select(
      registration,
      resultado_fmcc_FMCC_I,
      resultado_fmcc_FMCC_II,
      tentativas_FMCC_I,
      tentativas_FMCC_II,
      resultado_calculo,
      tentativas_calculo
    ) %>%
    head(20)
)

# =========================================================
# 69. TRAJETÓRIA FMCC I + FMCC II → CÁLCULO I
# =========================================================

trajetoria_completa %>%
  count(
    resultado_fmcc_FMCC_I,
    resultado_fmcc_FMCC_II,
    resultado_calculo
  ) %>%
  arrange(
    resultado_fmcc_FMCC_I,
    resultado_fmcc_FMCC_II,
    resultado_calculo
  ) %>%
  print(n = Inf)


# =========================================================
# 70. DESEMPENHO EM CÁLCULO I SEGUNDO A SITUAÇÃO EM FMCC II
# =========================================================

analise_fmcc_calculo <- trajetoria_completa %>%
  filter(
    !is.na(resultado_calculo)
  ) %>%
  mutate(
    desempenho_calculo = case_when(
      resultado_calculo %in% c(
        "Aprovado",
        "Dispensa"
      ) ~ "Sucesso",
      
      resultado_calculo == "Sem aprovação" ~
        "Sem sucesso",
      
      TRUE ~ "Outro"
    )
  )

analise_fmcc_calculo %>%
  count(
    resultado_fmcc_FMCC_II,
    desempenho_calculo
  ) %>%
  group_by(
    resultado_fmcc_FMCC_II
  ) %>%
  mutate(
    percentual = 100 * n / sum(n)
  ) %>%
  ungroup() %>%
  print(n = Inf)


analise_fmcc_calculo %>%
  count(
    resultado_fmcc_FMCC_I,
    desempenho_calculo
  ) %>%
  group_by(
    resultado_fmcc_FMCC_I
  ) %>%
  mutate(
    percentual = 100 * n / sum(n)
  ) %>%
  ungroup() %>%
  print(n = Inf)

# =========================================================
# 71. ASSOCIAÇÃO ENTRE FMCC II E CÁLCULO I
# =========================================================

tabela_fmccII_calculo <- analise_fmcc_calculo %>%
  filter(
    resultado_fmcc_FMCC_II %in% c(
      "Aprovado",
      "Dispensa",
      "Sem aprovação"
    ),
    desempenho_calculo %in% c(
      "Sucesso",
      "Sem sucesso"
    )
  ) %>%
  mutate(
    resultado_fmccII_binario = case_when(
      resultado_fmcc_FMCC_II %in% c(
        "Aprovado",
        "Dispensa"
      ) ~ "Sucesso",
      resultado_fmcc_FMCC_II == "Sem aprovação" ~
        "Sem sucesso"
    )
  ) %>%
  count(
    resultado_fmccII_binario,
    desempenho_calculo
  ) %>%
  tidyr::pivot_wider(
    names_from = desempenho_calculo,
    values_from = n,
    values_fill = 0
  )

print(tabela_fmccII_calculo)


matriz_fmccII <- tabela_fmccII_calculo %>%
  select(
    Sucesso,
    `Sem sucesso`
  ) %>%
  as.matrix()

rownames(matriz_fmccII) <-
  tabela_fmccII_calculo$resultado_fmccII_binario

print(matriz_fmccII)

teste_fmccII <- chisq.test(
  matriz_fmccII
)

print(teste_fmccII)


# =========================================================
# V DE CRAMÉR - FMCC II × CÁLCULO I
# =========================================================

n_total <- sum(matriz_fmccII)

v_fmccII <- sqrt(
  as.numeric(teste_fmccII$statistic) /
    (n_total * min(
      nrow(matriz_fmccII) - 1,
      ncol(matriz_fmccII) - 1
    ))
)

cat(
  "\nV de Cramér - FMCC II × Cálculo I:",
  v_fmccII,
  "\n"
)

# Agora eu faria o teste de FMCC I

# =========================================================
# 72. ASSOCIAÇÃO ENTRE FMCC I E CÁLCULO I
# =========================================================

tabela_fmccI_calculo <- analise_fmcc_calculo %>%
  filter(
    resultado_fmcc_FMCC_I %in% c(
      "Aprovado",
      "Dispensa",
      "Sem aprovação"
    ),
    desempenho_calculo %in% c(
      "Sucesso",
      "Sem sucesso"
    )
  ) %>%
  mutate(
    resultado_fmccI_binario = case_when(
      resultado_fmcc_FMCC_I %in% c(
        "Aprovado",
        "Dispensa"
      ) ~ "Sucesso",
      
      resultado_fmcc_FMCC_I == "Sem aprovação" ~
        "Sem sucesso"
    )
  ) %>%
  count(
    resultado_fmccI_binario,
    desempenho_calculo
  ) %>%
  tidyr::pivot_wider(
    names_from = desempenho_calculo,
    values_from = n,
    values_fill = 0
  )

print(tabela_fmccI_calculo)

#

matriz_fmccI <- tabela_fmccI_calculo %>%
  select(
    Sucesso,
    `Sem sucesso`
  ) %>%
  as.matrix()

rownames(matriz_fmccI) <-
  tabela_fmccI_calculo$resultado_fmccI_binario

print(matriz_fmccI)

teste_fmccI <- chisq.test(
  matriz_fmccI
)

print(teste_fmccI)


n_total <- sum(matriz_fmccI)

v_fmccI <- sqrt(
  as.numeric(teste_fmccI$statistic) /
    (
      n_total *
        min(
          nrow(matriz_fmccI) - 1,
          ncol(matriz_fmccI) - 1
        )
    )
)

cat(
  "\nV de Cramér - FMCC I × Cálculo I:",
  v_fmccI,
  "\n"
)

resultado_fmcc_aluno %>%
  filter(
    disciplina == "FMCC I"
  ) %>%
  count(
    resultado_fmcc
  )

trajetoria_completa %>%
  count(
    resultado_fmcc_FMCC_I,
    sort = TRUE
  )

# 

trajetoria_completa <- trajetoria_fmcc %>%
  inner_join(
    resultado_calculo_2017,
    by = "registration"
  )

trajetoria_completa <- trajetoria_fmcc %>%
  inner_join(
    resultado_calculo_2017,
    by = "registration"
  )

trajetoria_completa <- trajetoria_fmcc %>%
  left_join(
    resultado_calculo_2017,
    by = "registration"
  )

cat("\n=========================================================\n")
cat("TRAJETÓRIA FMCC I + FMCC II\n")
cat("=========================================================\n")

cat(
  "\nTotal de alunos:",
  nrow(trajetoria_completa),
  "\n"
)

trajetoria_completa %>%
  count(
    resultado_fmcc_FMCC_I
  ) %>%
  print()

trajetoria_completa %>%
  filter(
    !is.na(resultado_calculo)
  ) %>%
  count(
    resultado_fmcc_FMCC_I,
    resultado_calculo
  ) %>%
  print(n = Inf)

analise_fmccI_calculo <- trajetoria_completa %>%
  filter(
    !is.na(resultado_calculo),
    resultado_fmcc_FMCC_I %in% c(
      "Aprovado",
      "Dispensa",
      "Sem aprovação"
    ),
    resultado_calculo %in% c(
      "Aprovado",
      "Dispensa",
      "Sem aprovação"
    )
  ) %>%
  mutate(
    resultado_fmccI_binario = case_when(
      resultado_fmcc_FMCC_I %in% c(
        "Aprovado",
        "Dispensa"
      ) ~ "Sucesso",
      
      resultado_fmcc_FMCC_I == "Sem aprovação" ~
        "Sem sucesso"
    ),
    
    desempenho_calculo = case_when(
      resultado_calculo %in% c(
        "Aprovado",
        "Dispensa"
      ) ~ "Sucesso",
      
      resultado_calculo == "Sem aprovação" ~
        "Sem sucesso"
    )
  )

analise_fmccI_calculo %>%
  count(
    resultado_fmccI_binario,
    desempenho_calculo
  ) %>%
  group_by(
    resultado_fmccI_binario
  ) %>%
  mutate(
    percentual = 100 * n / sum(n)
  ) %>%
  ungroup() %>%
  print()


