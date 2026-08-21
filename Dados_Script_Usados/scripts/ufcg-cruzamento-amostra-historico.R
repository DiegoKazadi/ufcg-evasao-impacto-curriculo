#=========================================================
# DISSERTAÇÃO - ANÁLISE COMPARATIVA ENTRE DISCIPLINAS
# Seção 5.7.5
#
# Etapa 2 - Cruzamento da amostra final com o histórico
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
# 2. Carregar as bases
#=========================================================

amostra_final <- read_csv2(
  file.path(
    pasta_processados,
    "amostra_final_dissertacao.csv"
  ),
  show_col_types = FALSE
)

historico <- read_csv2(
  file.path(
    pasta_dados,
    "historico.csv"
  ),
  show_col_types = FALSE
)

#=========================================================
# 3. Verificar matrículas antes do JOIN
#=========================================================

cat("\n=========================================================\n")
cat("VERIFICAÇÃO ANTES DO JOIN\n")
cat("=========================================================\n")

cat(
  "\nAlunos distintos na amostra:",
  n_distinct(amostra_final$Matricula),
  "\n"
)

cat(
  "Alunos distintos no histórico:",
  n_distinct(historico$MATRICULA),
  "\n"
)

#=========================================================
# 4. Verificar cobertura da amostra no histórico
#=========================================================

matriculas_amostra <- amostra_final %>%
  distinct(Matricula)

matriculas_historico <- historico %>%
  distinct(MATRICULA)

alunos_com_historico <- matriculas_amostra %>%
  semi_join(
    matriculas_historico,
    by = c("Matricula" = "MATRICULA")
  )

alunos_sem_historico <- matriculas_amostra %>%
  anti_join(
    matriculas_historico,
    by = c("Matricula" = "MATRICULA")
  )

cat(
  "\nAlunos da amostra encontrados no histórico:",
  nrow(alunos_com_historico),
  "\n"
)

cat(
  "Alunos da amostra não encontrados no histórico:",
  nrow(alunos_sem_historico),
  "\n"
)

cat(
  "Percentual encontrado:",
  round(
    100 * nrow(alunos_com_historico) /
      nrow(matriculas_amostra),
    2
  ),
  "%\n"
)

#=========================================================
# 5. JOIN
#=========================================================

historico_amostra_final <- historico %>%
  inner_join(
    amostra_final,
    by = c("MATRICULA" = "Matricula")
  )

#=========================================================
# 6. Verificar resultado do JOIN
#=========================================================

cat("\n=========================================================\n")
cat("RESULTADO DO JOIN\n")
cat("=========================================================\n")

cat(
  "\nRegistros de histórico recuperados:",
  nrow(historico_amostra_final),
  "\n"
)

cat(
  "Alunos distintos recuperados:",
  n_distinct(
    historico_amostra_final$MATRICULA
  ),
  "\n"
)

#=========================================================
# 7. Distribuição dos registros por currículo
#=========================================================

cat("\n=========================================================\n")
cat("REGISTROS POR CURRÍCULO\n")
cat("=========================================================\n")

registros_por_curriculo <- historico_amostra_final %>%
  count(
    Curriculo,
    name = "registros"
  )

print(registros_por_curriculo)

#=========================================================
# 8. Alunos distintos por currículo
#=========================================================

cat("\n=========================================================\n")
cat("ALUNOS POR CURRÍCULO\n")
cat("=========================================================\n")

alunos_por_curriculo <- historico_amostra_final %>%
  group_by(
    Curriculo
  ) %>%
  summarise(
    alunos = n_distinct(MATRICULA),
    registros = n(),
    .groups = "drop"
  )

print(alunos_por_curriculo)

#=========================================================
# 9. Verificar valores de SITUACAO
#=========================================================

cat("\n=========================================================\n")
cat("SITUAÇÃO ACADÊMICA\n")
cat("=========================================================\n")

situacoes <- historico_amostra_final %>%
  count(
    SITUACAO,
    sort = TRUE
  )

print(situacoes)

#=========================================================
# 10. Salvar alunos sem histórico
#=========================================================

if (nrow(alunos_sem_historico) > 0) {
  
  write_csv2(
    alunos_sem_historico,
    file.path(
      pasta_processados,
      "alunos_amostra_sem_historico.csv"
    )
  )
  
}

#=========================================================
# 11. Salvar base principal
#=========================================================

write_csv2(
  historico_amostra_final,
  file.path(
    pasta_processados,
    "historico_amostra_final.csv"
  )
)

#=========================================================
# 12. Finalização
#=========================================================

cat("\n=========================================================\n")
cat("ETAPA 2 CONCLUÍDA\n")
cat("=========================================================\n")

cat(
  "\nBase criada:\n",
  file.path(
    pasta_processados,
    "historico_amostra_final.csv"
  ),
  "\n"
)


#=========================================================
# DIAGNÓSTICO DOS ALUNOS NÃO ENCONTRADOS NO HISTÓRICO
#=========================================================

# Matrículas da amostra
matriculas_amostra <- amostra_final %>%
  distinct(Matricula)

# Matrículas do histórico
matriculas_historico <- historico %>%
  distinct(MATRICULA)

# Alunos encontrados
alunos_com_historico <- matriculas_amostra %>%
  semi_join(
    matriculas_historico,
    by = c("Matricula" = "MATRICULA")
  )

# Alunos não encontrados
alunos_sem_historico <- matriculas_amostra %>%
  anti_join(
    matriculas_historico,
    by = c("Matricula" = "MATRICULA")
  )

cat("\n=========================================================\n")
cat("COBERTURA DO HISTÓRICO\n")
cat("=========================================================\n")

cat(
  "\nTotal da amostra:",
  nrow(matriculas_amostra),
  "\n"
)

cat(
  "Encontrados:",
  nrow(alunos_com_historico),
  "\n"
)

cat(
  "Não encontrados:",
  nrow(alunos_sem_historico),
  "\n"
)

#=========================================================
# 1. Distribuição dos encontrados por currículo
#=========================================================

cat("\n=========================================================\n")
cat("ENCONTRADOS POR CURRÍCULO\n")
cat("=========================================================\n")

amostra_final %>%
  semi_join(
    alunos_com_historico,
    by = "Matricula"
  ) %>%
  count(
    Curriculo,
    name = "alunos"
  ) %>%
  print()

#=========================================================
# 2. Distribuição dos NÃO encontrados por currículo
#=========================================================

cat("\n=========================================================\n")
cat("NÃO ENCONTRADOS POR CURRÍCULO\n")
cat("=========================================================\n")

amostra_final %>%
  semi_join(
    alunos_sem_historico,
    by = "Matricula"
  ) %>%
  count(
    Curriculo,
    name = "alunos"
  ) %>%
  print()

#=========================================================
# 3. NÃO encontrados por período de ingresso
#=========================================================

cat("\n=========================================================\n")
cat("NÃO ENCONTRADOS POR PERÍODO DE INGRESSO\n")
cat("=========================================================\n")

amostra_final %>%
  semi_join(
    alunos_sem_historico,
    by = "Matricula"
  ) %>%
  count(
    Curriculo,
    `Periodo de Ingresso`,
    name = "alunos"
  ) %>%
  arrange(
    Curriculo,
    `Periodo de Ingresso`
  ) %>%
  print()

#=========================================================
# 4. Mostrar alguns alunos não encontrados
#=========================================================

cat("\n=========================================================\n")
cat("EXEMPLOS DE ALUNOS NÃO ENCONTRADOS\n")
cat("=========================================================\n")

amostra_final %>%
  semi_join(
    alunos_sem_historico,
    by = "Matricula"
  ) %>%
  select(
    Matricula,
    `Periodo de Ingresso`,
    Curriculo,
    Status,
    `Tipo de Evasao`,
    `Periodo de Evasao`
  ) %>%
  head(20) %>%
  print()
