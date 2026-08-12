#=========================================================
# DISSERTAÇÃO - ANÁLISE COMPARATIVA ENTRE DISCIPLINAS
# Etapa 1 - Exploração e preparação das bases
# Etapa 1.1 - Validação da amostra final na tabela histórico
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

pasta_dados_processados <- file.path(
  projeto,
  "dados_processados"
)

pasta_resultados <- file.path(
  projeto,
  "resultados_analise_disciplinas"
)

# Criar pasta de resultados caso não exista
if (!dir.exists(pasta_resultados)) {
  dir.create(pasta_resultados, recursive = TRUE)
}

#=========================================================
# 2. Localização dos arquivos
#=========================================================

cat("\n=============================================\n")
cat("ARQUIVOS ENCONTRADOS\n")
cat("=============================================\n")

print(list.files(pasta_dados))
print(list.files(pasta_dados_processados))

#=========================================================
# 3. Leitura das bases
#=========================================================

#---------------------------------------------------------
# 3.1 Base original dos alunos
#---------------------------------------------------------

alunos_final <- read_delim(
  file.path(pasta_dados, "alunos-final.csv"),
  delim = ";",
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE,
  trim_ws = TRUE
)

#---------------------------------------------------------
# 3.2 Amostra final utilizada na dissertação
#---------------------------------------------------------

amostra_final <- read_delim(
  file.path(
    pasta_dados_processados,
    "amostra_final_dissertacao.csv"
  ),
  delim = ";",
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE,
  trim_ws = TRUE
)

#---------------------------------------------------------
# 3.3 Histórico acadêmico
#---------------------------------------------------------

tabela_historico <- read_delim(
  file.path(pasta_dados, "tabela_historico.csv"),
  delim = ";",
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE,
  trim_ws = TRUE
)

#=========================================================
# 4. Visualização das estruturas
#=========================================================

cat("\n=============================================\n")
cat("ESTRUTURA DAS BASES\n")
cat("=============================================\n")

cat("\n--- alunos_final ---\n")
print(names(alunos_final))

cat("\n--- amostra_final ---\n")
print(names(amostra_final))

cat("\n--- tabela_historico ---\n")
print(names(tabela_historico))

#=========================================================
# 5. Padronização dos nomes das colunas
#=========================================================

# Remover espaços extras dos nomes
names(alunos_final) <- trimws(names(alunos_final))
names(amostra_final) <- trimws(names(amostra_final))
names(tabela_historico) <- trimws(names(tabela_historico))

#=========================================================
# 6. Padronização da matrícula
#=========================================================

# Função para padronizar matrícula
padronizar_matricula <- function(x) {
  
  x <- as.character(x)
  
  x <- trimws(x)
  
  # Remover espaços
  x <- gsub("\\s+", "", x)
  
  # Remover .0 quando a matrícula foi lida como número
  x <- sub("\\.0$", "", x)
  
  return(x)
}

# Aplicar padronização
alunos_final <- alunos_final %>%
  mutate(
    MATRICULA = padronizar_matricula(Matrícula)
  )

amostra_final <- amostra_final %>%
  mutate(
    MATRICULA = padronizar_matricula(Matrícula)
  )

tabela_historico <- tabela_historico %>%
  mutate(
    MATRICULA = padronizar_matricula(MATRICULA)
  )

#=========================================================
# 7. Quantidade de alunos nas bases
#=========================================================

cat("\n=============================================\n")
cat("QUANTIDADE DE ALUNOS\n")
cat("=============================================\n")

cat(
  "\nAlunos na base alunos-final:",
  n_distinct(alunos_final$MATRICULA),
  "\n"
)

cat(
  "Alunos na amostra final:",
  n_distinct(amostra_final$MATRICULA),
  "\n"
)

cat(
  "Alunos distintos na tabela histórico:",
  n_distinct(tabela_historico$MATRICULA),
  "\n"
)

#=========================================================
# 8. Verificação:
#    alunos-final x amostra_final_dissertacao
#=========================================================

cat("\n=============================================\n")
cat("COMPARAÇÃO: ALUNOS-FINAL x AMOSTRA FINAL\n")
cat("=============================================\n")

# Alunos presentes em alunos-final e não na amostra final
alunos_excluidos <- alunos_final %>%
  anti_join(
    amostra_final %>% select(MATRICULA),
    by = "MATRICULA"
  )

# Alunos presentes na amostra final e não em alunos-final
alunos_novos_amostra <- amostra_final %>%
  anti_join(
    alunos_final %>% select(MATRICULA),
    by = "MATRICULA"
  )

cat(
  "\nAlunos presentes em alunos-final e ausentes da amostra final:",
  n_distinct(alunos_excluidos$MATRICULA),
  "\n"
)

cat(
  "Alunos presentes na amostra final e ausentes de alunos-final:",
  n_distinct(alunos_novos_amostra$MATRICULA),
  "\n"
)

# Salvar diferenças
write_csv(
  alunos_excluidos,
  file.path(
    pasta_resultados,
    "alunos_excluidos_amostra_final.csv"
  )
)

write_csv(
  alunos_novos_amostra,
  file.path(
    pasta_resultados,
    "alunos_novos_amostra_final.csv"
  )
)

#=========================================================
# 9. CRUZAMENTO PRINCIPAL
#    Amostra final x tabela histórico
#=========================================================

cat("\n=============================================\n")
cat("COBERTURA DA AMOSTRA FINAL NO HISTÓRICO\n")
cat("=============================================\n")

# Identificar alunos da amostra final que aparecem no histórico
amostra_com_historico <- amostra_final %>%
  select(
    MATRICULA,
    everything()
  ) %>%
  distinct(MATRICULA, .keep_all = TRUE) %>%
  left_join(
    tabela_historico %>%
      distinct(MATRICULA),
    by = "MATRICULA",
    relationship = "many-to-one"
  ) %>%
  mutate(
    possui_historico = !is.na(MATRICULA.y)
  )

# Como a coluna MATRICULA aparece duas vezes,
# reorganizar o resultado
amostra_com_historico <- amostra_com_historico %>%
  select(
    -MATRICULA.y
  ) %>%
  rename(
    MATRICULA = MATRICULA.x
  )

# Resumo
resumo_cobertura <- amostra_com_historico %>%
  summarise(
    alunos_amostra = n_distinct(MATRICULA),
    alunos_com_historico = n_distinct(
      MATRICULA[possui_historico]
    ),
    alunos_sem_historico = n_distinct(
      MATRICULA[!possui_historico]
    ),
    percentual_com_historico =
      100 * alunos_com_historico / alunos_amostra
  )

print(resumo_cobertura)

#=========================================================
# 10. Alunos da amostra SEM registro no histórico
#=========================================================

alunos_sem_historico <- amostra_com_historico %>%
  filter(
    !possui_historico
  )

cat(
  "\nAlunos da amostra final sem nenhum registro no histórico:",
  n_distinct(alunos_sem_historico$MATRICULA),
  "\n"
)

write_csv(
  alunos_sem_historico,
  file.path(
    pasta_resultados,
    "alunos_amostra_sem_historico.csv"
  )
)

#=========================================================
# 11. Quantidade de registros de histórico por aluno
#=========================================================

historico_por_aluno <- tabela_historico %>%
  filter(
    MATRICULA %in% amostra_final$MATRICULA
  ) %>%
  group_by(MATRICULA) %>%
  summarise(
    registros_historico = n(),
    disciplinas_distintas = n_distinct(DISCIPLINA),
    periodos_distintos = n_distinct(PERIODO),
    .groups = "drop"
  )

# Juntar à amostra
cobertura_detalhada <- amostra_final %>%
  select(
    MATRICULA,
    `Curriculo Entrada`,
    Status,
    `Tipo de Evasao`,
    `Periodo de Evasao`
  ) %>%
  distinct() %>%
  left_join(
    historico_por_aluno,
    by = "MATRICULA"
  ) %>%
  mutate(
    registros_historico = replace_na(
      registros_historico,
      0
    ),
    disciplinas_distintas = replace_na(
      disciplinas_distintas,
      0
    ),
    periodos_distintos = replace_na(
      periodos_distintos,
      0
    ),
    possui_historico =
      registros_historico > 0
  )

#=========================================================
# 12. Cobertura por currículo
#=========================================================

cobertura_por_curriculo <- cobertura_detalhada %>%
  group_by(
    `Curriculo Entrada`
  ) %>%
  summarise(
    alunos = n(),
    com_historico = sum(possui_historico),
    sem_historico = sum(!possui_historico),
    percentual_com_historico =
      100 * com_historico / alunos,
    .groups = "drop"
  )

cat("\n=============================================\n")
cat("COBERTURA POR CURRÍCULO\n")
cat("=============================================\n")

print(cobertura_por_curriculo)

#=========================================================
# 13. Verificar campos necessários do histórico
#=========================================================

cat("\n=============================================\n")
cat("COMPLETUDE DOS CAMPOS DO HISTÓRICO\n")
cat("=============================================\n")

historico_amostra <- tabela_historico %>%
  filter(
    MATRICULA %in% amostra_final$MATRICULA
  )

completude_historico <- tibble(
  variavel = c(
    "MATRICULA",
    "DISCIPLINA",
    "PERIODO",
    "NOTA",
    "SITUACAO",
    "ESTADO"
  ),
  registros = c(
    nrow(historico_amostra),
    nrow(historico_amostra),
    nrow(historico_amostra),
    nrow(historico_amostra),
    nrow(historico_amostra),
    nrow(historico_amostra)
  ),
  preenchidos = c(
    sum(!is.na(historico_amostra$MATRICULA) &
          historico_amostra$MATRICULA != ""),
    sum(!is.na(historico_amostra$DISCIPLINA) &
          historico_amostra$DISCIPLINA != ""),
    sum(!is.na(historico_amostra$PERIODO) &
          historico_amostra$PERIODO != ""),
    sum(!is.na(historico_amostra$NOTA) &
          historico_amostra$NOTA != ""),
    sum(!is.na(historico_amostra$SITUACAO) &
          historico_amostra$SITUACAO != ""),
    sum(!is.na(historico_amostra$ESTADO) &
          historico_amostra$ESTADO != "")
  )
) %>%
  mutate(
    percentual_preenchido =
      100 * preenchidos / registros
  )

print(completude_historico)

#=========================================================
# 14. Valores distintos de SITUACAO
#=========================================================

cat("\n=============================================\n")
cat("VALORES DE SITUACAO\n")
cat("=============================================\n")

situacoes <- historico_amostra %>%
  count(
    SITUACAO,
    sort = TRUE
  )

print(situacoes)

write_csv(
  situacoes,
  file.path(
    pasta_resultados,
    "valores_situacao_historico.csv"
  )
)

#=========================================================
# 15. Valores distintos de ESTADO
#=========================================================

cat("\n=============================================\n")
cat("VALORES DE ESTADO\n")
cat("=============================================\n")

estados <- historico_amostra %>%
  count(
    ESTADO,
    sort = TRUE
  )

print(estados)

write_csv(
  estados,
  file.path(
    pasta_resultados,
    "valores_estado_historico.csv"
  )
)

#=========================================================
# 16. Distribuição das notas
#=========================================================

cat("\n=============================================\n")
cat("INFORMAÇÕES SOBRE NOTAS\n")
cat("=============================================\n")

# Converter para numérico somente se possível
historico_amostra <- historico_amostra %>%
  mutate(
    NOTA_NUMERICA = suppressWarnings(
      as.numeric(
        gsub(",", ".", as.character(NOTA))
      )
    )
  )

resumo_notas <- historico_amostra %>%
  summarise(
    registros = n(),
    notas_preenchidas = sum(
      !is.na(NOTA_NUMERICA)
    ),
    notas_ausentes = sum(
      is.na(NOTA_NUMERICA)
    ),
    nota_minima = min(
      NOTA_NUMERICA,
      na.rm = TRUE
    ),
    nota_maxima = max(
      NOTA_NUMERICA,
      na.rm = TRUE
    ),
    media_nota = mean(
      NOTA_NUMERICA,
      na.rm = TRUE
    )
  )

print(resumo_notas)

#=========================================================
# 17. Mostrar alguns registros para inspeção
#=========================================================

cat("\n=============================================\n")
cat("AMOSTRA DE REGISTROS DO HISTÓRICO\n")
cat("=============================================\n")

historico_amostra %>%
  select(
    MATRICULA,
    DISCIPLINA,
    CREDITO,
    HORAS,
    PERIODO,
    ID_CLASS,
    NOTA,
    SITUACAO,
    ESTADO
  ) %>%
  slice_head(n = 20) %>%
  print()

#=========================================================
# 18. Salvar bases principais
#=========================================================

write_csv(
  resumo_cobertura,
  file.path(
    pasta_resultados,
    "resumo_cobertura_amostra_historico.csv"
  )
)

write_csv(
  cobertura_por_curriculo,
  file.path(
    pasta_resultados,
    "cobertura_amostra_historico_por_curriculo.csv"
  )
)

write_csv(
  cobertura_detalhada,
  file.path(
    pasta_resultados,
    "cobertura_detalhada_amostra_historico.csv"
  )
)

write_csv(
  completude_historico,
  file.path(
    pasta_resultados,
    "completude_campos_historico.csv"
  )
)

#=========================================================
# 19. Base de histórico somente dos alunos da amostra
#=========================================================

historico_amostra_final <- tabela_historico %>%
  filter(
    MATRICULA %in% amostra_final$MATRICULA
  ) %>%
  left_join(
    amostra_final %>%
      select(
        MATRICULA,
        `Periodo de Ingresso`,
        `Curriculo Entrada`,
        Status,
        `Tipo de Evasao`,
        `Periodo de Evasao`
      ) %>%
      distinct(),
    by = "MATRICULA"
  )

write_csv(
  historico_amostra_final,
  file.path(
    pasta_resultados,
    "historico_apenas_amostra_final.csv"
  )
)

#=========================================================
# 20. Finalização
#=========================================================

cat("\n=============================================\n")
cat("VALIDAÇÃO FINALIZADA\n")
cat("=============================================\n")

cat(
  "\nOs resultados foram salvos em:\n",
  pasta_resultados,
  "\n"
)

cat("\nArquivos principais gerados:\n")

print(
  list.files(
    pasta_resultados
  )
)

cat("\n=============================================\n")
cat("FIM DA ETAPA 1.1\n")
cat("=============================================\n")