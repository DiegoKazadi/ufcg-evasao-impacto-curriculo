# =========================================================
# INVESTIGAÇÃO DAS 5 MATRÍCULAS AUSENTES
# E VALIDAÇÃO DA AMOSTRA
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
# 2. CARREGAR AMOSTRA E TABELAS
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

alunos <- read_csv2(
  file.path(
    pasta_dados,
    "alunos.csv"
  ),
  show_col_types = FALSE
)

alunos_final <- read_csv2(
  file.path(
    pasta_dados,
    "alunos-final.csv"
  ),
  show_col_types = FALSE
)

# =========================================================
# 3. PADRONIZAR MATRÍCULAS
# =========================================================

amostra <- amostra %>%
  mutate(
    MATRICULA = as.character(Matricula)
  )

matriculas <- matriculas %>%
  mutate(
    MATRICULA = as.character(MATRICULA)
  )

alunos <- alunos %>%
  mutate(
    MATRICULA = as.character(MATRICULA)
  )

alunos_final <- alunos_final %>%
  mutate(
    MATRICULA = as.character(Matricula)
  )

# =========================================================
# 4. CINCO AUSENTES
# =========================================================

ausentes <- c(
  "113210072",
  "113210439",
  "120210014",
  "121110389",
  "121210808"
)

cat("\n=========================================================\n")
cat("5 MATRÍCULAS AUSENTES\n")
cat("=========================================================\n")

print(ausentes)

# =========================================================
# 5. PROCURAR NAS TABELAS
# =========================================================

cat("\n=========================================================\n")
cat("PROCURANDO NAS TABELAS\n")
cat("=========================================================\n")

for (mat in ausentes) {
  
  cat("\n\n---------------------------------------------\n")
  cat("MATRÍCULA:", mat, "\n")
  cat("---------------------------------------------\n")
  
  cat("\nmatriculas.csv:\n")
  
  print(
    matriculas %>%
      filter(
        MATRICULA == mat
      )
  )
  
  cat("\nalunos.csv:\n")
  
  print(
    alunos %>%
      filter(
        MATRICULA == mat
      )
  )
  
  cat("\nalunos-final.csv:\n")
  
  print(
    alunos_final %>%
      filter(
        MATRICULA == mat
      )
  )
}

# =========================================================
# 6. VERIFICAR DUPLICIDADE NA AMOSTRA
# =========================================================

cat("\n=========================================================\n")
cat("DUPLICIDADES NA AMOSTRA\n")
cat("=========================================================\n")

duplicadas <- amostra %>%
  count(
    MATRICULA,
    sort = TRUE
  ) %>%
  filter(
    n > 1
  )

cat(
  "\nQuantidade de matrículas duplicadas:",
  nrow(duplicadas),
  "\n"
)

if (nrow(duplicadas) > 0) {
  print(duplicadas)
}

# =========================================================
# 7. COMPARAR QUANTIDADES
# =========================================================

cat("\n=========================================================\n")
cat("QUANTIDADES\n")
cat("=========================================================\n")

cat(
  "\nLinhas da amostra:",
  nrow(amostra),
  "\n"
)

cat(
  "Matrículas únicas da amostra:",
  n_distinct(
    amostra$MATRICULA
  ),
  "\n"
)

cat(
  "Matrículas únicas em matriculas.csv:",
  n_distinct(
    matriculas$MATRICULA
  ),
  "\n"
)

cat(
  "Matrículas únicas em alunos.csv:",
  n_distinct(
    alunos$MATRICULA
  ),
  "\n"
)

cat(
  "Matrículas únicas em alunos-final.csv:",
  n_distinct(
    alunos_final$MATRICULA
  ),
  "\n"
)

# =========================================================
# 8. COMPARAR COBERTURA
# =========================================================

cat("\n=========================================================\n")
cat("COBERTURA DA AMOSTRA NAS TABELAS\n")
cat("=========================================================\n")

cat(
  "\nmatriculas.csv:",
  sum(
    amostra$MATRICULA %in%
      matriculas$MATRICULA
  ),
  "\n"
)

cat(
  "alunos.csv:",
  sum(
    amostra$MATRICULA %in%
      alunos$MATRICULA
  ),
  "\n"
)

cat(
  "alunos-final.csv:",
  sum(
    amostra$MATRICULA %in%
      alunos_final$MATRICULA
  ),
  "\n"
)

# =========================================================
# 9. VERIFICAR PADRÃO DAS MATRÍCULAS
# =========================================================

cat("\n=========================================================\n")
cat("PADRÃO DAS MATRÍCULAS DA AMOSTRA\n")
cat("=========================================================\n")

print(
  amostra %>%
    mutate(
      prefixo = str_sub(
        MATRICULA,
        1,
        4
      )
    ) %>%
    count(
      prefixo,
      sort = TRUE
    )
)

# =========================================================
# 10. RELAÇÃO MATRÍCULA × PERÍODO DE INGRESSO
# =========================================================

cat("\n=========================================================\n")
cat("MATRÍCULAS AUSENTES × DADOS DA AMOSTRA\n")
cat("=========================================================\n")

print(
  amostra %>%
    filter(
      MATRICULA %in% ausentes
    )
)

cat("\n=========================================================\n")
cat("FIM DA INVESTIGAÇÃO\n")
cat("=========================================================\n")
