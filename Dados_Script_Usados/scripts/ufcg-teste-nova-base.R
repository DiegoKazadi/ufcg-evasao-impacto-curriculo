#=========================================================
# DISSERTAÇÃO - ANÁLISE COMPARATIVA ENTRE DISCIPLINAS
# Teste da nova base de históricos
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
# 2. Nome da nova tabela
#=========================================================

arquivo_historicos <- "historicos_de_todos_os_alunos_ingressaram_partir_2002.csv"

# Se o nome do arquivo for diferente, altere somente acima.

#=========================================================
# 3. Carregar amostra final
#=========================================================

amostra <- read_csv2(
  file.path(
    pasta_processados,
    "amostra_final_dissertacao.csv"
  ),
  show_col_types = FALSE
)

#=========================================================
# 4. Carregar nova tabela de históricos
#=========================================================

historicos <- read_csv2(
  file.path(
    pasta_dados,
    arquivo_historicos
  ),
  show_col_types = FALSE
)

#=========================================================
# 5. Explorar AMOSTRA FINAL
#=========================================================

cat("\n=========================================================\n")
cat("AMOSTRA FINAL DA DISSERTAÇÃO\n")
cat("=========================================================\n")

cat("\nLinhas:", nrow(amostra), "\n")
cat("Colunas:", ncol(amostra), "\n")

cat("\nNomes das colunas:\n")
print(names(amostra))

cat("\nClasse da matrícula:\n")
print(class(amostra$Matricula))

cat("\nPrimeiras 5 matrículas:\n")
print(
  head(
    amostra$Matricula,
    5
  )
)

#=========================================================
# 6. Explorar NOVA TABELA
#=========================================================

cat("\n=========================================================\n")
cat("NOVA TABELA DE HISTÓRICOS\n")
cat("=========================================================\n")

cat("\nLinhas:", nrow(historicos), "\n")
cat("Colunas:", ncol(historicos), "\n")

cat("\nNomes das colunas:\n")
print(names(historicos))

cat("\nClasse da matrícula:\n")
print(class(historicos$MATRICULA))

cat("\nPrimeiras 5 linhas:\n")
print(
  head(
    historicos,
    5
  )
)

#=========================================================
# 7. Verificar comprimento das matrículas
#=========================================================

cat("\n=========================================================\n")
cat("FORMATO DAS MATRÍCULAS\n")
cat("=========================================================\n")

cat("\nAmostra final:\n")

print(
  table(
    nchar(
      as.character(
        amostra$Matricula
      )
    )
  )
)

cat("\nNova tabela:\n")

print(
  table(
    nchar(
      as.character(
        historicos$MATRICULA
      )
    )
  )
)

#=========================================================
# 8. Padronizar matrícula para comparação
#=========================================================

matriculas_amostra <- as.character(
  amostra$Matricula
)

matriculas_historicos <- as.character(
  historicos$MATRICULA
)

# Remover espaços
matriculas_amostra <- trimws(
  matriculas_amostra
)

matriculas_historicos <- trimws(
  matriculas_historicos
)

#=========================================================
# 9. Comparar matrículas
#=========================================================

matriculas_encontradas <- intersect(
  matriculas_amostra,
  matriculas_historicos
)

matriculas_nao_encontradas <- setdiff(
  matriculas_amostra,
  matriculas_historicos
)

cat("\n=========================================================\n")
cat("COBERTURA DA AMOSTRA NA NOVA TABELA\n")
cat("=========================================================\n")

cat(
  "\nMatrículas distintas na amostra:",
  length(
    unique(
      matriculas_amostra
    )
  ),
  "\n"
)

cat(
  "Matrículas distintas na nova tabela:",
  length(
    unique(
      matriculas_historicos
    )
  ),
  "\n"
)

cat(
  "Matrículas da amostra encontradas:",
  length(
    matriculas_encontradas
  ),
  "\n"
)

cat(
  "Matrículas da amostra não encontradas:",
  length(
    matriculas_nao_encontradas
  ),
  "\n"
)

cat(
  "Percentual da amostra encontrada:",
  round(
    100 *
      length(matriculas_encontradas) /
      length(unique(matriculas_amostra)),
    2
  ),
  "%\n"
)

#=========================================================
# 10. Mostrar exemplos de matrículas encontradas
#=========================================================

cat("\n=========================================================\n")
cat("EXEMPLOS DE MATRÍCULAS ENCONTRADAS\n")
cat("=========================================================\n")

print(
  head(
    matriculas_encontradas,
    20
  )
)

#=========================================================
# 11. Testar algumas matrículas conhecidas
#=========================================================

matriculas_teste <- c(
  "111110060",
  "111110061",
  "111110063",
  "111110064",
  "111110066"
)

cat("\n=========================================================\n")
cat("TESTE DE MATRÍCULAS ESPECÍFICAS\n")
cat("=========================================================\n")

for (mat in matriculas_teste) {
  
  registros <- historicos %>%
    filter(
      as.character(MATRICULA) == mat
    )
  
  cat(
    "\nMatrícula:",
    mat,
    "| Registros encontrados:",
    nrow(registros),
    "\n"
  )
  
}

#=========================================================
# 12. Verificar variáveis necessárias
#=========================================================

cat("\n=========================================================\n")
cat("VARIÁVEIS NECESSÁRIAS PARA A ANÁLISE\n")
cat("=========================================================\n")

variaveis_necessarias <- c(
  "MATRICULA",
  "DISCIPLINA",
  "MEDIA",
  "SITUACAO"
)

for (variavel in variaveis_necessarias) {
  
  if (variavel %in% names(historicos)) {
    
    cat(
      "✓",
      variavel,
      "-> encontrada\n"
    )
    
  } else {
    
    cat(
      "✗",
      variavel,
      "-> NÃO encontrada\n"
    )
    
  }
  
}

#=========================================================
# 13. Valores de SITUACAO
#=========================================================

if ("SITUACAO" %in% names(historicos)) {
  
  cat("\n=========================================================\n")
  cat("VALORES DE SITUACAO\n")
  cat("=========================================================\n")
  
  print(
    historicos %>%
      count(
        SITUACAO,
        sort = TRUE
      )
  )
  
}

#=========================================================
# 14. Valores de DISCIPLINA
#=========================================================

if ("DISCIPLINA" %in% names(historicos)) {
  
  cat("\n=========================================================\n")
  cat("ALGUMAS DISCIPLINAS DA NOVA TABELA\n")
  cat("=========================================================\n")
  
  print(
    historicos %>%
      count(
        DISCIPLINA,
        sort = TRUE
      ) %>%
      head(30)
  )
  
}

#=========================================================
# 15. Procurar Cálculo I
#=========================================================

if ("DISCIPLINA" %in% names(historicos)) {
  
  cat("\n=========================================================\n")
  cat("BUSCA POR CÁLCULO I\n")
  cat("=========================================================\n")
  
  calculo <- historicos %>%
    filter(
      grepl(
        "CÁLCULO.*I|CALCULO.*I",
        DISCIPLINA,
        ignore.case = TRUE
      )
    )
  
  cat(
    "\nRegistros encontrados:",
    nrow(calculo),
    "\n"
  )
  
  print(
    calculo %>%
      select(
        everything()
      ) %>%
      head(10)
  )
  
}

#=========================================================
# 16. Procurar FMCC I
#=========================================================

if ("DISCIPLINA" %in% names(historicos)) {
  
  cat("\n=========================================================\n")
  cat("BUSCA POR FMCC I\n")
  cat("=========================================================\n")
  
  fmcc <- historicos %>%
    filter(
      grepl(
        "FMCC|FUNDAMENTOS DE MATEM",
        DISCIPLINA,
        ignore.case = TRUE
      )
    )
  
  cat(
    "\nRegistros encontrados:",
    nrow(fmcc),
    "\n"
  )
  
  print(
    fmcc %>%
      select(
        everything()
      ) %>%
      head(10)
  )
  
}

#=========================================================
# FIM
#=========================================================

cat("\n=========================================================\n")
cat("TESTE CONCLUÍDO\n")
cat("=========================================================\n")