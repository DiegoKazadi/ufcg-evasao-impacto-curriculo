#=========================================================
# DISSERTAÇÃO - ANÁLISE COMPARATIVA ENTRE DISCIPLINAS
# Diagnóstico das variáveis de matrícula
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
# 2. Carregar amostra final
#=========================================================

amostra_final <- read_csv2(
  file.path(
    pasta_processados,
    "amostra_final_dissertacao.csv"
  ),
  show_col_types = FALSE
)

#=========================================================
# 3. Explorar matrícula da amostra final
#=========================================================

cat("\n=========================================================\n")
cat("AMOSTRA FINAL - MATRÍCULA\n")
cat("=========================================================\n")

cat("\nColunas da amostra:\n")
print(names(amostra_final))

cat("\nClasse da variável Matrícula:\n")
print(class(amostra_final$Matricula))

cat("\nPrimeiras 10 matrículas:\n")
print(
  head(
    amostra_final$Matricula,
    10
  )
)

cat(
  "\nQuantidade de matrículas distintas:",
  n_distinct(amostra_final$Matricula),
  "\n"
)

#=========================================================
# 4. Listar arquivos existentes na pasta dados
#=========================================================

cat("\n=========================================================\n")
cat("ARQUIVOS NA PASTA DADOS\n")
cat("=========================================================\n")

arquivos <- list.files(
  pasta_dados,
  full.names = TRUE
)

print(
  basename(arquivos)
)

#=========================================================
# 5. Identificar tabelas que possuem coluna de matrícula
#=========================================================

cat("\n=========================================================\n")
cat("PROCURANDO COLUNAS DE MATRÍCULA\n")
cat("=========================================================\n")

resultado_matricula <- data.frame()

for (arquivo in arquivos) {
  
  # Considerar apenas arquivos CSV
  if (!grepl("\\.csv$", arquivo, ignore.case = TRUE)) {
    next
  }
  
  cat(
    "\nAnalisando:",
    basename(arquivo),
    "\n"
  )
  
  # Tentar leitura
  tabela <- tryCatch(
    
    read_csv2(
      arquivo,
      show_col_types = FALSE,
      n_max = 5
    ),
    
    error = function(e) {
      cat(
        "ERRO ao ler:",
        basename(arquivo),
        "\n"
      )
      return(NULL)
    }
  )
  
  if (is.null(tabela)) {
    next
  }
  
  nomes_colunas <- names(tabela)
  
  # Procurar nomes relacionados a matrícula
  colunas_matricula <- nomes_colunas[
    grepl(
      "matr",
      nomes_colunas,
      ignore.case = TRUE
    )
  ]
  
  if (length(colunas_matricula) > 0) {
    
    cat(
      "Coluna(s) encontrada(s):",
      paste(
        colunas_matricula,
        collapse = ", "
      ),
      "\n"
    )
    
    resultado_matricula <- bind_rows(
      resultado_matricula,
      data.frame(
        arquivo = basename(arquivo),
        coluna_matricula = paste(
          colunas_matricula,
          collapse = ", "
        ),
        stringsAsFactors = FALSE
      )
    )
    
  } else {
    
    cat(
      "Nenhuma coluna de matrícula encontrada.\n"
    )
    
  }
}

#=========================================================
# 6. Resumo das tabelas encontradas
#=========================================================

cat("\n=========================================================\n")
cat("TABELAS COM COLUNA DE MATRÍCULA\n")
cat("=========================================================\n")

if (nrow(resultado_matricula) > 0) {
  
  print(resultado_matricula)
  
} else {
  
  cat(
    "\nNenhuma tabela com coluna contendo 'matr' foi encontrada.\n"
  )
  
}

#=========================================================
# 7. Testar compatibilidade das matrículas
#=========================================================

cat("\n=========================================================\n")
cat("TESTE DE COMPATIBILIDADE DAS MATRÍCULAS\n")
cat("=========================================================\n")

#---------------------------------------------------------
# Função para padronizar matrícula
#---------------------------------------------------------

padronizar_matricula <- function(x) {
  
  x <- as.character(x)
  
  x <- trimws(x)
  
  x <- gsub(
    "\\s+",
    "",
    x
  )
  
  # Remover .0 quando número foi convertido para texto
  x <- sub(
    "\\.0$",
    "",
    x
  )
  
  return(x)
}

# Matrículas da amostra
matriculas_amostra <- padronizar_matricula(
  amostra_final$Matricula
)

cat(
  "\nExemplos padronizados da amostra:\n"
)

print(
  head(
    matriculas_amostra,
    10
  )
)

#=========================================================
# 8. Comparar cada tabela encontrada
#=========================================================

for (i in seq_len(nrow(resultado_matricula))) {
  
  arquivo <- resultado_matricula$arquivo[i]
  
  coluna <- resultado_matricula$coluna_matricula[i]
  
  caminho <- file.path(
    pasta_dados,
    arquivo
  )
  
  cat("\n---------------------------------------------------------\n")
  cat(
    "Tabela:",
    arquivo,
    "\n"
  )
  cat(
    "Coluna:",
    coluna,
    "\n"
  )
  cat("---------------------------------------------------------\n")
  
  tabela <- tryCatch(
    
    read_csv2(
      caminho,
      show_col_types = FALSE
    ),
    
    error = function(e) {
      return(NULL)
    }
  )
  
  if (is.null(tabela)) {
    next
  }
  
  # Caso exista apenas uma coluna encontrada
  coluna_real <- strsplit(
    coluna,
    ", "
  )[[1]][1]
  
  matriculas_tabela <- padronizar_matricula(
    tabela[[coluna_real]]
  )
  
  # Comparação
  intersecao <- intersect(
    matriculas_amostra,
    matriculas_tabela
  )
  
  cat(
    "\nMatrículas distintas na tabela:",
    n_distinct(matriculas_tabela),
    "\n"
  )
  
  cat(
    "Matrículas da amostra encontradas:",
    length(intersecao),
    "\n"
  )
  
  cat(
    "Percentual da amostra encontrada:",
    round(
      100 * length(intersecao) /
        length(unique(matriculas_amostra)),
      2
    ),
    "%\n"
  )
  
  cat(
    "\nExemplos de matrículas coincidentes:\n"
  )
  
  print(
    head(
      intersecao,
      10
    )
  )
  
}

#=========================================================
# FIM
#=========================================================

cat("\n=========================================================\n")
cat("DIAGNÓSTICO CONCLUÍDO\n")
cat("=========================================================\n")










#=========================================================
# DIAGNÓSTICO DAS MATRÍCULAS
# Comparação entre amostra final e históricos
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
# 2. Carregar AMOSTRA FINAL
#=========================================================

amostra <- read_csv2(
  file.path(
    pasta_processados,
    "amostra_final_dissertacao.csv"
  ),
  show_col_types = FALSE
)

#=========================================================
# 3. Carregar HISTORICO
#=========================================================

historico <- read_csv2(
  file.path(
    pasta_dados,
    "historico.csv"
  ),
  show_col_types = FALSE
)

#=========================================================
# 4. Carregar TABELA HISTORICO
#=========================================================

tabela_historico <- read_csv2(
  file.path(
    pasta_dados,
    "tabela_historico.csv"
  ),
  show_col_types = FALSE
)

#=========================================================
# 5. AMOSTRA FINAL
#=========================================================

cat("\n=========================================================\n")
cat("AMOSTRA FINAL\n")
cat("=========================================================\n")

cat("\nClasse da Matrícula:\n")
print(class(amostra$Matricula))

cat("\nPrimeiras 20 matrículas:\n")
print(head(amostra$Matricula, 20))

cat("\nComprimento das matrículas:\n")
print(
  table(
    nchar(
      as.character(amostra$Matricula)
    )
  )
)

#=========================================================
# 6. HISTORICO
#=========================================================

cat("\n=========================================================\n")
cat("HISTORICO\n")
cat("=========================================================\n")

cat("\nClasse da MATRICULA:\n")
print(class(historico$MATRICULA))

cat("\nPrimeiras 20 matrículas:\n")
print(head(historico$MATRICULA, 20))

cat("\nComprimento das matrículas:\n")
print(
  table(
    nchar(
      as.character(historico$MATRICULA)
    )
  )
)

#=========================================================
# 7. TABELA HISTORICO
#=========================================================

cat("\n=========================================================\n")
cat("TABELA HISTORICO\n")
cat("=========================================================\n")

cat("\nClasse da MATRICULA:\n")
print(class(tabela_historico$MATRICULA))

cat("\nPrimeiras 20 matrículas:\n")
print(
  head(
    tabela_historico$MATRICULA,
    20
  )
)

cat("\nComprimento das matrículas:\n")
print(
  table(
    nchar(
      as.character(
        tabela_historico$MATRICULA
      )
    )
  )
)

#=========================================================
# 8. MATRÍCULAS ESPECÍFICAS DA AMOSTRA
#=========================================================

matriculas_teste <- c(
  "111110060",
  "111110061",
  "111110063",
  "111110064",
  "111110066"
)

#=========================================================
# 9. Procurar matrículas no HISTORICO
#=========================================================

cat("\n=========================================================\n")
cat("TESTE - HISTORICO\n")
cat("=========================================================\n")

for (mat in matriculas_teste) {
  
  resultado <- historico %>%
    filter(
      as.character(MATRICULA) == mat
    )
  
  cat(
    "\nMatrícula:",
    mat,
    " | Registros encontrados:",
    nrow(resultado),
    "\n"
  )
  
}

#=========================================================
# 10. Procurar matrículas na TABELA HISTORICO
#=========================================================

cat("\n=========================================================\n")
cat("TESTE - TABELA HISTORICO\n")
cat("=========================================================\n")

for (mat in matriculas_teste) {
  
  resultado <- tabela_historico %>%
    filter(
      as.character(MATRICULA) == mat
    )
  
  cat(
    "\nMatrícula:",
    mat,
    " | Registros encontrados:",
    nrow(resultado),
    "\n"
  )
  
}

#=========================================================
# 11. Verificar espaços nas matrículas
#=========================================================

cat("\n=========================================================\n")
cat("VERIFICAÇÃO DE ESPAÇOS\n")
cat("=========================================================\n")

cat(
  "\nHISTORICO - matrículas com espaços:",
  sum(
    grepl(
      "\\s",
      as.character(historico$MATRICULA)
    ),
    na.rm = TRUE
  ),
  "\n"
)

cat(
  "TABELA HISTORICO - matrículas com espaços:",
  sum(
    grepl(
      "\\s",
      as.character(tabela_historico$MATRICULA)
    ),
    na.rm = TRUE
  ),
  "\n"
)

#=========================================================
# 12. Verificar NAs
#=========================================================

cat("\n=========================================================\n")
cat("VALORES AUSENTES\n")
cat("=========================================================\n")

cat(
  "\nNA na matrícula - amostra:",
  sum(is.na(amostra$Matricula)),
  "\n"
)

cat(
  "NA na matrícula - historico:",
  sum(is.na(historico$MATRICULA)),
  "\n"
)

cat(
  "NA na matrícula - tabela_historico:",
  sum(is.na(tabela_historico$MATRICULA)),
  "\n"
)

#=========================================================
# FIM
#=========================================================

cat("\n=========================================================\n")
cat("DIAGNÓSTICO CONCLUÍDO\n")
cat("=========================================================\n")

