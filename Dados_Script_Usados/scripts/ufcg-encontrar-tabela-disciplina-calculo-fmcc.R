# =========================================================
# BUSCA DAS DISCIPLINAS-CHAVE DA SEÇÃO 5.7.5
#
# Disciplinas principais:
#   - Cálculo I
#   - Cálculo II
#   - FMCC I
#   - FMCC II
#
# Objetivo:
# Identificar quais tabelas possuem essas disciplinas
# e verificar a cobertura dos 1.772 alunos da amostra.
# =========================================================

rm(list = ls())

library(readr)
library(readxl)
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
# 2. CARREGAR AMOSTRA
# =========================================================

amostra <- read_csv2(
  file.path(
    pasta_processados,
    "amostra_final_dissertacao.csv"
  ),
  show_col_types = FALSE
)

# =========================================================
# 3. PADRONIZAR MATRÍCULA DA AMOSTRA
# =========================================================

amostra <- amostra %>%
  mutate(
    MATRICULA_JOIN =
      str_trim(
        as.character(
          Matricula
        )
      )
  ) %>%
  distinct(
    MATRICULA_JOIN,
    .keep_all = TRUE
  )

cat("\n=========================================================\n")
cat("AMOSTRA FINAL\n")
cat("=========================================================\n")

cat(
  "\nRegistros:",
  nrow(amostra),
  "\n"
)

cat(
  "Matrículas únicas:",
  n_distinct(
    amostra$MATRICULA_JOIN
  ),
  "\n"
)


# =========================================================
# 4. NORMALIZAR TEXTO
# =========================================================

normalizar <- function(x) {
  
  x <- as.character(x)
  
  x <- iconv(
    x,
    from = "",
    to = "ASCII//TRANSLIT",
    sub = ""
  )
  
  x <- toupper(x)
  
  x <- str_squish(x)
  
  return(x)
}


# =========================================================
# 5. DEFINIR DISCIPLINAS-ALVO
# =========================================================

# IMPORTANTE:
# A busca foi propositalmente ampla.
# Depois analisaremos os nomes exatos encontrados.

padroes_disciplinas <- c(
  
  # -------------------------------------------------------
  # CÁLCULO I
  # -------------------------------------------------------
  
  "CALCULO I",
  "CALCULO 1",
  "CALCULO DIFERENCIAL E INTEGRAL I",
  "CALCULO DIFERENCIAL E INTEGRAL 1",
  "CALCULO DIF E INTEGRAL I",
  "CALCULO DIF E INTEGRAL 1",
  
  # -------------------------------------------------------
  # CÁLCULO II
  # -------------------------------------------------------
  
  "CALCULO II",
  "CALCULO 2",
  "CALCULO DIFERENCIAL E INTEGRAL II",
  "CALCULO DIFERENCIAL E INTEGRAL 2",
  "CALCULO DIF E INTEGRAL II",
  "CALCULO DIF E INTEGRAL 2",
  
  # -------------------------------------------------------
  # FMCC
  # -------------------------------------------------------
  
  "FMCC",
  "FMCC I",
  "FMCC 1",
  "FMCC II",
  "FMCC 2",
  
  # -------------------------------------------------------
  # POSSÍVEL NOME POR EXTENSO
  # -------------------------------------------------------
  
  "FUNDAMENTOS DE MATEMATICA",
  "MATEMATICA PARA CIENCIA DA COMPUTACAO",
  "FUNDAMENTOS DE MATEMATICA PARA CIENCIA DA COMPUTACAO"
)


# =========================================================
# 6. LISTAR ARQUIVOS
# =========================================================

arquivos <- list.files(
  pasta_dados,
  full.names = TRUE
)

arquivos <- arquivos[
  !grepl(
    "^~\\$",
    basename(arquivos)
  )
]

arquivos <- arquivos[
  tolower(
    tools::file_ext(arquivos)
  ) %in%
    c(
      "csv",
      "xlsx",
      "xls",
      "tsv"
    )
]

cat("\n=========================================================\n")
cat("TABELAS A SEREM ANALISADAS\n")
cat("=========================================================\n")

cat(
  "\nTotal:",
  length(arquivos),
  "\n"
)

print(
  basename(arquivos)
)


# =========================================================
# 7. FUNÇÃO PARA CARREGAR
# =========================================================

carregar <- function(arquivo) {
  
  ext <- tolower(
    tools::file_ext(
      arquivo
    )
  )
  
  tryCatch({
    
    if (ext == "csv") {
      
      dados <- read_delim(
        arquivo,
        delim = ";",
        show_col_types = FALSE,
        progress = FALSE
      )
      
      if (ncol(dados) == 1) {
        
        dados <- read_delim(
          arquivo,
          delim = ",",
          show_col_types = FALSE,
          progress = FALSE
        )
      }
      
    } else if (ext == "tsv") {
      
      dados <- read_tsv(
        arquivo,
        show_col_types = FALSE,
        progress = FALSE
      )
      
    } else {
      
      dados <- read_excel(
        arquivo
      )
    }
    
    dados
    
  }, error = function(e) {
    
    NULL
    
  })
}


# =========================================================
# 8. RESULTADOS
# =========================================================

resultado <- data.frame()

detalhes <- data.frame()

# =========================================================
# 9. ANALISAR CADA TABELA
# =========================================================

for (arquivo in arquivos) {
  
  nome_arquivo <- basename(
    arquivo
  )
  
  cat("\n\n")
  cat("=========================================================\n")
  cat("ANALISANDO:", nome_arquivo, "\n")
  cat("=========================================================\n")
  
  dados <- carregar(
    arquivo
  )
  
  if (is.null(dados)) {
    
    cat(
      "ERRO AO CARREGAR\n"
    )
    
    next
  }
  
  cat(
    "\nLinhas:",
    nrow(dados),
    "\n"
  )
  
  cat(
    "Colunas:",
    ncol(dados),
    "\n"
  )
  
  cat(
    "Colunas:",
    paste(
      names(dados),
      collapse = " | "
    ),
    "\n"
  )
  
  # -------------------------------------------------------
  # Padronizar matrículas, se existir
  # -------------------------------------------------------
  
  nomes_colunas <- names(dados)
  
  indice_matricula <- which(
    str_detect(
      normalizar(nomes_colunas),
      "MATRIC"
    )
  )
  
  tem_matricula <- length(
    indice_matricula
  ) > 0
  
  coluna_matricula <- NULL
  
  if (tem_matricula) {
    
    coluna_matricula <-
      nomes_colunas[
        indice_matricula[1]
      ]
  }
  
  
  # -------------------------------------------------------
  # Procurar disciplinas em TODAS as colunas
  # -------------------------------------------------------
  
  encontrou <- FALSE
  
  resultados_tabela <- data.frame()
  
  for (coluna in nomes_colunas) {
    
    valores <- dados[[coluna]]
    
    valores_texto <- normalizar(
      valores
    )
    
    # -----------------------------------------------------
    # Cálculo / FMCC
    # -----------------------------------------------------
    
    indices <- which(
      str_detect(
        valores_texto,
        "CALCULO|FMCC|FUNDAMENTOS DE MATEMATICA|MATEMATICA PARA CIENCIA DA COMPUTACAO"
      )
    )
    
    if (length(indices) == 0) {
      
      next
    }
    
    encontrou <- TRUE
    
    valores_encontrados <- unique(
      valores[
        indices
      ]
    )
    
    cat("\n>>> DISCIPLINAS ENCONTRADAS <<<\n")
    
    cat(
      "\nArquivo:",
      nome_arquivo,
      "\n"
    )
    
    cat(
      "Coluna:",
      coluna,
      "\n"
    )
    
    cat(
      "Ocorrências:",
      length(indices),
      "\n"
    )
    
    cat("\nValores:\n")
    
    print(
      valores_encontrados
    )
    
    # -----------------------------------------------------
    # Classificar cada ocorrência
    # -----------------------------------------------------
    
    for (
      valor in valores_encontrados
    ) {
      
      valor_norm <- normalizar(
        valor
      )
      
      disciplina_tipo <- case_when(
        
        str_detect(
          valor_norm,
          "FMCC.*I|FMCC.*1"
        ) ~
          "FMCC I",
        
        str_detect(
          valor_norm,
          "FMCC.*II|FMCC.*2"
        ) ~
          "FMCC II",
        
        str_detect(
          valor_norm,
          "CALCULO.*I|CALCULO.*1"
        ) ~
          "CALCULO I",
        
        str_detect(
          valor_norm,
          "CALCULO.*II|CALCULO.*2"
        ) ~
          "CALCULO II",
        
        TRUE ~
          "OUTRA_DISCIPLINA_MATEMATICA"
      )
      
      resultados_tabela <- bind_rows(
        resultados_tabela,
        data.frame(
          arquivo = nome_arquivo,
          coluna = coluna,
          disciplina_detectada =
            as.character(valor),
          tipo = disciplina_tipo,
          registros =
            sum(
              valores ==
                valor,
              na.rm = TRUE
            )
        )
      )
    }
  }
  
  
  # =======================================================
  # 10. SE ENCONTROU DISCIPLINAS
  # =======================================================
  
  if (encontrou) {
    
    cat("\n")
    cat("#########################################################\n")
    cat(">>> TABELA CANDIDATA <<<\n")
    cat("#########################################################\n")
    
    print(
      resultados_tabela
    )
    
    # -----------------------------------------------------
    # Verificar cobertura da amostra
    # -----------------------------------------------------
    
    cobertura_total <- NA
    cobertura_calculo <- NA
    cobertura_fmcc <- NA
    
    if (
      !is.null(
        coluna_matricula
      )
    ) {
      
      dados_join <- dados %>%
        mutate(
          MATRICULA_JOIN =
            str_trim(
              as.character(
                .data[[
                  coluna_matricula
                ]]
              )
            )
        )
      
      matriculas_encontradas <- amostra %>%
        filter(
          MATRICULA_JOIN %in%
            dados_join$MATRICULA_JOIN
        )
      
      cobertura_total <-
        n_distinct(
          matriculas_encontradas$
            MATRICULA_JOIN
        )
      
      # ---------------------------------------------------
      # Somente CÁLCULO
      # ---------------------------------------------------
      
      linhas_calculo <- dados_join %>%
        filter(
          str_detect(
            normalizar(
              do.call(
                paste,
                c(
                  dados_join[
                    ,
                    setdiff(
                      names(dados_join),
                      "MATRICULA_JOIN"
                    ),
                    drop = FALSE
                  ]
                )
              )
            ),
            "CALCULO"
          )
        )
      
      # ---------------------------------------------------
      # Somente FMCC
      # ---------------------------------------------------
      
      linhas_fmcc <- dados_join %>%
        filter(
          str_detect(
            normalizar(
              do.call(
                paste,
                c(
                  dados_join[
                    ,
                    setdiff(
                      names(dados_join),
                      "MATRICULA_JOIN"
                    ),
                    drop = FALSE
                )
              )
            )
          ),
          "FMCC|FUNDAMENTOS DE MATEMATICA"
        )
      )

cobertura_calculo <-
  n_distinct(
    linhas_calculo$MATRICULA_JOIN[
      linhas_calculo$MATRICULA_JOIN
      %in%
        amostra$MATRICULA_JOIN
    ]
  )

cobertura_fmcc <-
  n_distinct(
    linhas_fmcc$MATRICULA_JOIN[
      linhas_fmcc$MATRICULA_JOIN
      %in%
        amostra$MATRICULA_JOIN
    ]
  )

cat(
  "\nMatrículas da amostra encontradas na tabela:",
  cobertura_total,
  "\n"
)

cat(
  "Alunos da amostra com Cálculo:",
  cobertura_calculo,
  "\n"
)

cat(
  "Alunos da amostra com FMCC:",
  cobertura_fmcc,
  "\n"
)
    }
    
    # -----------------------------------------------------
    # Guardar resumo
    # -----------------------------------------------------
    
    resultado <- bind_rows(
      resultado,
      data.frame(
        arquivo = nome_arquivo,
        linhas = nrow(dados),
        colunas = ncol(dados),
        coluna_matricula =
          ifelse(
            is.null(
              coluna_matricula
            ),
            NA,
            coluna_matricula
          ),
        matriculas_amostra =
          cobertura_total,
        alunos_calculo =
          cobertura_calculo,
        alunos_fmcc =
          cobertura_fmcc
      )
    )
    
    detalhes <- bind_rows(
      detalhes,
      resultados_tabela
    )
  }
}


# =========================================================
# 11. RESULTADO FINAL
# =========================================================

cat("\n\n")
cat("=========================================================\n")
cat("RESULTADO FINAL - DISCIPLINAS-CHAVE\n")
cat("=========================================================\n")

print(
  resultado,
  row.names = FALSE
)


# =========================================================
# 12. DETALHAMENTO
# =========================================================

cat("\n\n")
cat("=========================================================\n")
cat("DETALHAMENTO DAS DISCIPLINAS ENCONTRADAS\n")
cat("=========================================================\n")

print(
  detalhes,
  row.names = FALSE
)


# =========================================================
# 13. SALVAR RESULTADOS
# =========================================================

write_csv2(
  resultado,
  file.path(
    pasta_dados,
    "resultado_disciplinas_chave_5_7_5.csv"
  )
)

write_csv2(
  detalhes,
  file.path(
    pasta_dados,
    "detalhes_disciplinas_chave_5_7_5.csv"
  )
)


# =========================================================
# 14. FINAL
# =========================================================

cat("\n\n")
cat("=========================================================\n")
cat("ANÁLISE CONCLUÍDA\n")
cat("=========================================================\n")

cat(
  "\nTabelas com disciplinas-chave:",
  nrow(resultado),
  "\n"
)

cat(
  "Arquivos gerados:\n"
)

cat(
  "resultado_disciplinas_chave_5_7_5.csv\n"
)

cat(
  "detalhes_disciplinas_chave_5_7_5.csv\n"
)

cat("\n=========================================================\n")
cat("FIM\n")
cat("=========================================================\n")

