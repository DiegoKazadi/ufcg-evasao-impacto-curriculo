# =========================================================
# INVENTÁRIO DE DISCIPLINAS NAS TABELAS CANDIDATAS
# SEÇÃO 5.7.5 - ANÁLISE COMPARATIVA DAS DISCIPLINAS
# =========================================================

rm(list = ls())

library(readr)
library(readxl)
library(dplyr)
library(stringr)
library(purrr)

options(scipen = 999)

# =========================================================
# 1. DIRETÓRIOS
# =========================================================

projeto <- "C:/Users/Big Data/Documents/Master UFCG/Semestre 2026.1/ufcg-evasao-impacto-curriculo/Dados_Script_Usados"

pasta_dados <- file.path(
  projeto,
  "dados"
)

# =========================================================
# 2. ARQUIVOS QUE SERÃO INVESTIGADOS
# =========================================================

arquivos <- c(
  
  "historicos de todos os alunos que ingressaram a partir de 2002.tsv",
  
  "historicos_todos_alunos_ingressaram_2002.xlsx",
  
  "alunosUFCG.csv",
  
  "todosAlunosTudo.csv",
  
  "historico.csv"
  
)

# =========================================================
# 3. FUNÇÃO PARA CARREGAR CADA ARQUIVO
# =========================================================

carregar_arquivo <- function(nome_arquivo) {
  
  caminho <- file.path(
    pasta_dados,
    nome_arquivo
  )
  
  extensao <- tolower(
    tools::file_ext(nome_arquivo)
  )
  
  cat("\n\n")
  cat("=========================================================\n")
  cat("CARREGANDO:", nome_arquivo, "\n")
  cat("=========================================================\n")
  
  dados <- tryCatch({
    
    if (extensao == "csv") {
      
      # Tenta primeiro ;
      dados <- read_delim(
        caminho,
        delim = ";",
        show_col_types = FALSE,
        progress = FALSE
      )
      
      # Se resultar em apenas uma coluna,
      # tenta novamente com ,
      if (ncol(dados) == 1) {
        
        dados <- read_delim(
          caminho,
          delim = ",",
          show_col_types = FALSE,
          progress = FALSE
        )
      }
      
    } else if (extensao == "tsv") {
      
      dados <- read_tsv(
        caminho,
        show_col_types = FALSE,
        progress = FALSE
      )
      
    } else if (extensao %in% c("xlsx", "xls")) {
      
      dados <- read_excel(
        caminho
      )
      
    } else {
      
      stop(
        "Formato não suportado"
      )
    }
    
    dados
    
  }, error = function(e) {
    
    cat(
      "\nERRO:",
      conditionMessage(e),
      "\n"
    )
    
    NULL
  })
  
  return(dados)
}


# =========================================================
# 4. FUNÇÃO PARA NORMALIZAR NOMES DAS COLUNAS
# =========================================================

normalizar_nome <- function(x) {
  
  x %>%
    str_to_upper() %>%
    iconv(
      from = "UTF-8",
      to = "ASCII//TRANSLIT"
    ) %>%
    str_replace_all(
      "[^A-Z0-9]",
      "_"
    ) %>%
    str_replace_all(
      "_+",
      "_"
    ) %>%
    str_replace_all(
      "^_|_$",
      ""
    )
}


# =========================================================
# 5. FUNÇÃO PARA IDENTIFICAR POSSÍVEIS COLUNAS
#    DE DISCIPLINA
# =========================================================

identificar_colunas_disciplina <- function(nomes) {
  
  nomes_originais <- nomes
  
  nomes_normalizados <- normalizar_nome(
    nomes
  )
  
  tibble(
    coluna_original = nomes_originais,
    
    coluna_normalizada =
      nomes_normalizados,
    
    possivel_disciplina =
      str_detect(
        nomes_normalizados,
        "DISCIPLIN|NOME|COMPONENTE|MATERIA|CADEIRA|DESCRICAO"
      ),
    
    possivel_codigo =
      str_detect(
        nomes_normalizados,
        "COD.*DISCIPLIN|DISCIPLIN.*COD|CODIGO|CODE"
      ),
    
    possivel_matricula =
      str_detect(
        nomes_normalizados,
        "MATRIC|ALUNO|DISCENTE|ESTUDANTE|RA"
      ),
    
    possivel_periodo =
      str_detect(
        nomes_normalizados,
        "PERIOD|SEMESTRE|TERMO"
      ),
    
    possivel_situacao =
      str_detect(
        nomes_normalizados,
        "SITUAC|STATUS|ESTATUS|RESULTADO|ESTADO"
      ),
    
    possivel_nota =
      str_detect(
        nomes_normalizados,
        "NOTA|MEDIA|GRAU|CONCEITO"
      )
  )
}


# =========================================================
# 6. FUNÇÃO PARA PROCURAR CÁLCULO / FMCC
# =========================================================

procurar_disciplinas <- function(
    dados,
    nome_tabela,
    colunas_disciplina
) {
  
  resultados <- list()
  
  for (coluna in colunas_disciplina) {
    
    valores <- dados[[coluna]]
    
    valores_texto <- as.character(
      valores
    )
    
    valores_normalizados <- valores_texto %>%
      str_to_upper() %>%
      iconv(
        from = "UTF-8",
        to = "ASCII//TRANSLIT"
      )
    
    # -----------------------------------------------------
    # CÁLCULO
    # -----------------------------------------------------
    
    indices_calculo <- which(
      str_detect(
        valores_normalizados,
        "CALC"
      )
    )
    
    if (length(indices_calculo) > 0) {
      
      resultados[[length(resultados) + 1]] <-
        tibble(
          tabela = nome_tabela,
          coluna = coluna,
          tipo_busca = "CALCULO",
          valor_original =
            valores_texto[
              indices_calculo
            ]
        )
    }
    
    # -----------------------------------------------------
    # FMCC
    # -----------------------------------------------------
    
    indices_fmcc <- which(
      str_detect(
        valores_normalizados,
        "FMCC"
      )
    )
    
    if (length(indices_fmcc) > 0) {
      
      resultados[[length(resultados) + 1]] <-
        tibble(
          tabela = nome_tabela,
          coluna = coluna,
          tipo_busca = "FMCC",
          valor_original =
            valores_texto[
              indices_fmcc
            ]
        )
    }
  }
  
  if (length(resultados) == 0) {
    
    return(
      tibble(
        tabela = character(),
        coluna = character(),
        tipo_busca = character(),
        valor_original = character()
      )
    )
    
  }
  
  bind_rows(
    resultados
  )
}


# =========================================================
# 7. PROCESSAR TODAS AS TABELAS
# =========================================================

inventario_colunas <- list()

resultados_disciplinas <- list()

dados_tabelas <- list()


for (arquivo in arquivos) {
  
  dados <- carregar_arquivo(
    arquivo
  )
  
  if (is.null(dados)) {
    next
  }
  
  dados_tabelas[[arquivo]] <- dados
  
  # -------------------------------------------------------
  # Informações gerais
  # -------------------------------------------------------
  
  cat("\nLinhas:", nrow(dados), "\n")
  cat("Colunas:", ncol(dados), "\n")
  
  cat("\nNomes das colunas:\n")
  
  print(
    names(dados)
  )
  
  # -------------------------------------------------------
  # Identificar colunas
  # -------------------------------------------------------
  
  mapa_colunas <-
    identificar_colunas_disciplina(
      names(dados)
    )
  
  inventario_colunas[[arquivo]] <-
    mapa_colunas
  
  cat("\nMapa das colunas:\n")
  
  print(
    mapa_colunas
  )
  
  # -------------------------------------------------------
  # Colunas candidatas a disciplina
  # -------------------------------------------------------
  
  colunas_disciplina <-
    mapa_colunas %>%
    filter(
      possivel_disciplina
    ) %>%
    pull(
      coluna_original
    )
  
  cat(
    "\nColunas consideradas candidatas a DISCIPLINA:\n"
  )
  
  print(
    colunas_disciplina
  )
  
  # -------------------------------------------------------
  # Procurar Cálculo / FMCC
  # -------------------------------------------------------
  
  if (length(colunas_disciplina) > 0) {
    
    resultado <-
      procurar_disciplinas(
        dados = dados,
        nome_tabela = arquivo,
        colunas_disciplina =
          colunas_disciplina
      )
    
    resultados_disciplinas[[arquivo]] <-
      resultado
  }
}


# =========================================================
# 8. CONSOLIDAR RESULTADOS
# =========================================================

resultado_final <- bind_rows(
  resultados_disciplinas
)

cat("\n\n")
cat("=========================================================\n")
cat("RESULTADO CONSOLIDADO - CÁLCULO / FMCC\n")
cat("=========================================================\n")

if (nrow(resultado_final) == 0) {
  
  cat(
    "\nNenhuma ocorrência de Cálculo ou FMCC foi encontrada.\n"
  )
  
} else {
  
  print(
    resultado_final %>%
      distinct(
        tabela,
        coluna,
        tipo_busca,
        valor_original
      ) %>%
      arrange(
        tabela,
        tipo_busca,
        valor_original
      )
  )
}


# =========================================================
# 9. NOMES ÚNICOS DAS DISCIPLINAS ENCONTRADAS
# =========================================================

cat("\n\n")
cat("=========================================================\n")
cat("NOMES ÚNICOS ENCONTRADOS\n")
cat("=========================================================\n")

if (nrow(resultado_final) > 0) {
  
  nomes_encontrados <-
    resultado_final %>%
    distinct(
      tabela,
      coluna,
      tipo_busca,
      valor_original
    ) %>%
    arrange(
      tabela,
      valor_original
    )
  
  print(
    nomes_encontrados
  )
}


# =========================================================
# 10. RESUMO POR TABELA
# =========================================================

cat("\n\n")
cat("=========================================================\n")
cat("RESUMO POR TABELA\n")
cat("=========================================================\n")

if (nrow(resultado_final) > 0) {
  
  resumo <- resultado_final %>%
    distinct(
      tabela,
      coluna,
      tipo_busca,
      valor_original
    ) %>%
    group_by(
      tabela
    ) %>%
    summarise(
      ocorrencias_calculo =
        sum(
          tipo_busca == "CALCULO"
        ),
      
      ocorrencias_fmcc =
        sum(
          tipo_busca == "FMCC"
        ),
      
      .groups = "drop"
    )
  
  print(
    resumo
  )
}


# =========================================================
# 11. SALVAR RESULTADO
# =========================================================

arquivo_saida <- file.path(
  pasta_dados,
  "inventario_disciplinas_tabelas_candidatas.csv"
)

write_csv2(
  resultado_final,
  arquivo_saida
)

cat("\n\n")
cat("=========================================================\n")
cat("ARQUIVO GERADO\n")
cat("=========================================================\n")

cat(
  "\n",
  arquivo_saida,
  "\n"
)

cat("\n=========================================================\n")
cat("FIM\n")
cat("=========================================================\n")




# =========================================================
# LOCALIZAR ARQUIVOS CANDIDATOS
# =========================================================

projeto <- "C:/Users/Big Data/Documents/Master UFCG/Semestre 2026.1/ufcg-evasao-impacto-curriculo/Dados_Script_Usados"

arquivos_procurar <- c(
  "historicos de todos os alunos que ingressaram a partir de 2002.tsv",
  "historicos_todos_alunos_ingressaram_2002.xlsx",
  "alunosUFCG.csv",
  "todosAlunosTudo.csv",
  "historico.csv"
)

cat("\n=========================================================\n")
cat("LOCALIZAÇÃO DOS ARQUIVOS\n")
cat("=========================================================\n")

for (arquivo in arquivos_procurar) {
  
  encontrado <- list.files(
    projeto,
    pattern = paste0(
      "^",
      gsub(
        "([.|()\\^{}+$*?]|\\[|\\])",
        "\\\\\\1",
        arquivo
      ),
      "$"
    ),
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  cat("\n---------------------------------------------\n")
  cat("Arquivo:", arquivo, "\n")
  
  if (length(encontrado) == 0) {
    
    cat("NÃO ENCONTRADO\n")
    
  } else {
    
    cat("ENCONTRADO:\n")
    
    print(
      encontrado
    )
  }
}

cat("\n=========================================================\n")

