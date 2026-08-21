# =========================================================
# INVENTÁRIO COMPLETO DAS TABELAS
# MESTRADO UFCG - ANÁLISE DE HISTÓRICO ACADÊMICO
# =========================================================

rm(list = ls())

library(readr)
library(dplyr)
library(readxl)
library(stringr)
library(tibble)

options(scipen = 999)

# =========================================================
# 1. DIRETÓRIO DOS DADOS
# =========================================================

projeto <- "C:/Users/Big Data/Documents/Master UFCG/Semestre 2026.1/ufcg-evasao-impacto-curriculo/Dados_Script_Usados"

pasta_dados <- file.path(
  projeto,
  "dados"
)

# =========================================================
# 2. LISTAR TODOS OS ARQUIVOS
# =========================================================

arquivos <- list.files(
  pasta_dados,
  full.names = TRUE
)

# Remover arquivos temporários do Excel
arquivos <- arquivos[
  !grepl("^~\\$", basename(arquivos))
]

cat("\n=========================================================\n")
cat("INVENTÁRIO DOS ARQUIVOS\n")
cat("=========================================================\n")

cat("\nPasta analisada:\n")
cat(pasta_dados, "\n")

cat("\nQuantidade de arquivos encontrados:", length(arquivos), "\n\n")

print(basename(arquivos))


# =========================================================
# 3. IDENTIFICAR EXTENSÕES
# =========================================================

extensoes <- tools::file_ext(arquivos)

cat("\n=========================================================\n")
cat("TIPOS DE ARQUIVOS\n")
cat("=========================================================\n")

print(
  table(
    toupper(extensoes)
  )
)


# =========================================================
# 4. FUNÇÃO PARA IDENTIFICAR POSSÍVEIS COLUNAS-CHAVE
# =========================================================

identificar_colunas <- function(nomes) {
  
  nomes_original <- nomes
  
  nomes_lower <- tolower(nomes)
  
  resultado <- tibble(
    coluna = nomes_original,
    possivel_matricula = str_detect(
      nomes_lower,
      "matric|ra$|registro|cod.*aluno|id.*aluno|aluno.*id"
    ),
    possivel_aluno = str_detect(
      nomes_lower,
      "aluno|discente|estudante"
    ),
    possivel_periodo = str_detect(
      nomes_lower,
      "period|semestre|ano|ingresso|evasao"
    ),
    possivel_disciplina = str_detect(
      nomes_lower,
      "discipl|componente|cadeira|materia"
    ),
    possivel_codigo_disciplina = str_detect(
      nomes_lower,
      "cod.*discipl|cod.*comp|codigo.*discipl"
    ),
    possivel_nota = str_detect(
      nomes_lower,
      "nota|media|grau|conceito"
    ),
    possivel_situacao = str_detect(
      nomes_lower,
      "situac|status|resultado|aprov|reprov|tranc|cancel"
    ),
    possivel_curso = str_detect(
      nomes_lower,
      "curso|graduacao"
    ),
    possivel_curriculo = str_detect(
      nomes_lower,
      "curric"
    )
  )
  
  resultado
}


# =========================================================
# 5. TABELA RESUMO DOS ARQUIVOS
# =========================================================

resumo_arquivos <- tibble(
  arquivo = basename(arquivos),
  extensao = toupper(tools::file_ext(arquivos)),
  linhas = NA_integer_,
  colunas = NA_integer_,
  nomes_colunas = NA_character_,
  status = NA_character_
)


# =========================================================
# 6. LISTA PARA GUARDAR OS NOMES DAS COLUNAS
# =========================================================

informacoes_colunas <- list()


# =========================================================
# 7. CARREGAR ARQUIVO POR ARQUIVO
# =========================================================

for (i in seq_along(arquivos)) {
  
  arquivo_atual <- arquivos[i]
  
  nome_arquivo <- basename(
    arquivo_atual
  )
  
  extensao <- tolower(
    tools::file_ext(
      arquivo_atual
    )
  )
  
  cat("\n\n")
  cat("=========================================================\n")
  cat("ARQUIVO", i, "DE", length(arquivos), "\n")
  cat("=========================================================\n")
  
  cat("\nNome:", nome_arquivo, "\n")
  cat("Extensão:", extensao, "\n")
  
  
  # -------------------------------------------------------
  # TENTAR CARREGAR
  # -------------------------------------------------------
  
  resultado <- tryCatch({
    
    if (extensao == "csv") {
      
      # Primeiro tenta CSV separado por ;
      dados <- read_delim(
        arquivo_atual,
        delim = ";",
        show_col_types = FALSE,
        progress = FALSE
      )
      
    } else if (extensao == "xlsx") {
      
      dados <- read_excel(
        arquivo_atual
      )
      
    } else if (extensao == "xls") {
      
      dados <- read_excel(
        arquivo_atual
      )
      
    } else {
      
      stop(
        "Formato não suportado automaticamente"
      )
      
    }
    
    list(
      sucesso = TRUE,
      dados = dados,
      erro = NA_character_
    )
    
  }, error = function(e) {
    
    list(
      sucesso = FALSE,
      dados = NULL,
      erro = conditionMessage(e)
    )
    
  })
  
  
  # -------------------------------------------------------
  # SE CARREGOU
  # -------------------------------------------------------
  
  if (resultado$sucesso) {
    
    dados <- resultado$dados
    
    n_linhas <- nrow(dados)
    n_colunas <- ncol(dados)
    
    nomes <- names(dados)
    
    # Atualizar resumo
    resumo_arquivos$linhas[i] <- n_linhas
    resumo_arquivos$colunas[i] <- n_colunas
    resumo_arquivos$nomes_colunas[i] <- paste(
      nomes,
      collapse = " | "
    )
    
    resumo_arquivos$status[i] <- "OK"
    
    
    # -----------------------------------------------------
    # MOSTRAR INFORMAÇÕES
    # -----------------------------------------------------
    
    cat("\nLinhas:", n_linhas, "\n")
    cat("Colunas:", n_colunas, "\n")
    
    cat("\nNOMES DAS COLUNAS:\n")
    print(nomes)
    
    
    # -----------------------------------------------------
    # POSSÍVEIS COLUNAS IMPORTANTES
    # -----------------------------------------------------
    
    possiveis <- identificar_colunas(
      nomes
    )
    
    colunas_importantes <- possiveis %>%
      filter(
        possivel_matricula |
          possivel_aluno |
          possivel_periodo |
          possivel_disciplina |
          possivel_codigo_disciplina |
          possivel_nota |
          possivel_situacao |
          possivel_curriculo
      )
    
    if (nrow(colunas_importantes) > 0) {
      
      cat("\nPOSSÍVEIS COLUNAS IMPORTANTES:\n")
      
      print(
        colunas_importantes
      )
      
    } else {
      
      cat(
        "\nNenhuma coluna-chave identificada automaticamente.\n"
      )
      
    }
    
    
    # -----------------------------------------------------
    # PRIMEIRAS 3 LINHAS
    # -----------------------------------------------------
    
    cat("\nPRIMEIRAS 3 LINHAS:\n")
    
    print(
      head(
        dados,
        3
      )
    )
    
    
    # -----------------------------------------------------
    # GUARDAR INFORMAÇÕES
    # -----------------------------------------------------
    
    informacoes_colunas[[nome_arquivo]] <- list(
      arquivo = nome_arquivo,
      linhas = n_linhas,
      colunas = n_colunas,
      nomes = nomes,
      possiveis = possiveis
    )
    
    
  } else {
    
    # -----------------------------------------------------
    # ERRO
    # -----------------------------------------------------
    
    resumo_arquivos$status[i] <- paste(
      "ERRO:",
      resultado$erro
    )
    
    cat(
      "\nERRO AO CARREGAR:\n",
      resultado$erro,
      "\n"
    )
    
  }
  
}


# =========================================================
# 8. RESUMO FINAL
# =========================================================

cat("\n\n")
cat("=========================================================\n")
cat("RESUMO FINAL DOS ARQUIVOS\n")
cat("=========================================================\n")

print(
  resumo_arquivos %>%
    select(
      arquivo,
      extensao,
      linhas,
      colunas,
      status
    )
)


# =========================================================
# 9. ARQUIVOS QUE POSSUEM POSSÍVEL MATRÍCULA
# =========================================================

cat("\n\n")
cat("=========================================================\n")
cat("ARQUIVOS COM POSSÍVEL COLUNA DE MATRÍCULA\n")
cat("=========================================================\n")

for (nome in names(informacoes_colunas)) {
  
  info <- informacoes_colunas[[nome]]
  
  colunas_matricula <- info$possiveis %>%
    filter(
      possivel_matricula
    )
  
  if (nrow(colunas_matricula) > 0) {
    
    cat(
      "\nARQUIVO:",
      nome,
      "\n"
    )
    
    print(
      colunas_matricula$coluna
    )
  }
}


# =========================================================
# 10. ARQUIVOS COM POSSÍVEL HISTÓRICO ACADÊMICO
# =========================================================

cat("\n\n")
cat("=========================================================\n")
cat("ARQUIVOS COM POSSÍVEL ESTRUTURA DE HISTÓRICO\n")
cat("=========================================================\n")

for (nome in names(informacoes_colunas)) {
  
  info <- informacoes_colunas[[nome]]
  
  p <- info$possiveis
  
  tem_matricula <- any(
    p$possivel_matricula
  )
  
  tem_disciplina <- any(
    p$possivel_disciplina |
      p$possivel_codigo_disciplina
  )
  
  tem_periodo <- any(
    p$possivel_periodo
  )
  
  tem_situacao <- any(
    p$possivel_situacao
  )
  
  if (
    tem_matricula &&
    tem_disciplina &&
    (tem_periodo || tem_situacao)
  ) {
    
    cat(
      "\n>>> CANDIDATO FORTE:",
      nome,
      "\n"
    )
    
    cat(
      "Linhas:",
      info$linhas,
      "\n"
    )
    
    cat(
      "Colunas:\n"
    )
    
    print(
      info$nomes
    )
  }
}


# =========================================================
# 11. SALVAR RESUMO DO INVENTÁRIO
# =========================================================

arquivo_resumo <- file.path(
  pasta_dados,
  "inventario_tabelas.csv"
)

write_csv2(
  resumo_arquivos,
  arquivo_resumo
)

cat("\n\n")
cat("=========================================================\n")
cat("INVENTÁRIO CONCLUÍDO\n")
cat("=========================================================\n")

cat(
  "\nResumo salvo em:\n",
  arquivo_resumo,
  "\n"
)

cat(
  "\nTotal de arquivos analisados:",
  nrow(resumo_arquivos),
  "\n"
)

cat(
  "Arquivos carregados com sucesso:",
  sum(resumo_arquivos$status == "OK"),
  "\n"
)

cat(
  "Arquivos com erro:",
  sum(resumo_arquivos$status != "OK"),
  "\n"
)

cat("\n=========================================================\n")
cat("FIM\n")
cat("=========================================================\n")




# =========================================================
# TESTE DE COMPATIBILIDADE DO HISTÓRICO COM A AMOSTRA
# =========================================================

rm(list = ls())

library(readr)
library(dplyr)

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
# 3. CARREGAR TABELAS DE HISTÓRICO
# =========================================================

tabela_historico <- read_csv2(
  file.path(
    pasta_dados,
    "tabela_historico.csv"
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

historico <- read_csv2(
  file.path(
    pasta_dados,
    "historico.csv"
  ),
  show_col_types = FALSE
)

# =========================================================
# 4. PREPARAR MATRÍCULAS DA AMOSTRA
# =========================================================

matriculas_amostra <- amostra %>%
  transmute(
    MATRICULA_AMOSTRA = as.character(Matricula)
  ) %>%
  distinct()

cat("\n=========================================================\n")
cat("AMOSTRA FINAL\n")
cat("=========================================================\n")

cat(
  "\nNúmero de registros da amostra:",
  nrow(amostra),
  "\n"
)

cat(
  "Matrículas únicas:",
  nrow(matriculas_amostra),
  "\n"
)

# =========================================================
# 5. PADRONIZAR MATRÍCULAS
# =========================================================

tabela_historico <- tabela_historico %>%
  mutate(
    MATRICULA = as.character(MATRICULA)
  )

matriculas <- matriculas %>%
  mutate(
    MATRICULA = as.character(MATRICULA)
  )

historico <- historico %>%
  mutate(
    MATRICULA = as.character(MATRICULA)
  )

# =========================================================
# 6. FUNÇÃO DE TESTE
# =========================================================

testar_historico <- function(
    dados,
    nome_tabela,
    coluna_periodo = NULL,
    coluna_disciplina = NULL
) {
  
  cat("\n\n")
  cat("=========================================================\n")
  cat("TESTANDO:", nome_tabela, "\n")
  cat("=========================================================\n")
  
  # -------------------------------------------------------
  # Matrículas únicas
  # -------------------------------------------------------
  
  matriculas_tabela <- dados %>%
    filter(
      !is.na(MATRICULA)
    ) %>%
    distinct(
      MATRICULA
    )
  
  cat(
    "\nMatrículas únicas na tabela:",
    nrow(matriculas_tabela),
    "\n"
  )
  
  # -------------------------------------------------------
  # Match com amostra
  # -------------------------------------------------------
  
  match <- matriculas_amostra %>%
    mutate(
      encontrada =
        MATRICULA_AMOSTRA %in%
        matriculas_tabela$MATRICULA
    )
  
  encontradas <- sum(
    match$encontrada
  )
  
  nao_encontradas <- sum(
    !match$encontrada
  )
  
  percentual <- round(
    encontradas /
      nrow(match) *
      100,
    2
  )
  
  cat(
    "\nMatrículas da amostra encontradas:",
    encontradas,
    "\n"
  )
  
  cat(
    "Matrículas NÃO encontradas:",
    nao_encontradas,
    "\n"
  )
  
  cat(
    "Cobertura:",
    percentual,
    "%\n"
  )
  
  # -------------------------------------------------------
  # Filtrar histórico para nossa amostra
  # -------------------------------------------------------
  
  historico_amostra <- dados %>%
    semi_join(
      matriculas_amostra,
      by = c(
        "MATRICULA" =
          "MATRICULA_AMOSTRA"
      )
    )
  
  cat(
    "\nRegistros de histórico pertencentes à amostra:",
    nrow(historico_amostra),
    "\n"
  )
  
  # -------------------------------------------------------
  # Quantidade de registros por matrícula
  # -------------------------------------------------------
  
  resumo_alunos <- historico_amostra %>%
    count(
      MATRICULA,
      name = "N_REGISTROS"
    )
  
  if (nrow(resumo_alunos) > 0) {
    
    cat(
      "\nRegistros por aluno:\n"
    )
    
    print(
      resumo_alunos %>%
        summarise(
          minimo = min(N_REGISTROS),
          media = round(
            mean(N_REGISTROS),
            2
          ),
          mediana = median(
            N_REGISTROS
          ),
          maximo = max(
            N_REGISTROS
          )
        )
    )
  }
  
  # -------------------------------------------------------
  # Disciplinas únicas
  # -------------------------------------------------------
  
  if (!is.null(coluna_disciplina)) {
    
    cat(
      "\nDisciplinas únicas na amostra:",
      n_distinct(
        historico_amostra[[
          coluna_disciplina
        ]]
      ),
      "\n"
    )
  }
  
  # -------------------------------------------------------
  # Períodos
  # -------------------------------------------------------
  
  if (!is.null(coluna_periodo)) {
    
    cat(
      "\nPeríodo mínimo:",
      min(
        historico_amostra[[
          coluna_periodo
        ]],
        na.rm = TRUE
      ),
      "\n"
    )
    
    cat(
      "Período máximo:",
      max(
        historico_amostra[[
          coluna_periodo
        ]],
        na.rm = TRUE
      ),
      "\n"
    )
    
    cat(
      "\nPeríodos encontrados:\n"
    )
    
    print(
      sort(
        unique(
          historico_amostra[[
            coluna_periodo
          ]]
        )
      )
    )
  }
  
  # -------------------------------------------------------
  # Primeiras matrículas da amostra
  # -------------------------------------------------------
  
  exemplos <- matriculas_amostra %>%
    slice_head(
      n = 5
    )
  
  cat(
    "\n=========================================================\n"
  )
  
  cat(
    "EXEMPLOS DE HISTÓRICO DA AMOSTRA\n"
  )
  
  cat(
    "=========================================================\n"
  )
  
  for (mat in exemplos$MATRICULA_AMOSTRA) {
    
    cat(
      "\nMATRÍCULA:",
      mat,
      "\n"
    )
    
    print(
      historico_amostra %>%
        filter(
          MATRICULA == mat
        ) %>%
        head(10)
    )
  }
  
  # -------------------------------------------------------
  # Retornar histórico filtrado
  # -------------------------------------------------------
  
  return(
    list(
      match = match,
      historico_amostra = historico_amostra,
      resumo_alunos = resumo_alunos
    )
  )
}


# =========================================================
# 7. TESTAR TABELA_HISTORICO
# =========================================================

resultado_tabela_historico <- testar_historico(
  dados = tabela_historico,
  nome_tabela = "tabela_historico.csv",
  coluna_periodo = "PERIODO",
  coluna_disciplina = "DISCIPLINA"
)


# =========================================================
# 8. TESTAR MATRICULAS
# =========================================================

resultado_matriculas <- testar_historico(
  dados = matriculas,
  nome_tabela = "matriculas.csv",
  coluna_periodo = "TERMO",
  coluna_disciplina = "NOME"
)


# =========================================================
# 9. TESTAR HISTORICO
# =========================================================

resultado_historico <- testar_historico(
  dados = historico,
  nome_tabela = "historico.csv",
  coluna_periodo = "PERIODO",
  coluna_disciplina = "DISCIPLINA"
)


# =========================================================
# 10. COMPARAÇÃO FINAL
# =========================================================

cat("\n\n")
cat("=========================================================\n")
cat("COMPARAÇÃO FINAL\n")
cat("=========================================================\n")

comparacao <- tibble(
  tabela = c(
    "tabela_historico.csv",
    "matriculas.csv",
    "historico.csv"
  ),
  
  registros = c(
    nrow(tabela_historico),
    nrow(matriculas),
    nrow(historico)
  ),
  
  matriculas_unicas = c(
    n_distinct(
      tabela_historico$MATRICULA
    ),
    n_distinct(
      matriculas$MATRICULA
    ),
    n_distinct(
      historico$MATRICULA
    )
  ),
  
  matriculas_amostra_encontradas = c(
    sum(
      resultado_tabela_historico$match$encontrada
    ),
    sum(
      resultado_matriculas$match$encontrada
    ),
    sum(
      resultado_historico$match$encontrada
    )
  )
) %>%
  mutate(
    cobertura_percentual =
      round(
        matriculas_amostra_encontradas /
          nrow(matriculas_amostra) *
          100,
        2
      )
  )

print(
  comparacao
)

cat("\n=========================================================\n")
cat("FIM DO TESTE\n")
cat("=========================================================\n")