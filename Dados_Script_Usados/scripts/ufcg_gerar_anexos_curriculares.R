# =====================================================
# gerar_anexos_curriculares.R
#
# Objetivo:
# Gerar os Anexos A e B da dissertação
# organizando as disciplinas obrigatórias
# por período ideal.
# =====================================================

library(readr)
library(dplyr)

# =====================================================
# Diretórios
# =====================================================

projeto <- "C:/Users/Big Data/Documents/Master UFCG/Semestre 2026.1/ufcg-evasao-impacto-curriculo/Dados_Script_Usados"

pasta_dados <- file.path(
  projeto,
  "dados"
)

pasta_resultados <- file.path(
  projeto,
  "resultados"
)

pasta_anexos <- file.path(
  pasta_resultados,
  "anexos"
)

dir.create(
  pasta_anexos,
  recursive = TRUE,
  showWarnings = FALSE
)

# =====================================================
# Leitura dos currículos
# =====================================================

curriculo_1999 <- read_csv2(
  file.path(
    pasta_dados,
    "disciplinas_curriculo_1999.csv"
  ),
  show_col_types = FALSE
)

curriculo_2017 <- read_csv2(
  file.path(
    pasta_dados,
    "disciplinas_curriculo_2017.csv"
  ),
  show_col_types = FALSE
)

# =====================================================
# Filtrar obrigatórias
# =====================================================

obrigatorias_1999 <- curriculo_1999 %>%
  filter(
    TIPO == "OBRIGATORIO",
    SEMESTRE_IDEAL > 0
  ) %>%
  arrange(
    SEMESTRE_IDEAL,
    NOME_DISCIPLINA
  )

obrigatorias_2017 <- curriculo_2017 %>%
  filter(
    TIPO == "OBRIGATORIO",
    SEMESTRE_IDEAL > 0
  ) %>%
  arrange(
    SEMESTRE_IDEAL,
    NOME_DISCIPLINA
  )

# =====================================================
# Função para gerar anexo
# =====================================================

gerar_anexo <- function(base, titulo) {
  
  texto <- c()
  
  texto <- c(
    texto,
    titulo,
    ""
  )
  
  periodos <- sort(
    unique(base$SEMESTRE_IDEAL)
  )
  
  for(p in periodos){
    
    texto <- c(
      texto,
      paste0(p, "º Período"),
      ""
    )
    
    disciplinas <- base %>%
      filter(
        SEMESTRE_IDEAL == p
      ) %>%
      pull(
        NOME_DISCIPLINA
      )
    
    disciplinas <- paste0(
      "• ",
      disciplinas
    )
    
    texto <- c(
      texto,
      disciplinas,
      ""
    )
  }
  
  return(texto)
}

# =====================================================
# Gerar Anexo A
# =====================================================

anexo_A <- gerar_anexo(
  obrigatorias_1999,
  "ANEXO A – Estrutura Curricular do Curso de Ciência da Computação (Currículo 1999)"
)

writeLines(
  anexo_A,
  file.path(
    pasta_anexos,
    "anexo_A_curriculo_1999.txt"
  )
)

# =====================================================
# Gerar Anexo B
# =====================================================

anexo_B <- gerar_anexo(
  obrigatorias_2017,
  "ANEXO B – Estrutura Curricular do Curso de Ciência da Computação (Currículo 2017)"
)

writeLines(
  anexo_B,
  file.path(
    pasta_anexos,
    "anexo_B_curriculo_2017.txt"
  )
)

# =====================================================
# Mostrar resumo
# =====================================================

cat("\n=====================================\n")
cat("ANEXOS GERADOS\n")
cat("=====================================\n")

cat(
  "\nCurrículo 1999:",
  nrow(obrigatorias_1999),
  "disciplinas obrigatórias"
)

cat(
  "\nCurrículo 2017:",
  nrow(obrigatorias_2017),
  "disciplinas obrigatórias"
)

cat(
  "\n\nArquivos:\n"
)

cat(
  "\n- resultados/anexos/anexo_A_curriculo_1999.txt"
)

cat(
  "\n- resultados/anexos/anexo_B_curriculo_2017.txt\n"
)

