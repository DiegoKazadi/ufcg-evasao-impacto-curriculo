# =====================================================
# diagnostico_amostra.R
#
# Objetivo:
# Verificar a estrutura da amostra final
# utilizada na dissertação.
# =====================================================

library(readr)
library(dplyr)

# =====================================================
# Diretórios
# =====================================================

projeto <- "C:/Users/Big Data/Documents/Master UFCG/Semestre 2026.1/ufcg-evasao-impacto-curriculo/Dados_Script_Usados"

pasta_processados <- file.path(projeto, "dados_processados")

pasta_resultados <- file.path(projeto, "resultados")

pasta_tabelas <- file.path(pasta_resultados, "tabelas")

dir.create(
  pasta_tabelas,
  recursive = TRUE,
  showWarnings = FALSE
)

# =====================================================
# Carregar base
# =====================================================

arquivo <- file.path(
  pasta_processados,
  "amostra_final_dissertacao.csv"
)

dados <- read_csv(
  arquivo,
  show_col_types = FALSE
)

# =====================================================
# Estrutura da base
# =====================================================

cat("\n=====================================\n")
cat("ESTRUTURA DA BASE\n")
cat("=====================================\n")

print(names(dados))

cat("\nTotal de registros:", nrow(dados), "\n")

# =====================================================
# Função auxiliar
# =====================================================

explorar_variavel <- function(base, variavel){
  
  cat("\n=====================================\n")
  cat("VARIÁVEL:", variavel, "\n")
  cat("=====================================\n")
  
  tabela <- base %>%
    count(.data[[variavel]]) %>%
    arrange(desc(n))
  
  print(tabela, n = Inf)
  
  write_csv(
    tabela,
    file.path(
      pasta_tabelas,
      paste0(
        "diagnostico_",
        gsub(" ", "_", variavel),
        ".csv"
      )
    )
  )
}

# =====================================================
# Variáveis categóricas
# =====================================================

explorar_variavel(
  dados,
  "Sexo"
)

explorar_variavel(
  dados,
  "Cor"
)

explorar_variavel(
  dados,
  "Cota"
)

explorar_variavel(
  dados,
  "Status"
)

explorar_variavel(
  dados,
  "Tipo de Evasão"
)

explorar_variavel(
  dados,
  "Forma de Ingresso"
)

explorar_variavel(
  dados,
  "Currículo"
)

explorar_variavel(
  dados,
  "Currículo Entrada"
)

# =====================================================
# Idade
# =====================================================

cat("\n=====================================\n")
cat("IDADE APROXIMADA NO INGRESSO\n")
cat("=====================================\n")

summary(
  dados$`Idade Aproximada no Ingresso`
)

idade_resumo <- data.frame(
  
  Minimo =
    min(
      dados$`Idade Aproximada no Ingresso`,
      na.rm = TRUE
    ),
  
  Q1 =
    quantile(
      dados$`Idade Aproximada no Ingresso`,
      0.25,
      na.rm = TRUE
    ),
  
  Mediana =
    median(
      dados$`Idade Aproximada no Ingresso`,
      na.rm = TRUE
    ),
  
  Media =
    mean(
      dados$`Idade Aproximada no Ingresso`,
      na.rm = TRUE
    ),
  
  Q3 =
    quantile(
      dados$`Idade Aproximada no Ingresso`,
      0.75,
      na.rm = TRUE
    ),
  
  Maximo =
    max(
      dados$`Idade Aproximada no Ingresso`,
      na.rm = TRUE
    )
  
)

print(idade_resumo)

write_csv(
  idade_resumo,
  file.path(
    pasta_tabelas,
    "diagnostico_idade.csv"
  )
)

cat("\nDiagnóstico concluído.\n")
