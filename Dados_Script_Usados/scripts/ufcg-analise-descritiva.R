# =====================================================
# analise_descritiva.R

library(readr)
library(dplyr)
library(janitor)

# =====================================================
# Diretórios
# =====================================================

projeto <- "C:/Users/Big Data/Documents/Master UFCG/Semestre 2026.1/ufcg-evasao-impacto-curriculo/Dados_Script_Usados"

pasta_processados <- file.path(projeto, "dados_processados")

pasta_resultados <- file.path(projeto, "resultados")

pasta_tabelas <- file.path(pasta_resultados, "tabelas")

pasta_graficos <- file.path(pasta_resultados, "graficos")

dir.create(pasta_tabelas,
           recursive = TRUE,
           showWarnings = FALSE)

dir.create(pasta_graficos,
           recursive = TRUE,
           showWarnings = FALSE)

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
# Verificações
# =====================================================

cat("\n=====================================\n")
cat("BASE CARREGADA\n")
cat("=====================================\n")

cat("Arquivo:\n")
cat(arquivo, "\n")

cat("\nNúmero de registros:", nrow(dados), "\n")
cat("Número de colunas:", ncol(dados), "\n")

cat("\nVariáveis da base:\n")
print(names(dados))

# =====================================================
# Currículo Entrada
# =====================================================

tab_curriculo <- dados %>%
  tabyl(`Currículo Entrada`) %>%
  adorn_pct_formatting()

write_csv(
  tab_curriculo,
  file.path(
    pasta_tabelas,
    "tabela_curriculo.csv"
  )
)


# =====================================================
# Sexo
# =====================================================

tab_sexo <- dados %>%
  tabyl(Sexo) %>%
  adorn_pct_formatting()

write_csv(
  tab_sexo,
  file.path(
    pasta_tabelas,
    "tabela_sexo.csv"
  )
)

# =====================================================
# Estado Civil
# =====================================================

tab_estado_civil <- dados %>%
  tabyl(`Estado Civil`) %>%
  adorn_pct_formatting()

write_csv(
  tab_estado_civil,
  file.path(
    pasta_tabelas,
    "tabela_estado_civil.csv"
  )
)

# =====================================================
# Cor
# =====================================================

tab_cor <- dados %>%
  tabyl(Cor) %>%
  adorn_pct_formatting()

write_csv(
  tab_cor,
  file.path(
    pasta_tabelas,
    "tabela_cor.csv"
  )
)

# =====================================================
# Cota
# =====================================================

tab_cota <- dados %>%
  tabyl(Cota) %>%
  adorn_pct_formatting()

write_csv(
  tab_cota,
  file.path(
    pasta_tabelas,
    "tabela_cota.csv"
  )
)

# =====================================================
# Forma de Ingresso
# =====================================================

tab_ingresso <- dados %>%
  tabyl(`Forma de Ingresso`) %>%
  adorn_pct_formatting()

write_csv(
  tab_ingresso,
  file.path(
    pasta_tabelas,
    "tabela_forma_ingresso.csv"
  )
)

# =====================================================
# Idade
# =====================================================

idade_resumo <- dados %>%
  summarise(
    Quantidade = n(),
    Minimo = min(`Idade Aproximada no Ingresso`,
                 na.rm = TRUE),
    Media = round(
      mean(`Idade Aproximada no Ingresso`,
           na.rm = TRUE),
      2
    ),
    Mediana = median(
      `Idade Aproximada no Ingresso`,
      na.rm = TRUE
    ),
    Maximo = max(
      `Idade Aproximada no Ingresso`,
      na.rm = TRUE
    ),
    Desvio_Padrao = round(
      sd(`Idade Aproximada no Ingresso`,
         na.rm = TRUE),
      2
    )
  )

write_csv(
  idade_resumo,
  file.path(
    pasta_tabelas,
    "tabela_idade_resumo.csv"
  )
)

# =====================================================
# Exibir resultados no console
# =====================================================

cat("\n=====================================\n")
cat("CURRÍCULO DE ENTRADA\n")
cat("=====================================\n")
print(tab_curriculo)

cat("\n=====================================\n")
cat("SEXO\n")
cat("=====================================\n")
print(tab_sexo)

cat("\n=====================================\n")
cat("ESTADO CIVIL\n")
cat("=====================================\n")
print(tab_estado_civil)

cat("\n=====================================\n")
cat("COR\n")
cat("=====================================\n")
print(tab_cor)

cat("\n=====================================\n")
cat("COTA\n")
cat("=====================================\n")
print(tab_cota)

cat("\n=====================================\n")
cat("FORMA DE INGRESSO\n")
cat("=====================================\n")
print(tab_ingresso)

cat("\n=====================================\n")
cat("ESTATÍSTICAS DA IDADE\n")
cat("=====================================\n")
print(idade_resumo)

# =====================================================
# Resumo TXT
# =====================================================

resumo <- c(
  
  "=====================================",
  "ANALISE DESCRITIVA DA AMOSTRA",
  "=====================================",
  "",
  paste("Total de estudantes:", nrow(dados)),
  paste("Total de variaveis:", ncol(dados)),
  "",
  "Arquivos gerados:",
  "tabela_curriculo.csv",
  "tabela_sexo.csv",
  "tabela_estado_civil.csv",
  "tabela_cor.csv",
  "tabela_cota.csv",
  "tabela_forma_ingresso.csv",
  "tabela_idade_resumo.csv"
  
)

writeLines(
  resumo,
  file.path(
    pasta_resultados,
    "resumo_analise_descritiva.txt"
  )
)

# =====================================================
# Arquivos gerados
# =====================================================

cat("\n=====================================\n")
cat("ARQUIVOS GERADOS\n")
cat("=====================================\n")

cat(
  "\n",
  file.path(
    pasta_tabelas,
    "tabela_curriculo.csv"
  )
)

cat(
  "\n",
  file.path(
    pasta_tabelas,
    "tabela_sexo.csv"
  )
)

cat(
  "\n",
  file.path(
    pasta_tabelas,
    "tabela_estado_civil.csv"
  )
)

cat(
  "\n",
  file.path(
    pasta_tabelas,
    "tabela_cor.csv"
  )
)

cat(
  "\n",
  file.path(
    pasta_tabelas,
    "tabela_cota.csv"
  )
)

cat(
  "\n",
  file.path(
    pasta_tabelas,
    "tabela_forma_ingresso.csv"
  )
)

cat(
  "\n",
  file.path(
    pasta_tabelas,
    "tabela_idade_resumo.csv"
  )
)

cat(
  "\n",
  file.path(
    pasta_resultados,
    "resumo_analise_descritiva.txt"
  )
)

cat("\n\nAnálise descritiva concluída com sucesso.\n")

