# =====================================================

# selecao_amostra.R

# Objetivo:

# Reproduzir a amostra utilizada na versão final

# da dissertação após a defesa.

#

# Currículo 1999:

# Ingressantes de 2011.1 a 2015.2

# Total = 854 estudantes

#

# Currículo 2017:

# Ingressantes de 2018.1 a 2022.2

# Total = 918 estudantes

#

# Amostra Final:

# 1772 estudantes

#

# =====================================================

library(readr)
library(dplyr)

# =====================================================

# Diretórios

# =====================================================

projeto <- "C:/Users/Big Data/Documents/Master UFCG/Semestre 2026.1/ufcg-evasao-impacto-curriculo/Dados_Script_Usados"

pasta_dados <- file.path(projeto, "dados")
pasta_processados <- file.path(projeto, "dados_processados")
pasta_resultados <- file.path(projeto, "resultados")
pasta_tabelas <- file.path(pasta_resultados, "tabelas")
pasta_graficos <- file.path(pasta_resultados, "graficos")

dir.create(pasta_processados, recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_resultados, recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_tabelas, recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_graficos, recursive = TRUE, showWarnings = FALSE)

# =====================================================

# Leitura dos dados

# =====================================================

arquivo <- file.path(
  pasta_dados,
  "alunos-final.csv"
)

alunos <- read_csv(
  arquivo,
  show_col_types = FALSE
)

names(alunos) <- trimws(names(alunos))

# Renomear última coluna (Ingressantes)

names(alunos)[15] <- "Ingressantes"

# =====================================================

# Conversão de tipos

# =====================================================

alunos <- alunos %>%
  mutate(
    `Currículo Entrada` = as.numeric(`Currículo Entrada`),
    `Período de Ingresso` = as.numeric(`Período de Ingresso`),
    Ingressantes = as.numeric(Ingressantes)
  )

# =====================================================

# Seleção da amostra utilizada na defesa

# =====================================================

amostra_final <- alunos %>%
  filter(
    (`Currículo Entrada` == 1999 &
       `Período de Ingresso` >= 2011.1 &
       `Período de Ingresso` <= 2015.2)
    |
      (`Currículo Entrada` == 2017 &
         `Período de Ingresso` >= 2018.1 &
         `Período de Ingresso` <= 2022.2)
  )

# =====================================================

# Salvar amostra final

# =====================================================

write_csv(
  amostra_final,
  file.path(
    pasta_processados,
    "amostra_final_dissertacao.csv"
  )
)

# =====================================================

# Ingressantes por período

# =====================================================

tabela_periodos <- amostra_final %>%
  group_by(
    `Currículo Entrada`,
    `Período de Ingresso`
  ) %>%
  summarise(
    Ingressantes = sum(Ingressantes),
    .groups = "drop"
  ) %>%
  arrange(
    `Currículo Entrada`,
    `Período de Ingresso`
  )

# =====================================================

# Separar tabelas

# =====================================================

tabela_1999 <- tabela_periodos %>%
  filter(`Currículo Entrada` == 1999)

tabela_2017 <- tabela_periodos %>%
  filter(`Currículo Entrada` == 2017)

# =====================================================

# Salvar tabelas

# =====================================================

write_csv(
  tabela_1999,
  file.path(
    pasta_tabelas,
    "tabela_ingressantes_curriculo_1999.csv"
  )
)

write_csv(
  tabela_2017,
  file.path(
    pasta_tabelas,
    "tabela_ingressantes_curriculo_2017.csv"
  )
)

# =====================================================

# Totais

# =====================================================

total_1999 <- sum(tabela_1999$Ingressantes)

total_2017 <- sum(tabela_2017$Ingressantes)

total_amostra <- nrow(amostra_final)

# =====================================================

# Arquivo resumo

# =====================================================

resumo <- c(
  
  "=====================================",
  "AMOSTRA FINAL DA DISSERTACAO",
  "=====================================",
  "",
  "Curriculo 1999",
  paste("Total =", total_1999, "estudantes"),
  "",
  "Curriculo 2017",
  paste("Total =", total_2017, "estudantes"),
  "",
  paste(
    "Amostra Final =",
    total_amostra,
    "estudantes"
  )
  
)

writeLines(
  resumo,
  file.path(
    pasta_resultados,
    "resumo_amostra.txt"
  )
)

# =====================================================

# Saída no console

# =====================================================

cat("\n=====================================\n")
cat("AMOSTRA FINAL\n")
cat("=====================================\n")

cat("Currículo 1999:", total_1999, "\n")
cat("Currículo 2017:", total_2017, "\n")
cat("Total da amostra:", total_amostra, "\n")

cat("\nArquivos gerados:\n")
cat("- dados_processados/amostra_final_dissertacao.csv\n")
cat("- resultados/tabelas/tabela_ingressantes_curriculo_1999.csv\n")
cat("- resultados/tabelas/tabela_ingressantes_curriculo_2017.csv\n")
cat("- resultados/resumo_amostra.txt\n")

