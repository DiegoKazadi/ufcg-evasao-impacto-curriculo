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

arquivo <- file.path(
  pasta_dados,
  "alunos-final.csv"
)

# salvar amostra final

write_csv(
  amostra_final,
  file.path(
    pasta_processados,
    "amostra_final_dissertacao.csv"
  )
)

cat("\nAmostra final salva.\n")

#criar algumas pastas e salvar tabelas

tabela_periodos %>%
  filter(`Currículo Entrada` == 1999) %>%
  write_csv(
    file.path(
      pasta_tabelas,
      "tabela_ingressantes_curriculo_1999.csv"
    )
  )

tabela_periodos %>%
  filter(`Currículo Entrada` == 2017) %>%
  write_csv(
    file.path(
      pasta_tabelas,
      "tabela_ingressantes_curriculo_2017.csv"
    )
  )

# Gerar arquivo TXT

resumo <- c(
  
  "=====================================",
  "AMOSTRA FINAL DA DISSERTACAO",
  "=====================================",
  "",
  "Curriculo 1999",
  paste("Total =", sum(tabela_1999$Ingressantes), "estudantes"),
  "",
  "Curriculo 2017",
  paste("Total =", sum(tabela_2017$Ingressantes), "estudantes"),
  "",
  paste(
    "Amostra Final =",
    nrow(amostra_final),
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