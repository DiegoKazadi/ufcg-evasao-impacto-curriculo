# =====================================================
# 12_graduados.R
# Distribuição dos alunos graduados
# =====================================================

library(readr)
library(dplyr)
library(ggplot2)

# =====================================================
# Diretórios
# =====================================================

projeto <- "C:/Users/Big Data/Documents/Master UFCG/Semestre 2026.1/ufcg-evasao-impacto-curriculo/Dados_Script_Usados"

pasta_processados <- file.path(projeto, "dados_processados")

pasta_resultados <- file.path(projeto, "resultados")

pasta_tabelas <- file.path(pasta_resultados, "tabelas")

pasta_graficos <- file.path(pasta_resultados, "graficos")

dir.create(
  pasta_tabelas,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  pasta_graficos,
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
# Verificação da base
# =====================================================

cat("\n=====================================\n")
cat("STATUS ACADÊMICO\n")
cat("=====================================\n")

print(table(dados$Status))

cat("\n=====================================\n")
cat("TIPO DE EVASÃO\n")
cat("=====================================\n")

print(table(dados$`Tipo de Evasão`))

# =====================================================
# Selecionar somente graduados
# =====================================================

graduados <- dados %>%
  filter(
    trimws(`Tipo de Evasão`) == "GRADUADO"
  )

cat("\n=====================================\n")
cat("TOTAL DE GRADUADOS\n")
cat("=====================================\n")

cat(
  "Graduados:",
  nrow(graduados),
  "\n"
)

# =====================================================
# Tabela
# =====================================================

tabela_graduados <- graduados %>%
  count(
    `Currículo Entrada`,
    `Período de Ingresso`,
    name = "Graduados"
  ) %>%
  arrange(
    `Currículo Entrada`,
    `Período de Ingresso`
  )

cat("\n=====================================\n")
cat("GRADUADOS POR PERÍODO\n")
cat("=====================================\n")

print(tabela_graduados)

write_csv(
  tabela_graduados,
  file.path(
    pasta_tabelas,
    "tabela_graduados_periodo.csv"
  )
)

# =====================================================
# Gráfico
# =====================================================

grafico_graduados <- ggplot(
  tabela_graduados,
  aes(
    x = factor(`Período de Ingresso`),
    y = Graduados,
    fill = factor(`Currículo Entrada`)
  )
) +
  
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  
  geom_text(
    aes(label = Graduados),
    position = position_dodge(width = 0.8),
    vjust = -0.3,
    size = 3
  ) +
  
  scale_fill_manual(
    values = c(
      "1999" = "#1F77B4",
      "2017" = "#D62728"
    ),
    labels = c(
      "Currículo 1999",
      "Currículo 2017"
    ),
    name = "Currículo"
  ) +
  
  labs(
    title = "Distribuição dos estudantes graduados",
    x = "Período de ingresso",
    y = "Quantidade de graduados"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    
    legend.position = "right",
    
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

print(grafico_graduados)

ggsave(
  file.path(
    pasta_graficos,
    "figura_graduados_periodo.png"
  ),
  grafico_graduados,
  width = 10,
  height = 6,
  dpi = 300
)

# =====================================================
# Resumo por currículo
# =====================================================

resumo_graduados <- graduados %>%
  count(
    `Currículo Entrada`,
    name = "Graduados"
  )

cat("\n=====================================\n")
cat("GRADUADOS POR CURRÍCULO\n")
cat("=====================================\n")

print(resumo_graduados)

write_csv(
  resumo_graduados,
  file.path(
    pasta_tabelas,
    "resumo_graduados.csv"
  )
)

# =====================================================
# Arquivo TXT
# =====================================================

texto <- c(
  
  "=====================================",
  "DISTRIBUIÇÃO DOS GRADUADOS",
  "=====================================",
  "",
  
  paste(
    "Total de graduados:",
    nrow(graduados)
  ),
  
  "",
  
  paste(
    "Currículo 1999:",
    resumo_graduados$Graduados[
      resumo_graduados$`Currículo Entrada` == 1999
    ]
  ),
  
  paste(
    "Currículo 2017:",
    resumo_graduados$Graduados[
      resumo_graduados$`Currículo Entrada` == 2017
    ]
  )
  
)

writeLines(
  texto,
  file.path(
    pasta_resultados,
    "resumo_graduados.txt"
  )
)

# =====================================================
# Final
# =====================================================

cat("\n=====================================\n")
cat("ARQUIVOS GERADOS\n")
cat("=====================================\n")

cat("\nTabelas:\n")
cat("- tabela_graduados_periodo.csv\n")
cat("- resumo_graduados.csv\n")

cat("\nGráfico:\n")
cat("- figura_graduados_periodo.png\n")

cat("\nResumo:\n")
cat("- resumo_graduados.txt\n")

cat("\nScript concluído com sucesso.\n")

