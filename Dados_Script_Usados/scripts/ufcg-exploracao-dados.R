# =====================================================
# Caracterização Geral dos Dados
# =====================================================

library(readr)
library(dplyr)
library(ggplot2)

# =====================================================
# Distribuição por currículo
# =====================================================

tabela_curriculo <- dados %>%
  count(`Currículo Entrada`) %>%
  mutate(
    Percentual =
      round(
        100 * n / sum(n),
        1
      )
  ) %>%
  rename(
    Curriculo = `Currículo Entrada`,
    Estudantes = n
  )

# visualizar

cat("\n=====================================\n")
cat("CARACTERIZAÇÃO GERAL DA AMOSTRA\n")
cat("=====================================\n")

print(tabela_curriculo)

# salvar tabela

write_csv(
  tabela_curriculo,
  file.path(
    pasta_tabelas,
    "tabela_caracterizacao_geral.csv"
  )
)

# =====================================================
# gráfico
# =====================================================

grafico_curriculo <- ggplot(
  tabela_curriculo,
  aes(
    x = factor(Curriculo),
    y = Estudantes
  )
) +
  geom_col() +
  geom_text(
    aes(
      label = Estudantes
    ),
    vjust = -0.3
  ) +
  labs(
    title = "Distribuição dos estudantes por currículo",
    x = "Currículo",
    y = "Número de estudantes"
  ) +
  theme_minimal()

print(grafico_curriculo)

ggsave(
  filename = file.path(
    pasta_graficos,
    "figura_caracterizacao_geral.png"
  ),
  plot = grafico_curriculo,
  width = 8,
  height = 5
)

cat(
  "\nTabela e gráfico salvos com sucesso.\n"
)
