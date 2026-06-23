# =====================================================
# Caracterização Geral dos Dados
# =====================================================

library(readr)
library(dplyr)
library(ggplot2)


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
# Distribuição por período e currículo
# =====================================================

tabela_periodo <- dados %>%
  count(
    `Currículo Entrada`,
    `Período de Ingresso`
  )

print(tabela_periodo)

write_csv(
  tabela_periodo,
  file.path(
    pasta_tabelas,
    "perfil_geral_periodo_ingresso.csv"
  )
)

grafico_periodo <- ggplot(
  tabela_periodo,
  aes(
    x = factor(`Período de Ingresso`),
    y = n,
    fill = factor(`Currículo Entrada`)
  )
) +
  geom_col(position = "dodge") +
  labs(
    title = "Distribuição dos estudantes por período de ingresso",
    x = "Período de ingresso",
    y = "Quantidade de estudantes",
    fill = "Currículo"
  ) +
  scale_fill_manual(
    values = c(
      "1999" = "#1F77B4",
      "2017" = "#D62728"
    )
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

print(grafico_periodo)

ggsave(
  file.path(
    pasta_graficos,
    "figura_4_2_periodo_ingresso.png"
  ),
  grafico_periodo,
  width = 10,
  height = 6
)

#
# =====================================================
# Forma de ingresso
# =====================================================

tabela_ingresso <- dados %>%
  count(`Forma de Ingresso`) %>%
  arrange(desc(n))

print(tabela_ingresso)

write_csv(
  tabela_ingresso,
  file.path(
    pasta_tabelas,
    "perfil_geral_forma_ingresso.csv"
  )
)

grafico_ingresso <- ggplot(
  tabela_ingresso,
  aes(
    x = reorder(`Forma de Ingresso`, n),
    y = n,
    fill = `Forma de Ingresso`
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Distribuição dos estudantes por forma de ingresso",
    x = "Forma de ingresso",
    y = "Quantidade"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )

print(grafico_ingresso)

ggsave(
  file.path(
    pasta_graficos,
    "figura_4_3_forma_ingresso.png"
  ),
  grafico_ingresso,
  width = 10,
  height = 6
)

