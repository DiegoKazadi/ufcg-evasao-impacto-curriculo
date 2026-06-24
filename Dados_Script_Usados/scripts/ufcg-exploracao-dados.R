# =====================================================
# analise_exploratoria.R
# Caracterização Geral dos Dados
# =====================================================

library(readr)
library(dplyr)
library(ggplot2)


# =====================================================
# Caracterização geral da amostra
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

cat("\n=====================================\n")
cat("CARACTERIZAÇÃO GERAL DA AMOSTRA\n")
cat("=====================================\n")

print(tabela_curriculo)

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

cat("\n=====================================\n")
cat("INGRESSANTES POR PERÍODO\n")
cat("=====================================\n")

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
  
  geom_col(
    position = position_dodge(
      width = 0.9
    )
  ) +
  
  geom_text(
    aes(label = n),
    position = position_dodge(
      width = 0.9
    ),
    vjust = -0.3,
    size = 3
  ) +
  
  scale_fill_manual(
    values = c(
      "1999" = "#1F77B4",
      "2017" = "#D62728"
    )
  ) +
  
  labs(
    title = "Distribuição dos estudantes por período de ingresso",
    x = "Período de ingresso",
    y = "Quantidade de estudantes",
    fill = "Currículo"
  ) +
  
  ylim(
    0,
    max(tabela_periodo$n) + 15
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    
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

# =====================================================
# Distribuição por forma de ingresso e currículo
# =====================================================

tabela_ingresso <- dados %>%
  count(
    `Currículo Entrada`,
    `Forma de Ingresso`
  ) %>%
  arrange(
    `Currículo Entrada`,
    desc(n)
  )

cat("\n=====================================\n")
cat("FORMA DE INGRESSO POR CURRÍCULO\n")
cat("=====================================\n")

print(tabela_ingresso)

write_csv(
  tabela_ingresso,
  file.path(
    pasta_tabelas,
    "perfil_geral_forma_ingresso_curriculo.csv"
  )
)

grafico_ingresso <- ggplot(
  tabela_ingresso,
  aes(
    x = `Forma de Ingresso`,
    y = n,
    fill = factor(`Currículo Entrada`)
  )
) +
  
  geom_col(
    position = position_dodge(
      width = 0.9
    )
  ) +
  
  geom_text(
    aes(label = n),
    position = position_dodge(
      width = 0.9
    ),
    vjust = -0.3,
    size = 3
  ) +
  
  scale_fill_manual(
    values = c(
      "1999" = "#1F77B4",
      "2017" = "#D62728"
    )
  ) +
  
  labs(
    title = "Distribuição dos estudantes por forma de ingresso",
    x = "Forma de ingresso",
    y = "Quantidade de estudantes",
    fill = "Currículo"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

print(grafico_ingresso)

ggsave(
  file.path(
    pasta_graficos,
    "figura_4_3_forma_ingresso_curriculo.png"
  ),
  grafico_ingresso,
  width = 11,
  height = 6
)
# =====================================================
# Resumo Final
# =====================================================

cat("\n=====================================\n")
cat("ARQUIVOS GERADOS\n")
cat("=====================================\n")

cat(
  "\nTabela:",
  "\n- tabela_caracterizacao_geral.csv",
  "\n- perfil_geral_periodo_ingresso.csv",
  "\n- perfil_geral_forma_ingresso.csv\n"
)

cat(
  "\nGráficos:",
  "\n- figura_4_2_periodo_ingresso.png",
  "\n- figura_4_3_forma_ingresso.png\n"
)

cat("\nAnálise exploratória concluída.\n")

