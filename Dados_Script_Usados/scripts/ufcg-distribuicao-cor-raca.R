# =====================================================
# Distribuição por Cor/Raça
# =====================================================

# Tabela

tabela_cor <- dados %>%
  count(Cor) %>%
  mutate(
    Percentual = round(100 * n / sum(n), 1)
  ) %>%
  rename(
    Categoria = Cor,
    Estudantes = n
  ) %>%
  arrange(desc(Estudantes))

cat("\n=====================================\n")
cat("DISTRIBUIÇÃO POR COR/RAÇA\n")
cat("=====================================\n")

print(tabela_cor)

# Salvar tabela

write_csv(
  tabela_cor,
  file.path(
    pasta_tabelas,
    "tabela_cor_raca.csv"
  )
)

# =====================================================
# Gráfico
# =====================================================

grafico_cor <- ggplot(
  tabela_cor,
  aes(
    x = reorder(Categoria, Estudantes),
    y = Estudantes
  )
) +
  
  geom_col(
    fill = "#1F77B4",
    width = .7
  ) +
  
  geom_text(
    aes(label = Estudantes),
    vjust = -.4,
    size = 3
  ) +
  
  labs(
    title = "Distribuição dos estudantes por cor/raça",
    x = "Cor/Raça",
    y = "Quantidade de estudantes"
  ) +
  
  theme_minimal() +
  
  theme(
    
    plot.title = element_text(
      face = "bold",
      hjust = .5
    ),
    
    axis.text.x = element_text(
      angle = 30,
      hjust = 1
    )
    
  )

print(grafico_cor)

ggsave(
  
  file.path(
    pasta_graficos,
    "figura_4_5_cor_raca.png"
  ),
  
  grafico_cor,
  
  width = 8,
  
  height = 5,
  
  dpi = 300
  
)

cat("\nFigura salva.\n")