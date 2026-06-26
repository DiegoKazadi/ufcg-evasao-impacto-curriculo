# =====================================================
# Distribuição por Cor/Raça
# =====================================================

# Tabela

tabela_cor <- dados %>%
  count(
    `Currículo Entrada`,
    Cor
  ) %>%
  group_by(`Currículo Entrada`) %>%
  mutate(
    Percentual = round(
      100 * n / sum(n),
      1
    )
  ) %>%
  ungroup() %>%
  rename(
    Curriculo = `Currículo Entrada`,
    Categoria = Cor,
    Estudantes = n
  )

cat("\n=====================================\n")
cat("DISTRIBUIÇÃO POR COR/RAÇA\n")
cat("=====================================\n")

print(tabela_cor)

# =====================================================
# Salvar tabela
# =====================================================

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
    
    x = Categoria,
    
    y = Estudantes,
    
    fill = factor(Curriculo)
    
  )
  
) +
  
  geom_col(
    
    position = position_dodge(
      width = 0.8
    ),
    
    width = .7
    
  ) +
  
  geom_text(
    
    aes(
      label = Estudantes
    ),
    
    position = position_dodge(
      width = 0.8
    ),
    
    vjust = -.3,
    
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
    
    legend.position = "right",
    
    legend.title = element_text(
      
      face = "bold"
      
    ),
    
    axis.text.x = element_text(
      
      angle = 30,
      
      hjust = 1
      
    ),
    
    panel.grid.minor = element_blank()
    
  )

print(grafico_cor)

# =====================================================
# Salvar gráfico
# =====================================================

ggsave(
  
  file.path(
    
    pasta_graficos,
    
    "figura_4_5_cor_raca.png"
    
  ),
  
  grafico_cor,
  
  width = 9,
  
  height = 5.5,
  
  dpi = 300
  
)

cat("\n=====================================\n")
cat("ARQUIVOS GERADOS\n")
cat("=====================================\n")

cat(
  "\nTabela:",
  "\n- tabela_cor_raca.csv\n"
)

cat(
  "\nGráfico:",
  "\n- figura_4_5_cor_raca.png\n"
)

cat("\nAnálise concluída.\n")
