# =====================================================
# Distribuição dos estudantes inativos
# por tipo de evasão
# =====================================================

tabela_evasao <- dados %>%
  
  filter(
    Status == "INATIVO",
    `Tipo de Evasão` != "GRADUADO"
  ) %>%
  
  count(
    `Currículo Entrada`,
    `Tipo de Evasão`
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
    Tipo_Evasao = `Tipo de Evasão`,
    Estudantes = n
  )

cat("\n=====================================\n")
cat("TIPOS DE EVASÃO\n")
cat("=====================================\n")

print(tabela_evasao)

# Salvar tabela

write_csv(
  tabela_evasao,
  file.path(
    pasta_tabelas,
    "tabela_tipo_evasao.csv"
  )
)

# =====================================================
# Gráfico
# =====================================================

grafico_evasao <- ggplot(
  
  tabela_evasao,
  
  aes(
    
    x = reorder(
      Tipo_Evasao,
      Estudantes
    ),
    
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
    
    vjust = -.25,
    
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
    
    title = "Distribuição dos estudantes inativos por tipo de evasão",
    
    x = "Tipo de evasão",
    
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
      angle = 35,
      hjust = 1
    ),
    
    panel.grid.minor = element_blank()
    
  )

print(grafico_evasao)

ggsave(
  
  file.path(
    pasta_graficos,
    "figura_4_6_tipo_evasao.png"
  ),
  
  grafico_evasao,
  
  width = 11,
  
  height = 6,
  
  dpi = 300
  
)

cat("\n=====================================\n")
cat("ARQUIVOS GERADOS\n")
cat("=====================================\n")

cat(
  "\nTabela:",
  "\n- tabela_tipo_evasao.csv\n"
)

cat(
  "\nGráfico:",
  "\n- figura_4_6_tipo_evasao.png\n"
)

cat("\nAnálise concluída.\n")

