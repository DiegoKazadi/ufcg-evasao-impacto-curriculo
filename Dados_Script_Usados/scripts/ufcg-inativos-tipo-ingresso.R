# =====================================================
# Distribuição dos estudantes inativos
# por tipo de evasão
# =====================================================

tabela_evasao <- dados %>%
  
  filter(
    
    Status == "INATIVO",
    
    `Tipo de Evasão` != "GRADUADO"
    
  ) %>%
  
  count(`Tipo de Evasão`) %>%
  
  mutate(
    
    Percentual =
      round(
        100 * n / sum(n),
        1
      )
    
  ) %>%
  
  rename(
    
    Tipo_Evasao = `Tipo de Evasão`,
    
    Estudantes = n
    
  ) %>%
  
  arrange(desc(Estudantes))

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
    
    y = reorder(
      
      Tipo_Evasao,
      
      Estudantes
      
    ),
    
    x = Estudantes
    
  )
  
) +
  
  geom_col(
    
    fill = "#D62728",
    
    width = .7
    
  ) +
  
  geom_text(
    
    aes(
      
      label = Estudantes
      
    ),
    
    hjust = -.2,
    
    size = 3
    
  ) +
  
  labs(
    
    title = "Distribuição dos estudantes inativos por tipo de evasão",
    
    x = "Quantidade de estudantes",
    
    y = NULL
    
  ) +
  
  xlim(
    
    0,
    
    max(tabela_evasao$Estudantes) * 1.15
    
  ) +
  
  theme_minimal() +
  
  theme(
    
    plot.title = element_text(
      
      face = "bold",
      
      hjust = .5
      
    )
    
  )

print(grafico_evasao)

ggsave(
  
  file.path(
    
    pasta_graficos,
    
    "figura_4_6_tipo_evasao.png"
    
  ),
  
  grafico_evasao,
  
  width = 10,
  
  height = 6,
  
  dpi = 300
  
)

cat("\nFigura salva.\n")