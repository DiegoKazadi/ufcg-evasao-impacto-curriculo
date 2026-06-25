# =====================================================
# Distribuição dos alunos graduados
# Ingressantes + Percentual de graduados
# =====================================================

# Ingressantes por coorte

ingressantes <- dados %>%
  count(
    `Currículo Entrada`,
    `Período de Ingresso`,
    name = "Ingressantes"
  )

# Graduados por coorte

graduados <- dados %>%
  filter(`Tipo de Evasão` == "GRADUADO") %>%
  count(
    `Currículo Entrada`,
    `Período de Ingresso`,
    name = "Graduados"
  )

# =====================================================
# Junta as tabelas
# =====================================================

tabela_graduados <- ingressantes %>%
  left_join(
    graduados,
    by = c(
      "Currículo Entrada",
      "Período de Ingresso"
    )
  ) %>%
  mutate(
    
    Graduados = ifelse(
      is.na(Graduados),
      0,
      Graduados
    ),
    
    Percentual =
      round(
        100 * Graduados / Ingressantes,
        1
      ),
    
    # Não exibir percentual das coortes
    # que ainda não tiveram tempo suficiente
    # para conclusão do curso
    
    Percentual_plot =
      ifelse(
        `Período de Ingresso` >= 2021.1,
        NA,
        Percentual
      )
    
  )

cat("\n=====================================\n")
cat("INGRESSANTES E PERCENTUAL DE GRADUADOS\n")
cat("=====================================\n")

print(tabela_graduados)

# =====================================================
# Salvar tabela
# =====================================================

write_csv(
  tabela_graduados,
  file.path(
    pasta_tabelas,
    "tabela_graduados_percentual.csv"
  )
)

# =====================================================
# Escala do eixo secundário
# =====================================================

escala <- max(tabela_graduados$Ingressantes) / 100

# =====================================================
# Gráfico
# =====================================================

grafico_graduados <- ggplot(
  tabela_graduados,
  aes(
    x = factor(`Período de Ingresso`)
  )
) +
  
  # Barras
  
  geom_col(
    aes(
      y = Ingressantes,
      fill = factor(`Currículo Entrada`)
    ),
    width = .72
  ) +
  
  # Valores das barras
  
  geom_text(
    aes(
      y = Ingressantes,
      label = Ingressantes
    ),
    vjust = -0.35,
    size = 3
  ) +
  
  # Linha do percentual
  
  geom_line(
    aes(
      y = Percentual_plot * escala,
      group = factor(`Currículo Entrada`)
    ),
    colour = "gray30",
    linewidth = .6
  ) +
  
  # Pontos
  
  geom_point(
    aes(
      y = Percentual_plot * escala
    ),
    colour = "gray30",
    size = 2.3
  ) +
  
  # Percentuais
  
  geom_text(
    aes(
      y = Percentual_plot * escala,
      label =
        ifelse(
          is.na(Percentual_plot),
          "",
          paste0(
            Percentual_plot,
            "%"
          )
        )
    ),
    colour = "gray20",
    size = 2.8,
    vjust = -0.8
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
  
  scale_y_continuous(
    
    name = "Número de ingressantes",
    
    breaks = seq(
      0,
      110,
      20
    ),
    
    sec.axis = sec_axis(
      ~ . / escala,
      name = "% de graduados"
    )
    
  ) +
  
  labs(
    
    title = "Ingressantes e percentual de estudantes graduados por coorte",
    
    x = "Período de ingresso"
    
  ) +
  
  theme_minimal() +
  
  theme(
    
    plot.title = element_text(
      face = "bold",
      size = 16,
      hjust = .5
    ),
    
    legend.position = "right",
    
    legend.title = element_text(
      face = "bold"
    ),
    
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    
    panel.grid.minor = element_blank()
    
  )

# =====================================================
# Exibir gráfico
# =====================================================

print(grafico_graduados)

# =====================================================
# Salvar gráfico
# =====================================================

ggsave(
  
  file.path(
    pasta_graficos,
    "figura_graduados_percentual.png"
  ),
  
  grafico_graduados,
  
  width = 11,
  
  height = 6,
  
  dpi = 300
  
)

cat("\n=====================================\n")
cat("ARQUIVOS GERADOS\n")
cat("=====================================\n")

cat(
  "\nTabela:",
  "\n- tabela_graduados_percentual.csv\n"
)

cat(
  "\nGráfico:",
  "\n- figura_graduados_percentual.png\n"
)

cat("\nAnálise concluída.\n")

