# =====================================================
# evasao_periodo_exato.R
# Análise da evasão por período exato
# =====================================================

library(readr)
library(dplyr)
library(ggplot2)

# =====================================================
# Diretórios
# =====================================================

projeto <- "C:/Users/Big Data/Documents/Master UFCG/Semestre 2026.1/ufcg-evasao-impacto-curriculo/Dados_Script_Usados"

pasta_processados <- file.path(projeto, "dados_processados")
pasta_resultados  <- file.path(projeto, "resultados")
pasta_tabelas     <- file.path(pasta_resultados, "tabelas")

dir.create(
  pasta_tabelas,
  recursive = TRUE,
  showWarnings = FALSE
)

# =====================================================
# Carregar amostra
# =====================================================

dados <- read_csv2(
  file.path(
    pasta_processados,
    "amostra_final_dissertacao.csv"
  ),
  show_col_types = FALSE
)

# =====================================================
# Verificar variáveis
# =====================================================

cat("\n=============================\n")
cat("VARIÁVEIS DA BASE\n")
cat("=============================\n")

print(names(dados))

# =====================================================
# Tratamento do período de evasão
# =====================================================

dados <- dados %>%
  
  mutate(
    
    `Periodo de Evasao` = ifelse(
      
      `Periodo de Evasao`=="-",
      
      NA,
      
      `Periodo de Evasao`
      
    ),
    
    Periodo_Evasao_Num = as.numeric(
      
      ifelse(
        
        is.na(`Periodo de Evasao`),
        
        NA,
        
        gsub(
          "\\.",
          "",
          `Periodo de Evasao`
        )
        
      )
      
    )
    
  )

# =====================================================
# Função para converter semestre em índice
# =====================================================

codigo_semestre <- function(x){
  
  ano <- x %/% 10
  
  semestre <- x %% 10
  
  (ano - 2011) * 2 + semestre
  
}

# =====================================================
# Criar período relativo
# =====================================================

dados <- dados %>%
  
  mutate(
    
    indice_ingresso =
      
      codigo_semestre(
        `Periodo de Ingresso`
      ),
    
    indice_evasao =
      
      ifelse(
        
        is.na(Periodo_Evasao_Num),
        
        NA,
        
        codigo_semestre(
          Periodo_Evasao_Num
        )
        
      ),
    
    periodo_relativo =
      
      indice_evasao -
      indice_ingresso
    
  )

# =====================================================
# Conferência
# =====================================================

cat("\n=============================\n")
cat("PERÍODOS RELATIVOS\n")
cat("=============================\n")

print(
  
  dados %>%
    
    filter(
      !is.na(periodo_relativo)
    ) %>%
    
    count(periodo_relativo)
  
)

# =====================================================
# Ingressantes por coorte
# =====================================================

ingressantes <- dados %>%
  
  group_by(
    
    `Curriculo Entrada`,
    
    `Periodo de Ingresso`
    
  ) %>%
  
  summarise(
    
    Ingressantes = n(),
    
    .groups="drop"
    
  )

# =====================================================
# Função de cálculo
# =====================================================
calcular_periodo <- function(periodo){
  
  # Conversão:
  # P1 -> 0
  # P2 -> 1
  # P3 -> 2
  # P4 -> 3
  
  periodo_relativo_desejado <- periodo - 1
  
  evadidos <- dados %>%
    filter(periodo_relativo == periodo_relativo_desejado) %>%
    group_by(
      `Curriculo Entrada`,
      `Periodo de Ingresso`
    ) %>%
    summarise(
      Evadidos = n(),
      .groups = "drop"
    )
  
  tabela <- ingressantes %>%
    left_join(
      evadidos,
      by = c(
        "Curriculo Entrada",
        "Periodo de Ingresso"
      )
    ) %>%
    mutate(
      Evadidos = coalesce(Evadidos, 0L),
      Taxa = round(
        100 * Evadidos / Ingressantes,
        2
      )
    ) %>%
    arrange(
      `Curriculo Entrada`,
      `Periodo de Ingresso`
    )
  
  return(tabela)
  
}
################################################################################
# =====================================================
# Gerar tabelas
# =====================================================

for(i in 1:4){
  
  tabela <- calcular_periodo(i)
  
  cat("\n====================================\n")
  
  cat(
    
    "EVASÃO -",
    
    i,
    
    "º PERÍODO\n"
    
  )
  
  cat("====================================\n")
  
  print(tabela)
  
  write_csv2(
    
    tabela,
    
    file.path(
      
      pasta_tabelas,
      
      paste0(
        
        "evasao_periodo",
        
        i,
        
        ".csv"
        
      )
      
    )
    
  )
  
}





###############################################################################

# =====================================================
# Pasta dos gráficos
# =====================================================

pasta_graficos <- file.path(
  pasta_resultados,
  "graficos"
)

dir.create(
  pasta_graficos,
  recursive = TRUE,
  showWarnings = FALSE
)

# =====================================================
# Formatar período
# =====================================================

formatar_periodo <- function(x){
  
  x <- as.character(x)
  
  paste0(
    substr(x,1,4),
    ".",
    substr(x,5,5)
  )
  
}

# =====================================================
# Função para gerar gráfico
# =====================================================

gerar_grafico <- function(tabela, periodo){
  
  tabela_plot <- tabela %>%
    
    mutate(
      
      Periodo = formatar_periodo(`Periodo de Ingresso`),
      
      Curriculo = factor(
        `Curriculo Entrada`,
        levels = c(1999,2017),
        labels = c(
          "Currículo 1999",
          "Currículo 2017"
        )
      )
      
    )
  
  g <- ggplot(
    
    tabela_plot,
    
    aes(
      x = Periodo,
      y = Taxa,
      colour = Curriculo,
      group = Curriculo
    )
    
  ) +
    
    geom_line(
      linewidth = 1
    ) +
    
    geom_point(
      size = 3
    ) +
    
    geom_text(
      
      aes(
        label = sprintf("%.2f%%",Taxa)
      ),
      
      vjust = -0.7,
      
      size = 3
      
    ) +
    
    scale_colour_manual(
      
      values = c(
        "Currículo 1999" = "#1F77B4",
        "Currículo 2017" = "#D62728"
      )
      
    ) +
    
    labs(
      
      title = paste0(
        "Taxa de evasão no ",
        periodo,
        "º período"
      ),
      
      x = "Período de ingresso",
      
      y = "Taxa de evasão (%)",
      
      colour = "Currículo"
      
    ) +
    
    expand_limits(y = 0) +
    
    theme_classic(base_size = 13) +
    
    theme(
      
      plot.title = element_text(
        face = "bold",
        hjust = .5
      ),
      
      legend.position = "top",
      
      axis.text.x = element_text(
        angle = 45,
        hjust = 1
      )
      
    )
  
  print(g)
  
  ggsave(
    
    filename = file.path(
      
      pasta_graficos,
      
      paste0(
        "figura_5_",
        periodo,
        "_evasao.png"
      )
      
    ),
    
    plot = g,
    
    width = 11,
    
    height = 6,
    
    dpi = 300
    
  )
  
}


# =====================================================
# Gerar tabelas e gráficos
# =====================================================

lista_tabelas <- list()

for(i in 1:4){
  
  tabela <- calcular_periodo(i)
  
  lista_tabelas[[paste0("Periodo_",i)]] <- tabela
  
  cat("\n====================================\n")
  cat("EVASÃO -",i,"º PERÍODO\n")
  cat("====================================\n")
  
  print(tabela)
  
  write_csv2(
    
    tabela,
    
    file.path(
      
      pasta_tabelas,
      
      paste0(
        "evasao_periodo",
        i,
        ".csv"
      )
      
    )
    
  )
  
  gerar_grafico(
    tabela,
    i
  )
  
}

# =====================================================
# Tabela consolidada
# =====================================================

tabela_geral <- bind_rows(

  lista_tabelas,

  .id = "Periodo"

)

write_csv2(

  tabela_geral,

  file.path(

    pasta_tabelas,

    "evasao_todos_periodos.csv"

  )

)

cat("\n=========================================\n")
cat("PROCESSAMENTO CONCLUÍDO\n")
cat("=========================================\n")

cat("\nTabelas:\n")
cat(pasta_tabelas,"\n")

cat("\nGráficos:\n")
cat(pasta_graficos,"\n")


tabela4 <- calcular_periodo(4)

print(tabela4)

gerar_grafico(tabela4,4)
