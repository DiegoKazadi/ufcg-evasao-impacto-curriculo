# =====================================================
# graficos_evasao_periodo_exato.R
# Figuras 5.1 a 5.4
# =====================================================

library(readr)
library(dplyr)
library(ggplot2)

# =====================================================
# Diretórios
# =====================================================

projeto <- "C:/Users/Big Data/Documents/Master UFCG/Semestre 2026.1/ufcg-evasao-impacto-curriculo/Dados_Script_Usados"

pasta_tabelas <- file.path(
  projeto,
  "resultados",
  "tabelas"
)

pasta_graficos <- file.path(
  projeto,
  "resultados",
  "graficos"
)

dir.create(
  pasta_graficos,
  recursive = TRUE,
  showWarnings = FALSE
)

# =====================================================
# Função para formatar o período
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

gerar_grafico <- function(
    arquivo,
    titulo,
    nome_figura
){
  
  dados <- read_csv(
    file.path(
      pasta_tabelas,
      arquivo
    ),
    show_col_types = FALSE
  )
  
  dados <- dados %>%
    
    mutate(
      
      Periodo = formatar_periodo(
        `Periodo de Ingresso`
      ),
      
      Curriculo = factor(
        `Curriculo Entrada`,
        levels = c(1999,2017),
        labels = c(
          "Currículo 1999",
          "Currículo 2017"
        )
      )
      
    )
  
  grafico <- ggplot(
    
    dados,
    
    aes(
      
      x = Periodo,
      
      y = Taxa,
      
      group = Curriculo,
      
      color = Curriculo
      
    )
    
  ) +
    
    geom_line(
      
      linewidth = 1
      
    ) +
    
    geom_point(
      
      size = 2.8
      
    ) +
    
    geom_text(
      
      aes(
        label = sprintf("%.1f",Taxa)
      ),
      
      vjust = -0.7,
      
      size = 3
      
    ) +
    
    scale_color_manual(
      
      values = c(
        
        "Currículo 1999" = "#1F77B4",
        
        "Currículo 2017" = "#D62728"
        
      )
      
    ) +
    
    labs(
      
      title = titulo,
      
      x = "Período de ingresso",
      
      y = "Taxa de evasão (%)",
      
      color = "Currículo"
      
    ) +
    
    expand_limits(y = 0) +
    
    theme_classic() +
    
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
  
  print(grafico)
  
  ggsave(
    
    file.path(
      
      pasta_graficos,
      
      nome_figura
      
    ),
    
    grafico,
    
    width = 10,
    
    height = 5.8,
    
    dpi = 300
    
  )
  
}

# =====================================================
# Figura 5.1
# =====================================================

gerar_grafico(
  
  "evasao_periodo1.csv",
  
  "Taxa de evasão no 1º período por coorte de ingresso",
  
  "figura_5_1_evasao_periodo1.png"
  
)

# =====================================================
# Figura 5.2
# =====================================================

gerar_grafico(
  
  "evasao_periodo2.csv",
  
  "Taxa de evasão no 2º período por coorte de ingresso",
  
  "figura_5_2_evasao_periodo2.png"
  
)

# =====================================================
# Figura 5.3
# =====================================================

gerar_grafico(
  
  "evasao_periodo3.csv",
  
  "Taxa de evasão no 3º período por coorte de ingresso",
  
  "figura_5_3_evasao_periodo3.png"
  
)

# =====================================================
# Figura 5.4
# =====================================================

gerar_grafico(
  
  "evasao_periodo4.csv",
  
  "Taxa de evasão no 4º período por coorte de ingresso",
  
  "figura_5_4_evasao_periodo4.png"
  
)

# =====================================================
# Resumo
# =====================================================

cat("\n=====================================\n")
cat("GRÁFICOS GERADOS\n")
cat("=====================================\n")

cat(
  
  "\n- figura_5_1_evasao_periodo1.png",
  "\n- figura_5_2_evasao_periodo2.png",
  "\n- figura_5_3_evasao_periodo3.png",
  "\n- figura_5_4_evasao_periodo4.png\n"
  
)

cat("\nProcesso concluído com sucesso.\n")