# =====================================================
# 13_graduados_percentual.R
# Distribuição dos estudantes graduados
# =====================================================

library(readr)
library(dplyr)
library(ggplot2)
library(scales)

# =====================================================
# Diretórios
# =====================================================

projeto <- "C:/Users/Big Data/Documents/Master UFCG/Semestre 2026.1/ufcg-evasao-impacto-curriculo/Dados_Script_Usados"

pasta_processados <- file.path(projeto,"dados_processados")
pasta_resultados <- file.path(projeto,"resultados")
pasta_tabelas <- file.path(pasta_resultados,"tabelas")
pasta_graficos <- file.path(pasta_resultados,"graficos")

# =====================================================
# Carregar dados
# =====================================================

dados <- read_csv(
  file.path(
    pasta_processados,
    "amostra_final_dissertacao.csv"
  ),
  show_col_types = FALSE
)

# =====================================================
# Ingressantes por coorte
# =====================================================

ingressantes <- dados %>%
  count(
    `Currículo Entrada`,
    `Período de Ingresso`,
    name="Ingressantes"
  )

# =====================================================
# Graduados
# =====================================================

graduados <- dados %>%
  filter(`Tipo de Evasão`=="GRADUADO") %>%
  count(
    `Currículo Entrada`,
    `Período de Ingresso`,
    name="Graduados"
  )

# =====================================================
# Tabela final
# =====================================================

tabela <- ingressantes %>%
  
  left_join(
    graduados,
    by=c(
      "Currículo Entrada",
      "Período de Ingresso"
    )
  ) %>%
  
  mutate(
    
    Graduados=ifelse(
      is.na(Graduados),
      0,
      Graduados
    ),
    
    Percentual=round(
      Graduados/Ingressantes*100,
      1
    )
    
  )

print(tabela)

write_csv(
  tabela,
  file.path(
    pasta_tabelas,
    "tabela_graduados_percentual.csv"
  )
)

# =====================================================
# Gráfico
# =====================================================

grafico <- ggplot(
  
  tabela,
  
  aes(
    
    factor(`Período de Ingresso`),
    
    Percentual,
    
    fill=factor(`Currículo Entrada`)
    
  )
  
)+
  
  geom_col(
    position=position_dodge(0.8),
    width=.7
  )+
  
  geom_text(
    
    aes(
      label=paste0(
        Percentual,"%"
      )
    ),
    
    position=position_dodge(.8),
    
    vjust=-0.25,
    
    size=3
    
  )+
  
  scale_fill_manual(
    
    values=c(
      "#1F77B4",
      "#D62728"
    ),
    
    labels=c(
      "Currículo 1999",
      "Currículo 2017"
    ),
    
    name="Currículo"
    
  )+
  
  labs(
    
    title="Percentual de estudantes graduados por coorte de ingresso",
    
    x="Período de ingresso",
    
    y="% de graduados"
    
  )+
  
  theme_minimal()+
  
  theme(
    
    plot.title=element_text(
      face="bold",
      hjust=.5
    ),
    
    legend.position="right",
    
    axis.text.x=element_text(
      angle=45,
      hjust=1
    )
    
  )

print(grafico)

ggsave(
  
  file.path(
    
    pasta_graficos,
    
    "figura_graduados_percentual.png"
    
  ),
  
  grafico,
  
  width=10,
  
  height=6,
  
  dpi=300
  
)

# =====================================================
# Resumo
# =====================================================

cat("\n====================================\n")

cat("GRADUADOS POR COORTE\n")

cat("====================================\n")

print(tabela)