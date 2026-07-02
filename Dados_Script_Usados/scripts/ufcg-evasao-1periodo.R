# =====================================================
# evasao_primeiro_periodo.R
# Validação da Tabela do 1º Período
# =====================================================

library(readr)
library(dplyr)

# =====================================================
# Diretórios
# =====================================================

projeto <- "C:/Users/Big Data/Documents/Master UFCG/Semestre 2026.1/ufcg-evasao-impacto-curriculo/Dados_Script_Usados"

pasta_processados <- file.path(projeto,"dados_processados")

pasta_resultados <- file.path(projeto,"resultados")

pasta_tabelas <- file.path(pasta_resultados,"tabelas")

dir.create(
  pasta_tabelas,
  recursive = TRUE,
  showWarnings = FALSE
)

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
# Preparação da base
# =====================================================

dados <- dados %>%
  
  mutate(
    
    `Período de Evasão` =
      ifelse(
        `Período de Evasão`=="-",
        NA,
        `Período de Evasão`
      ),
    
    `Período de Evasão` =
      as.numeric(`Período de Evasão`)
    
  )

# =====================================================
# Índice dos períodos
# =====================================================

periodos <- sort(
  
  unique(
    
    c(
      
      dados$`Período de Ingresso`,
      
      dados$`Período de Evasão`
      
    )
    
  ),
  
  na.last = TRUE
  
)

indice <- tibble(
  
  Periodo = periodos,
  
  indice = seq_along(periodos)
  
)

dados <- dados %>%
  
  left_join(
    
    indice,
    
    by = c(
      "Período de Ingresso"="Periodo"
    )
    
  ) %>%
  
  rename(
    indice_ingresso = indice
  ) %>%
  
  left_join(
    
    indice,
    
    by = c(
      "Período de Evasão"="Periodo"
    )
    
  ) %>%
  
  rename(
    indice_evasao = indice
  ) %>%
  
  mutate(
    
    periodo_relativo =
      indice_evasao -
      indice_ingresso
    
  )

# =====================================================
# Janela utilizada na dissertação
# =====================================================

dados_p1 <- dados %>%
  
  filter(
    
    (`Currículo Entrada`==1999 &
       `Período de Ingresso`>=2011.1 &
       `Período de Ingresso`<=2015.2)
    
    |
      
      (`Currículo Entrada`==2017 &
         `Período de Ingresso`>=2018.1 &
         `Período de Ingresso`<=2022.2)
    
  )

# =====================================================
# Conferência da amostra
# =====================================================

cat("\n===============================\n")
cat("AMOSTRA\n")
cat("===============================\n")

print(
  
  dados_p1 %>%
    
    count(`Currículo Entrada`)
  
)

# Esperado:
# 1999 = 854
# 2017 = 918

# =====================================================
# Conferência dos períodos relativos
# =====================================================

cat("\n===============================\n")
cat("PERÍODO RELATIVO\n")
cat("===============================\n")

print(
  
  dados_p1 %>%
    
    filter(
      
      Status=="INATIVO",
      
      `Tipo de Evasão`!="GRADUADO"
      
    ) %>%
    
    count(periodo_relativo)
  
)

# =====================================================
# Ingressantes
# =====================================================

ingressantes <- dados_p1 %>%
  
  group_by(
    
    `Currículo Entrada`,
    
    `Período de Ingresso`
    
  ) %>%
  
  summarise(
    
    Ingressantes=n(),
    
    .groups="drop"
    
  )

# =====================================================
# Evadidos exatamente no 1º período
# =====================================================

evadidos_p1 <- dados_p1 %>%
  
  filter(
    
    Status=="INATIVO",
    
    `Tipo de Evasão`!="GRADUADO"
    
  ) %>%
  
  filter(
    
    periodo_relativo==1
    
  ) %>%
  
  group_by(
    
    `Currículo Entrada`,
    
    `Período de Ingresso`
    
  ) %>%
  
  summarise(
    
    Evadidos=n(),
    
    .groups="drop"
    
  )

# =====================================================
# Junta resultados
# =====================================================

tabela_p1 <- ingressantes %>%
  
  left_join(
    
    evadidos_p1,
    
    by=c(
      
      "Currículo Entrada",
      
      "Período de Ingresso"
      
    )
    
  ) %>%
  
  mutate(
    
    Evadidos=
      
      ifelse(
        
        is.na(Evadidos),
        
        0,
        
        Evadidos
        
      ),
    
    Taxa=
      
      round(
        
        100*Evadidos/Ingressantes,
        
        2
        
      )
    
  ) %>%
  
  arrange(
    
    `Currículo Entrada`,
    
    `Período de Ingresso`
    
  )

# =====================================================
# Resultado
# =====================================================

cat("\n===============================\n")
cat("EVASÃO NO 1º PERÍODO\n")
cat("===============================\n")

print(tabela_p1)

write_csv(
  
  tabela_p1,
  
  file.path(
    
    pasta_tabelas,
    
    "tabela_periodo1.csv"
    
  )
  
)

cat("\nTabela salva com sucesso.\n")