# =====================================================
# evasao_periodo_exato.R
# Evasão por período exato
# =====================================================

library(readr)
library(dplyr)

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

dados <- read_csv(
  file.path(
    pasta_processados,
    "amostra_final_dissertacao.csv"
  ),
  show_col_types = FALSE
)

# =====================================================
# Tratamento dos períodos
# =====================================================

dados <- dados %>%
  
  mutate(
    
    `Período de Evasão` = ifelse(
      `Período de Evasão` == "-",
      NA,
      `Período de Evasão`
    ),
    
    `Período de Evasão` =
      as.numeric(`Período de Evasão`)
    
  )

# =====================================================
# Criar índice sequencial dos semestres
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

indice_periodos <- data.frame(
  
  Periodo = periodos,
  
  Indice = seq_along(periodos)
  
)

# =====================================================
# Associar índice aos períodos
# =====================================================

dados <- dados %>%
  
  left_join(
    
    indice_periodos,
    
    by = c(
      "Período de Ingresso" = "Periodo"
    )
    
  ) %>%
  
  rename(
    indice_ingresso = Indice
  ) %>%
  
  left_join(
    
    indice_periodos,
    
    by = c(
      "Período de Evasão" = "Periodo"
    )
    
  ) %>%
  
  rename(
    indice_evasao = Indice
  )

# =====================================================
# Ingressantes por coorte
# =====================================================

ingressantes <- dados %>%
  
  group_by(
    
    `Currículo Entrada`,
    
    `Período de Ingresso`
    
  ) %>%
  
  summarise(
    
    Ingressantes = n(),
    
    .groups = "drop"
    
  )

# =====================================================
# Função para calcular evasão por período
# =====================================================

calcular_periodo <- function(periodo){
  
  evadidos <- dados %>%
    
    filter(
      
      !is.na(indice_evasao)
      
    ) %>%
    
    filter(
      
      indice_evasao - indice_ingresso == periodo
      
    ) %>%
    
    group_by(
      
      `Currículo Entrada`,
      
      `Período de Ingresso`
      
    ) %>%
    
    summarise(
      
      Evadidos = n(),
      
      .groups = "drop"
      
    )
  
  tabela <- ingressantes %>%
    
    left_join(
      
      evadidos,
      
      by = c(
        
        "Currículo Entrada",
        
        "Período de Ingresso"
        
      )
      
    ) %>%
    
    mutate(
      
      Evadidos = ifelse(
        
        is.na(Evadidos),
        
        0,
        
        Evadidos
        
      ),
      
      Taxa = round(
        
        100 * Evadidos / Ingressantes,
        
        2
        
      )
      
    ) %>%
    
    arrange(
      
      `Currículo Entrada`,
      
      `Período de Ingresso`
      
    )
  
  return(tabela)
  
}

# =====================================================
# Período 1
# =====================================================

tabela_p1 <- calcular_periodo(1)

cat("\n=====================================\n")
cat("EVASÃO - 1º PERÍODO\n")
cat("=====================================\n")

print(tabela_p1)

write_csv(
  
  tabela_p1,
  
  file.path(
    
    pasta_tabelas,
    
    "evasao_periodo1.csv"
    
  )
  
)

# =====================================================
# Período 2
# =====================================================

tabela_p2 <- calcular_periodo(2)

cat("\n=====================================\n")
cat("EVASÃO - 2º PERÍODO\n")
cat("=====================================\n")

print(tabela_p2)

write_csv(
  
  tabela_p2,
  
  file.path(
    
    pasta_tabelas,
    
    "evasao_periodo2.csv"
    
  )
  
)

# =====================================================
# Período 3
# =====================================================

tabela_p3 <- calcular_periodo(3)

cat("\n=====================================\n")
cat("EVASÃO - 3º PERÍODO\n")
cat("=====================================\n")

print(tabela_p3)

write_csv(
  
  tabela_p3,
  
  file.path(
    
    pasta_tabelas,
    
    "evasao_periodo3.csv"
    
  )
  
)

# =====================================================
# Período 4
# =====================================================

tabela_p4 <- calcular_periodo(4)

cat("\n=====================================\n")
cat("EVASÃO - 4º PERÍODO\n")
cat("=====================================\n")

print(tabela_p4)

write_csv(
  
  tabela_p4,
  
  file.path(
    
    pasta_tabelas,
    
    "evasao_periodo4.csv"
    
  )
  
)

# =====================================================
# Resumo
# =====================================================

cat("\n=====================================\n")
cat("ARQUIVOS GERADOS\n")
cat("=====================================\n")

cat(
  "\n- evasao_periodo1.csv",
  "\n- evasao_periodo2.csv",
  "\n- evasao_periodo3.csv",
  "\n- evasao_periodo4.csv\n"
)
