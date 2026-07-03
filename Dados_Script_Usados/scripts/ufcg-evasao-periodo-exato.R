# =====================================================
# evasao_periodo_exato.R
# Análise da evasão por período exato
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
  
  evadidos <- dados %>%
    
    filter(
      
      periodo_relativo == periodo
      
    ) %>%
    
    group_by(
      
      `Curriculo Entrada`,
      
      `Periodo de Ingresso`
      
    ) %>%
    
    summarise(
      
      Evadidos = n(),
      
      .groups="drop"
      
    )
  
  tabela <- ingressantes %>%
    
    left_join(
      
      evadidos,
      
      by=c(
        
        "Curriculo Entrada",
        
        "Periodo de Ingresso"
        
      )
      
    ) %>%
    
    mutate(
      
      Evadidos = ifelse(
        
        is.na(Evadidos),
        
        0,
        
        Evadidos
        
      ),
      
      Taxa = round(
        
        100 *
          Evadidos /
          Ingressantes,
        
        2
        
      )
      
    ) %>%
    
    arrange(
      
      `Curriculo Entrada`,
      
      `Periodo de Ingresso`
      
    )
  
  return(tabela)
  
}

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
  
  write_csv(
    
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

# =====================================================
# Resumo
# =====================================================

cat("\n====================================\n")
cat("ARQUIVOS GERADOS\n")
cat("====================================\n")

cat(
  
  "\n- evasao_periodo1.csv",
  "\n- evasao_periodo2.csv",
  "\n- evasao_periodo3.csv",
  "\n- evasao_periodo4.csv\n"
  
)
