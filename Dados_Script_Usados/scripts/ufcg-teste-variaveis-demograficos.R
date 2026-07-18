#=========================================================
# DISSERTAÇÃO - CAPÍTULO 5
# Testes estatísticos das variáveis demográficas
# Tabela 5.18
#=========================================================

install.packages("lsr")
rm(list = ls())

library(readr)
library(dplyr)
library(tidyr)
library(rstatix)
library(lsr)

options(scipen = 999)

#=========================================================
# Diretórios
#=========================================================

projeto <- "C:/Users/Big Data/Documents/Master UFCG/Semestre 2026.1/ufcg-evasao-impacto-curriculo/Dados_Script_Usados"

pasta_processados <- file.path(projeto, "dados_processados")
pasta_resultados  <- file.path(projeto, "resultados")
pasta_tabelas     <- file.path(pasta_resultados, "tabelas")

dir.create(
  pasta_tabelas,
  recursive = TRUE,
  showWarnings = FALSE
)

#=========================================================
# Carregar amostra
#=========================================================

dados <- read_csv2(
  file.path(
    pasta_processados,
    "amostra_final_dissertacao.csv"
  ),
  show_col_types = FALSE
)

#=========================================================
# Variável Evadiu
#=========================================================

dados <- dados %>%
  mutate(
    Evadiu = ifelse(Status == "INATIVO",
                    "Sim",
                    "Não")
  )

dados$Evadiu <- factor(
  dados$Evadiu,
  levels = c("Não","Sim")
)

#=========================================================
# Criar Faixa Etária
#=========================================================

dados <- dados %>%
  mutate(
    faixa_etaria = case_when(
      
      `Idade Aproximada no Ingresso` <= 17 ~ "≤17",
      
      `Idade Aproximada no Ingresso` >=18 &
        `Idade Aproximada no Ingresso` <=20 ~ "18–20",
      
      `Idade Aproximada no Ingresso` >=21 &
        `Idade Aproximada no Ingresso` <=24 ~ "21–24",
      
      `Idade Aproximada no Ingresso` >=25 ~ "≥25",
      
      TRUE ~ NA_character_
      
    )
  )

#=========================================================
# Função dos testes
#=========================================================

executar_teste <- function(variavel){
  
  tabela <- table(
    dados[[variavel]],
    dados$Evadiu
  )
  
  qui <- suppressWarnings(
    chisq.test(tabela)
  )
  
  if(any(qui$expected < 5)){
    
    teste <- fisher.test(tabela)
    
    nome_teste <- "Fisher"
    
    estatistica <- NA
    
    pvalor <- teste$p.value
    
  }else{
    
    nome_teste <- "Qui-quadrado"
    
    estatistica <- unname(qui$statistic)
    
    pvalor <- qui$p.value
    
  }
  
  cramers <- cramersV(tabela)
  
  tibble(
    
    Variavel = variavel,
    
    Teste = nome_teste,
    
    Estatistica = round(estatistica,3),
    
    p_valor = ifelse(
      pvalor < 0.001,
      "<0,001",
      format(round(pvalor,4),
             decimal.mark = ",",
             nsmall = 4)
    ),
    
    V_Cramer = round(cramers,3),
    
    Conclusao = ifelse(
      pvalor < 0.05,
      "Diferença significativa",
      "Sem diferença significativa"
    )
    
  )
  
}

#=========================================================
# Executar testes
#=========================================================

tabela_5_18 <- bind_rows(
  
  executar_teste("Sexo"),
  
  executar_teste("faixa_etaria")
  
)

print(tabela_5_18)

#=========================================================
# Exportar
#=========================================================

write.csv2(
  
  tabela_5_18,
  
  file.path(
    
    pasta_tabelas,
    
    "Tabela_5_18_Testes_Demograficos.csv"
    
  ),
  
  row.names = FALSE
  
)

cat("\nTabela 5.18 gerada com sucesso.\n")

