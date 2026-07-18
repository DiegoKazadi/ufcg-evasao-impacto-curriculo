#=========================================================
# DISSERTAÇÃO - CAPÍTULO 5
# Testes estatísticos das variáveis acadêmicas
# Tabela 5.19
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
# Manter apenas estudantes evadidos
#=========================================================

dados <- dados %>%
  filter(Status == "INATIVO")

#=========================================================
# Verificar variáveis
#=========================================================

variaveis <- c(
  "Curriculo Entrada",
  "Tipo de Evasao"
)

faltando <- setdiff(variaveis, names(dados))

if(length(faltando) > 0){
  
  stop(
    paste(
      "Variáveis não encontradas:",
      paste(faltando, collapse = ", ")
    )
  )
  
}

cat("\nBase carregada com sucesso.\n")
cat("Número de estudantes evadidos:", nrow(dados), "\n")

#=========================================================
# Remover registros sem Tipo de Evasão
#=========================================================

dados <- dados %>%
  filter(!is.na(`Tipo de Evasao`))

#=========================================================
# Tabela de contingência
#=========================================================

tabela <- table(
  
  dados$`Tipo de Evasao`,
  
  dados$`Curriculo Entrada`
  
)

#=========================================================
# Teste estatístico
#=========================================================

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

#=========================================================
# V de Cramér
#=========================================================

cramers <- cramersV(tabela)

#=========================================================
# Tabela 5.19
#=========================================================

tabela_5_19 <- tibble(
  
  Variavel = "Tipo de evasão",
  
  Teste = nome_teste,
  
  Estatistica = round(estatistica,3),
  
  p_valor = ifelse(
    pvalor < 0.001,
    "<0,001",
    format(
      round(pvalor,4),
      decimal.mark = ",",
      nsmall = 4
    )
  ),
  
  V_Cramer = round(cramers,3),
  
  Conclusao = ifelse(
    pvalor < 0.05,
    "Diferença significativa",
    "Sem diferença significativa"
  )
  
)

print(tabela_5_19)

#=========================================================
# Exportar
#=========================================================

write.csv2(
  
  tabela_5_19,
  
  file.path(
    
    pasta_tabelas,
    
    "Tabela_5_19_Testes_Academicos.csv"
    
  ),
  
  row.names = FALSE
  
)

cat("\nTabela 5.19 gerada com sucesso.\n")