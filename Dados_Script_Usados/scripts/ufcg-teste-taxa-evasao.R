#=========================================================
# DISSERTAÇÃO - CAPÍTULO 5
# Testes estatísticos das taxas de evasão
# Tabela 5.16
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

dir.create(pasta_tabelas,
           recursive = TRUE,
           showWarnings = FALSE)

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
# Criar variável Evadiu
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
# Verificação das variáveis
#=========================================================

variaveis <- c(
  "Curriculo Entrada",
  "periodo_relativo",
  "Evadiu"
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
cat("Número de estudantes:", nrow(dados), "\n")

# ====

dados %>%
  count(`Tipo de Evasao`) %>%
  print()

#=========================================================
# Padronizar Período de Ingresso
#=========================================================

dados$`Periodo de Ingresso` <-
  as.character(dados$`Periodo de Ingresso`)

dados$`Periodo de Ingresso` <-
  paste0(
    substr(dados$`Periodo de Ingresso`, 1, 4),
    ".",
    substr(dados$`Periodo de Ingresso`, 5, 5)
  )

head(dados$`Periodo de Ingresso`)

#=========================================================
# Converter semestre em índice
#=========================================================

converter_periodo <- function(x){
  
  x <- as.character(x)
  
  ano <- as.numeric(substr(x, 1, 4))
  semestre <- as.numeric(substr(x, 6, 6))
  
  ano * 2 + semestre - 1
  
}

names(dados)

# =====================================================
# Criar as variaveis
# =====================================================

dados$indice_ingresso <-
  converter_periodo(dados$`Periodo de Ingresso`)

dados$indice_evasao <-
  ifelse(
    is.na(dados$`Periodo de Evasao`) |
      dados$`Periodo de Evasao` == "",
    NA,
    converter_periodo(dados$`Periodo de Evasao`)
  )

dados$periodo_relativo <-
  dados$indice_evasao -
  dados$indice_ingresso +
  1


# =====================================================
# Verificação
# =====================================================

summary(dados$periodo_relativo)

sort(unique(na.omit(dados$periodo_relativo)))

sort(unique(dados$periodo_relativo))

head(dados$`Periodo de Ingresso`, 10)

names(dados)



#=========================================================
# Função para executar o teste estatístico
#=========================================================

executar_teste <- function(periodo){
  
  base <- dados %>%
    mutate(
      
      Evadiu_Periodo = case_when(
        
        Evadiu == "Sim" &
          periodo_relativo <= periodo ~ "Sim",
        
        TRUE ~ "Não"
        
      )
      
    )
  
  tabela <- table(
    base$`Curriculo Entrada`,
    base$Evadiu_Periodo
  )
  
  teste_chi <- suppressWarnings(chisq.test(tabela))
  
  if(any(teste_chi$expected < 5)){
    
    teste <- fisher.test(tabela)
    
    nome_teste <- "Fisher"
    
    chisq <- NA
    
    pvalor <- teste$p.value
    
  }else{
    
    teste <- teste_chi
    
    nome_teste <- "Qui-quadrado"
    
    chisq <- unname(teste$statistic)
    
    pvalor <- teste$p.value
    
  }
  
  v <- suppressWarnings(lsr::cramersV(tabela))
  
  conclusao <- ifelse(
    
    pvalor < 0.05,
    
    "Diferença significativa",
    
    "Diferença não significativa"
    
  )
  
  data.frame(
    
    Periodo = paste0(periodo,"º"),
    
    Teste = nome_teste,
    
    Qui_Quadrado = round(chisq,3),
    
    p_valor = round(pvalor,4),
    
    V_Cramer = round(v,3),
    
    Conclusao = conclusao,
    
    stringsAsFactors = FALSE
    
  )
  
}

# ==========================================================
# Gerar as Tabelas
# ==========================================================

tabela_5_16 <-
  bind_rows(
    
    lapply(
      1:4,
      executar_teste
    )
    
  )

tabela_5_16


# =========================================================
# Salvar Tabela
# =========================================================

write.csv2(
  
  tabela_5_16,
  
  file.path(
    
    pasta_tabelas,
    
    "Tabela_5_16_Teste_Taxa_Evasao.csv"
    
  ),
  
  row.names = FALSE
  
)


#=========================================================
# Tabela 5.16
# Distribuição da amostra utilizada nos testes
#=========================================================
tabela_5_16 <- dados %>%
  group_by(`Curriculo Entrada`) %>%
  summarise(
    Ingressantes = n(),
    Ativos = sum(Evadiu == "Não"),
    Evadidos = sum(Evadiu == "Sim"),
    .groups = "drop"
  )

tabela_total <- tibble(
  `Curriculo Entrada` = "Total",
  Ingressantes = sum(tabela_5_16$Ingressantes),
  Ativos = sum(tabela_5_16$Ativos),
  Evadidos = sum(tabela_5_16$Evadidos)
)

tabela_5_16 <- bind_rows(
  tabela_5_16,
  tabela_total
)

# ======================================================
# Visualizar resultado
# ======================================================

print(tabela_5_16)

write.csv2(
  tabela_5_16,
  file.path(
    pasta_tabelas,
    "Tabela_5_16_Distribuicao_Amostra_Testes.csv"
  ),
  row.names = FALSE
)




#=========================================================
# Verificação da base
#=========================================================

cat("\n========================================\n")
cat("VARIÁVEIS DA BASE\n")
cat("========================================\n")

print(names(dados))

cat("\n========================================\n")
cat("TOTAL DE ESTUDANTES\n")
cat("========================================\n")

cat(nrow(dados), "\n")

cat("\n========================================\n")
cat("INGRESSANTES POR CURRÍCULO\n")
cat("========================================\n")

dados %>%
  count(`Curriculo Entrada`) %>%
  print()

cat("\n========================================\n")
cat("STATUS DOS ESTUDANTES\n")
cat("========================================\n")

dados %>%
  count(Status) %>%
  print()

cat("\n========================================\n")
cat("TIPO DE EVASÃO\n")
cat("========================================\n")
s