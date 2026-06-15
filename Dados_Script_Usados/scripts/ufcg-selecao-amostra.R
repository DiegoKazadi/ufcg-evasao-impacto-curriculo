# 03_selecao_amostra.R
# Seleção da amostra final da dissertação

library(readr)
library(dplyr)

# Diretório dos dados

pasta_dados <- "C:/Users/Big Data/Documents/Master UFCG/Semestre 2026.1/ufcg-evasao-impacto-curriculo/Dados_Script_Usados/dados"

# Carregar dados

alunos <- read_csv2(
  file.path(pasta_dados, "alunos-final.csv"),
  show_col_types = FALSE
)

# Ajustar nomes das colunas

names(alunos) <- trimws(names(alunos))

# Exclusões

tipos_excluir <- c(
  "INGRESSANTE NAO FEZ 1ª MATRICULA",
  "NAO COMPARECEU AO REMANEJAMENTO",
  "NAO COMPARECEU CADASTRO"
)

# Seleção da amostra

amostra_teste <- alunos %>%
  filter(
    (Curriculo == 1999 &
       `Periodo de Ingresso` >= 20111 &
       `Periodo de Ingresso` <= 20152) |
      
      (Curriculo == 2017 &
         `Periodo de Ingresso` >= 20181 &
         `Periodo de Ingresso` <= 20222)
  )

table(amostra_teste$Curriculo)

# Verificações

cat("\n=====================================\n")
cat("TOTAL DA AMOSTRA\n")
cat("=====================================\n")

n_total <- nrow(amostra_final)

cat("Total:", n_total, "\n")

cat("\n=====================================\n")
cat("POR CURRÍCULO\n")
cat("=====================================\n")

table(amostra_final$Curriculo)

cat("\n=====================================\n")
cat("PERÍODOS DE INGRESSO\n")
cat("=====================================\n")

table(amostra_final$`Periodo de Ingresso`)

# Salvar amostra utilizada na dissertação

write_csv(
  amostra_final,
  file.path(
    pasta_dados,
    "amostra_final_dissertacao.csv"
  )
)

cat("\nArquivo salvo com sucesso!\n")

unique(alunos$`Periodo de Ingresso`)
sort(unique(alunos$`Periodo de Ingresso`))
str(alunos$`Periodo de Ingresso`)
table(alunos$Curriculo)

###

library(dplyr)

alunos %>%
  count(Curriculo, `Periodo de Ingresso`) %>%
  arrange(Curriculo, `Periodo de Ingresso`)

names(alunos)

names(alunos)[3]
names(alunos)[15]

table(alunos[[15]])
