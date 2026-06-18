library(readr)
library(dplyr)

# Diretório dos dados
# Certifique-se de que o arquivo 'alunos_processados.csv' esteja nesta pasta.
pasta_dados <- "C:/Users/Big Data/Documents/Master UFCG/Semestre 2026.1/ufcg-evasao-impacto-curriculo/Dados_Script_Usados/dados"

# Caminho completo para o arquivo CSV
caminho_arquivo_csv <- file.path(pasta_dados, "alunos-1.csv")

# Lendo o CSV com read_delim para maior controle sobre o delimitador e locale.
# Usamos delim = "," para indicar que a vírgula é o separador de colunas.
# Usamos locale = locale(decimal_mark = ".", grouping_mark = "") para garantir que o ponto seja o separador decimal
# e que não haja separador de milhares, evitando interpretações erradas.
dados <- read_delim(
  caminho_arquivo_csv, 
  delim = ",", 
  locale = locale(decimal_mark = ".", grouping_mark = ""),
  show_col_types = FALSE
)

# Verificando as primeiras linhas e nomes das colunas para garantir que a leitura foi correta
print(head(dados))
print(colnames(dados))

tabela_1999 <- dados %>%
  filter(
    `Currículo Entrada` == 1999,
    `Período de Ingresso` >= 2011.1,
    `Período de Ingresso` <= 2015.2
  ) %>%
  group_by(`Período de Ingresso`) %>%
  summarise(
    Ingressantes = sum(Contagem)
  )

tabela_2017 <- dados %>%
  filter(
    `Currículo Entrada` == 2017,
    `Período de Ingresso` >= 2018.1,
    `Período de Ingresso` <= 2022.2
  ) %>%
  group_by(`Período de Ingresso`) %>%
  summarise(
    Ingressantes = sum(Contagem)
  )

# Opcional: Visualizar as tabelas resultantes
print("Tabela 1999:")
print(tabela_1999)
print("Tabela 2017:")
print(tabela_2017)
