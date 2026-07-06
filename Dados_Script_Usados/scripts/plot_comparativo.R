
# Script R para comparar os currículos 1999 e 2017
library(ggplot2)
library(dplyr)

# Dados (carregando do CSV ou dataframe)
data <- read.csv("dados_evasao.csv")

# Converter a coluna de 'Curriculo' para fator para facilitar o agrupamento
data$Curriculo.Entrada <- as.factor(data$Curriculo.Entrada)

# Gráfico de Taxa de Evasão por Período de Ingresso, comparando os currículos
ggplot(data, aes(x = as.factor(Periodo.de.Ingresso), y = Taxa, color = Curriculo.Entrada, group = Curriculo.Entrada)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(title = "Comparação da Taxa de Evasão: Currículos 1999 vs 2017",
       x = "Período de Ingresso",
       y = "Taxa de Evasão (%)",
       color = "Currículo") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
