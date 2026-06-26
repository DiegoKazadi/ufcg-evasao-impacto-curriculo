# =====================================================
# 05_evasao_periodo_exato.R
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

dir.create(pasta_tabelas,
           recursive = TRUE,
           showWarnings = FALSE)

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
# EVASÃO NO 1º PERÍODO
# =====================================================

evadidos_p1 <- dados %>%
  filter(
    !is.na(`Período de Evasão`)
  ) %>%
  mutate(
    diferenca =
      (`Período de Evasão` - `Período de Ingresso`)
  ) %>%
  filter(
    diferenca == 0.1
  ) %>%
  group_by(
    `Currículo Entrada`,
    `Período de Ingresso`
  ) %>%
  summarise(
    Evadidos_P1 = n(),
    .groups = "drop"
  )

# =====================================================
# Junta resultados
# =====================================================

tabela_p1 <- ingressantes %>%
  left_join(
    evadidos_p1,
    by = c(
      "Currículo Entrada",
      "Período de Ingresso"
    )
  ) %>%
  mutate(
    Evadidos_P1 =
      ifelse(
        is.na(Evadidos_P1),
        0,
        Evadidos_P1
      ),
    
    Taxa_P1 =
      round(
        100 * Evadidos_P1 / Ingressantes,
        2
      )
  ) %>%
  arrange(
    `Currículo Entrada`,
    `Período de Ingresso`
  )

# =====================================================
# Console
# =====================================================

cat("\n=====================================\n")
cat("EVASÃO - 1º PERÍODO\n")
cat("=====================================\n")

print(tabela_p1)

# =====================================================
# Salvar
# =====================================================

write_csv(
  tabela_p1,
  file.path(
    pasta_tabelas,
    "evasao_periodo1.csv"
  )
)