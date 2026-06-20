# =====================================================
# anexos_curriculos.R
#
# Objetivo:
# Organizar e documentar as matrizes curriculares
# utilizadas na dissertação.
#
# Gera os anexos curriculares e estatísticas
# resumidas dos currículos de 1999 e 2017.
# =====================================================

library(readr)
library(dplyr)

# =====================================================
# Diretórios
# =====================================================

projeto <- "C:/Users/Big Data/Documents/Master UFCG/Semestre 2026.1/ufcg-evasao-impacto-curriculo/Dados_Script_Usados"

pasta_dados <- file.path(projeto, "dados")

pasta_resultados <- file.path(projeto, "resultados")

pasta_tabelas <- file.path(
  pasta_resultados,
  "tabelas"
)

dir.create(
  pasta_tabelas,
  recursive = TRUE,
  showWarnings = FALSE
)

# =====================================================
# Leitura dos currículos
# =====================================================

curriculo_1999 <- read_csv2(
  file.path(
    pasta_dados,
    "disciplinas_curriculo_1999.csv"
  ),
  show_col_types = FALSE
)

curriculo_2017 <- read_csv2(
  file.path(
    pasta_dados,
    "disciplinas_curriculo_2017.csv"
  ),
  show_col_types = FALSE
)
# =====================================================
# Estrutura
# =====================================================

cat("\n=================================\n")
cat("CURRÍCULO 1999\n")
cat("=================================\n")

print(names(curriculo_1999))

cat(
  "\nTotal de componentes:",
  nrow(curriculo_1999),
  "\n"
)

cat("\n=================================\n")
cat("CURRÍCULO 2017\n")
cat("=================================\n")

print(names(curriculo_2017))

cat(
  "\nTotal de componentes:",
  nrow(curriculo_2017),
  "\n"
)

# =====================================================
# Salvar cópia para anexos
# =====================================================

write_csv(
  curriculo_1999,
  file.path(
    pasta_tabelas,
    "anexo_curriculo_1999.csv"
  )
)

write_csv(
  curriculo_2017,
  file.path(
    pasta_tabelas,
    "anexo_curriculo_2017.csv"
  )
)

# =====================================================
# Estatísticas gerais
# =====================================================

resumo_1999 <- data.frame(
  Curriculo = "1999",
  Componentes = nrow(curriculo_1999)
)

resumo_2017 <- data.frame(
  Curriculo = "2017",
  Componentes = nrow(curriculo_2017)
)

resumo <- bind_rows(
  resumo_1999,
  resumo_2017
)

print(resumo)

write_csv(
  resumo,
  file.path(
    pasta_tabelas,
    "resumo_curriculos.csv"
  )
)

# =====================================================
# Resumo textual
# =====================================================

texto <- c(
  
  "=====================================",
  "RESUMO DOS CURRICULOS",
  "=====================================",
  "",
  
  paste(
    "Curriculo 1999:",
    nrow(curriculo_1999),
    "componentes curriculares"
  ),
  
  paste(
    "Curriculo 2017:",
    nrow(curriculo_2017),
    "componentes curriculares"
  )
  
)

writeLines(
  texto,
  file.path(
    pasta_resultados,
    "resumo_curriculos.txt"
  )
)

cat("\nArquivos gerados:\n")

cat(
  "\n- anexo_curriculo_1999.csv"
)

cat(
  "\n- anexo_curriculo_2017.csv"
)

cat(
  "\n- resumo_curriculos.csv"
)

cat(
  "\n- resumo_curriculos.txt\n"
)

names(curriculo_1999)

names(curriculo_2017)

head(curriculo_1999)

head(curriculo_2017)

###
# As tabelas

table(curriculo_1999$TIPO)

table(curriculo_2017$TIPO)

table(curriculo_1999$SEMESTRE_IDEAL)
table(curriculo_2017$SEMESTRE_IDEAL)

# Verificar as variaveis
curriculo_1999 %>%
  filter(
    TIPO == "OBRIGATORIO",
    SEMESTRE_IDEAL > 0
  ) %>%
  select(
    NOME_DISCIPLINA,
    SEMESTRE_IDEAL
  ) %>%
  arrange(
    SEMESTRE_IDEAL,
    NOME_DISCIPLINA
  )

curriculo_2017 %>%
  filter(
    TIPO == "OBRIGATORIO",
    SEMESTRE_IDEAL > 0
  ) %>%
  select(
    NOME_DISCIPLINA,
    SEMESTRE_IDEAL
  ) %>%
  arrange(
    SEMESTRE_IDEAL,
    NOME_DISCIPLINA
  )
