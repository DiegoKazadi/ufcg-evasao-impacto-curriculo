# =====================================================
# evasao_periodo_exato_sexo.R
# Análise da evasão por período exato segundo Sexo
# =====================================================

library(readr)
library(dplyr)
library(ggplot2)

# =====================================================
# Diretórios
# =====================================================

projeto <- "C:/Users/Big Data/Documents/Master UFCG/Semestre 2026.1/ufcg-evasao-impacto-curriculo/Dados_Script_Usados"

pasta_processados <- file.path(projeto, "dados_processados")
pasta_resultados  <- file.path(projeto, "resultados")
pasta_tabelas     <- file.path(pasta_resultados, "tabelas")

dir.create(pasta_tabelas, recursive = TRUE, showWarnings = FALSE)

# =====================================================
# Carregar amostra
# =====================================================

dados <- read_csv2(
  file.path(pasta_processados, "amostra_final_dissertacao.csv"),
  show_col_types = FALSE
)

# =====================================================
# Tratamento do período de evasão
# =====================================================

dados <- dados %>%
  mutate(
    `Periodo de Evasao` = ifelse(`Periodo de Evasao` == "-", NA, `Periodo de Evasao`),
    Periodo_Evasao_Num = as.numeric(
      ifelse(
        is.na(`Periodo de Evasao`),
        NA,
        gsub("\\.", "", `Periodo de Evasao`)
      )
    )
  )

codigo_semestre <- function(x){
  ano <- x %/% 10
  semestre <- x %% 10
  (ano - 2011) * 2 + semestre
}

dados <- dados %>%
  mutate(
    indice_ingresso = codigo_semestre(`Periodo de Ingresso`),
    indice_evasao = ifelse(
      is.na(Periodo_Evasao_Num),
      NA,
      codigo_semestre(Periodo_Evasao_Num)
    ),
    periodo_relativo = indice_evasao - indice_ingresso
  )

# =====================================================
# Ingressantes por periodo evasao
# =====================================================

ingressantes <- dados %>%
  group_by(
    `Curriculo Entrada`,
    `Periodo de Ingresso`,
    `Periodo de Evasao`
  ) %>%
  summarise(
    Ingressantes = n(),
    .groups = "drop"
  )

calcular_periodo <- function(periodo){
  
  periodo_relativo_desejado <- periodo - 1
  
  evadidos <- dados %>%
    filter(periodo_relativo == periodo_relativo_desejado) %>%
    group_by(
      `Curriculo Entrada`,
      `Periodo de Ingresso`,
      
    ) %>%
    summarise(
      Evadidos = n(),
      .groups = "drop"
    )
  
  ingressantes %>%
    left_join(
      evadidos,
      by = c(
        "Curriculo Entrada",
        "Periodo de Ingresso"
      )
    ) %>%
    mutate(
      Evadidos = coalesce(Evadidos, 0L),
      Taxa = round(100 * Evadidos / Ingressantes, 2)
    ) %>%
    arrange(
      `Curriculo Entrada`,
      `Periodo de Ingresso`,
     
    )
}

lista_tabelas <- list()

for(i in 1:4){
  
  tabela <- calcular_periodo(i)
  
  lista_tabelas[[paste0("Periodo_", i)]] <- tabela
  
  print(tabela)
  
  write_csv2(
    tabela,
    file.path(
      pasta_tabelas,
      paste0("evasao_sexo_periodo", i, ".csv")
    )
  )
}

# =====================================================
# Gráficos
# =====================================================

pasta_graficos <- file.path(pasta_resultados, "graficos")
dir.create(pasta_graficos, recursive = TRUE, showWarnings = FALSE)

formatar_periodo <- function(x){
  x <- as.character(x)
  paste0(substr(x,1,4),".",substr(x,5,5))
}

gerar_grafico <- function(tabela, periodo){
  
  tabela_plot <- tabela %>%
    mutate(
      Periodo = formatar_periodo(`Periodo de Ingresso`),
      Curriculo = factor(
        `Curriculo Entrada`,
        levels = c(1999,2017),
        labels = c("Currículo 1999","Currículo 2017")
      )
    )
  
  g <- ggplot(
    tabela_plot,
    aes(
      x = Periodo,
      y = Taxa,
      fill = Sexo
    )
  ) +
    geom_col(
      position=position_dodge(width=0.70),
      width=0.70,
      colour="black",
      linewidth=0.2
    ) +
    geom_text(
      aes(label=ifelse(Taxa==0,"",sprintf("%.1f",Taxa))),
      position=position_dodge(width=0.70),
      vjust=-0.35,
      size=3,
      show.legend=FALSE
    ) +
    scale_fill_manual(
      values=c(
        "FEMININO"="#F8766D",
        "MASCULINO"="#00BFC4"
      )
    ) +
    scale_y_continuous(
      expand=expansion(mult=c(0,0.10))
    ) +
    labs(
      title=paste("Taxa de evasão por sexo -", periodo, "º período"),
      x="Período de ingresso",
      y="Taxa de evasão (%)"
    ) +
    theme_classic(base_size=13) +
    theme(
      legend.position="top",
      axis.text.x=element_text(angle=45,hjust=1),
      plot.title=element_text(face="bold",hjust=.5)
    )
  
  ggsave(
    filename=file.path(
      pasta_graficos,
      paste0("figura_sexo_p", periodo, ".png")
    ),
    plot=g,
    width=11,
    height=7,
    dpi=300
  )
}

for(i in seq_along(lista_tabelas)){
  gerar_grafico(lista_tabelas[[i]], i)
}

tabela_geral <- bind_rows(lista_tabelas, .id="Periodo")

write_csv2(
  tabela_geral,
  file.path(
    pasta_tabelas,
    "evasao_sexo_todos_periodos.csv"
  )
)

cat("Processamento concluído.\n")

colnames(dados)
