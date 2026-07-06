# =====================================================
# evasao_periodo_exato.R
# Análise da evasão por período exato
# Versão refatorada
# =====================================================

library(readr)
library(dplyr)
library(ggplot2)
library(openxlsx)

# =====================================================
# Diretórios
# =====================================================

projeto <- "C:/Users/Big Data/Documents/Master UFCG/Semestre 2026.1/ufcg-evasao-impacto-curriculo/Dados_Script_Usados"

pasta_processados <- file.path(projeto, "dados_processados")
pasta_resultados  <- file.path(projeto, "resultados")
pasta_tabelas     <- file.path(pasta_resultados, "tabelas")
pasta_graficos    <- file.path(pasta_resultados, "graficos")

invisible(lapply(
  c(pasta_resultados, pasta_tabelas, pasta_graficos),
  dir.create,
  recursive = TRUE,
  showWarnings = FALSE
))

# =====================================================
# Leitura
# =====================================================

dados <- read_csv2(
  file.path(pasta_processados, "amostra_final_dissertacao.csv"),
  show_col_types = FALSE
)

# =====================================================
# Tratamento
# =====================================================

codigo_semestre <- function(x){
  ano <- x %/% 10
  semestre <- x %% 10
  (ano - 2011) * 2 + semestre
}

dados <- dados %>%
  mutate(
    `Periodo de Evasao` = na_if(`Periodo de Evasao`, "-"),
    Periodo_Evasao_Num = as.numeric(gsub("\\.", "", `Periodo de Evasao`)),
    indice_ingresso = codigo_semestre(`Periodo de Ingresso`),
    indice_evasao = ifelse(
      is.na(Periodo_Evasao_Num),
      NA,
      codigo_semestre(Periodo_Evasao_Num)
    ),
    periodo_relativo = indice_evasao - indice_ingresso
  )

# =====================================================
# Ingressantes
# =====================================================

ingressantes <- dados %>%
  group_by(`Curriculo Entrada`, `Periodo de Ingresso`) %>%
  summarise(Ingressantes = n(), .groups = "drop")

# =====================================================
# Funções
# =====================================================

salvar_csv <- function(df, arquivo){

  tryCatch({

    if(file.exists(arquivo))
      file.remove(arquivo)

    write_csv2(df, arquivo)

  }, error = function(e){

    message("Erro ao salvar: ", arquivo)
    message(e$message)

  })

}

formatar_periodo <- function(x){
  x <- as.character(x)
  paste0(substr(x,1,4),".",substr(x,5,5))
}

calcular_periodo <- function(periodo){

  evadidos <- dados %>%
    filter(periodo_relativo == periodo) %>%
    group_by(`Curriculo Entrada`, `Periodo de Ingresso`) %>%
    summarise(Evadidos=n(), .groups="drop")

  ingressantes %>%
    left_join(
      evadidos,
      by=c("Curriculo Entrada","Periodo de Ingresso")
    ) %>%
    mutate(
      Evadidos = coalesce(Evadidos,0L),
      Taxa = round(Evadidos*100/Ingressantes,2)
    ) %>%
    arrange(`Curriculo Entrada`,`Periodo de Ingresso`)
}

salvar_grafico <- function(df, periodo){

  df <- df %>%
    mutate(
      Curriculo=factor(`Curriculo Entrada`,levels=c("1999","2017")),
      Periodo=formatar_periodo(`Periodo de Ingresso`)
    )

  g <- ggplot(df,
              aes(x=Periodo,y=Taxa,fill=Curriculo))+
    geom_col(position="dodge",width=.75)+
    geom_text(aes(label=sprintf("%.2f%%",Taxa)),
              position=position_dodge(.75),
              vjust=-0.25,size=3)+
    scale_fill_manual(values=c("#1F77B4","#D62728"),
                      labels=c("Currículo 1999","Currículo 2017"))+
    labs(
      title=paste0("Taxa de evasão - ",periodo,"º período"),
      x="Período de ingresso",
      y="Taxa (%)",
      fill="Currículo"
    )+
    theme_bw(base_size=12)+
    theme(
      plot.title=element_text(face="bold",hjust=.5),
      axis.text.x=element_text(angle=45,hjust=1)
    )

  ggsave(
    filename=file.path(
      pasta_graficos,
      paste0("evasao_periodo_",periodo,".png")
    ),
    plot=g,
    width=11,
    height=5.5,
    dpi=300
  )

}

# =====================================================
# Processamento
# =====================================================

lista_tabelas <- list()

for(i in 1:4){

  tabela <- calcular_periodo(i)

  lista_tabelas[[paste0("Periodo_",i)]] <- tabela

  salvar_csv(
    tabela,
    file.path(
      pasta_tabelas,
      paste0("evasao_periodo",i,".csv")
    )
  )

  salvar_grafico(tabela,i)

}

# =====================================================
# Consolidado
# =====================================================

tabela_geral <- bind_rows(
  lista_tabelas,
  .id="Periodo"
)

salvar_csv(
  tabela_geral,
  file.path(
    pasta_tabelas,
    "evasao_todos_periodos.csv"
  )
)

write.xlsx(
  lista_tabelas,
  file.path(
    pasta_tabelas,
    "Tabelas_Evasao.xlsx"
  ),
  overwrite=TRUE
)

cat("\n===================================\n")
cat("PROCESSAMENTO CONCLUÍDO\n")
cat("===================================\n")
cat("Tabelas:", pasta_tabelas,"\n")
cat("Gráficos:", pasta_graficos,"\n")
