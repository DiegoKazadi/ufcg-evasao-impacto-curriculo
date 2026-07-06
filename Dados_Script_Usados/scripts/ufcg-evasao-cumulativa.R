# =====================================================
# evasao_cumulativa.R
# Análise da evasão cumulativa por período
# Baseado em evasao_periodo_exato.R
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
pasta_graficos    <- file.path(pasta_resultados, "graficos")

dir.create(pasta_tabelas, recursive=TRUE, showWarnings=FALSE)
dir.create(pasta_graficos, recursive=TRUE, showWarnings=FALSE)

dados <- read_csv2(
  file.path(pasta_processados,"amostra_final_dissertacao.csv"),
  show_col_types = FALSE
)

dados <- dados %>%
  mutate(
    `Periodo de Evasao` = ifelse(`Periodo de Evasao`=="-", NA, `Periodo de Evasao`),
    Periodo_Evasao_Num = as.numeric(
      ifelse(is.na(`Periodo de Evasao`), NA,
             gsub("\\.","",`Periodo de Evasao`))
    )
  )

codigo_semestre <- function(x){
  ano <- x %/% 10
  semestre <- x %% 10
  (ano-2011)*2 + semestre
}

dados <- dados %>%
  mutate(
    indice_ingresso = codigo_semestre(`Periodo de Ingresso`),
    indice_evasao   = ifelse(is.na(Periodo_Evasao_Num),NA,
                             codigo_semestre(Periodo_Evasao_Num)),
    periodo_relativo = indice_evasao - indice_ingresso
  )

formatar_periodo <- function(x){
  x <- as.character(x)
  paste0(substr(x,1,4),".",substr(x,5,5))
}

# -----------------------------------------------------
# Janelas de comparação
# -----------------------------------------------------

janelas <- list(
  `1` = list(`1999`=c(20111,20172), `2017`=c(20181,20231)),
  `2` = list(`1999`=c(20111,20161), `2017`=c(20181,20222)),
  `3` = list(`1999`=c(20111,20152), `2017`=c(20181,20221)),
  `4` = list(`1999`=c(20111,20142), `2017`=c(20181,20212))
)

filtrar_janela <- function(base, periodo){

  j <- janelas[[as.character(periodo)]]

  bind_rows(
    base %>%
      filter(`Curriculo Entrada`==1999,
             `Periodo de Ingresso`>=j$`1999`[1],
             `Periodo de Ingresso`<=j$`1999`[2]),
    base %>%
      filter(`Curriculo Entrada`==2017,
             `Periodo de Ingresso`>=j$`2017`[1],
             `Periodo de Ingresso`<=j$`2017`[2])
  )
}

calcular_periodo_cumulativo <- function(periodo){

  base <- filtrar_janela(dados, periodo)

  ingressantes <- base %>%
    group_by(`Curriculo Entrada`,`Periodo de Ingresso`) %>%
    summarise(Ingressantes=n(), .groups="drop")

  evadidos <- base %>%
    filter(periodo_relativo>=0,
           periodo_relativo <= (periodo-1)) %>%
    group_by(`Curriculo Entrada`,`Periodo de Ingresso`) %>%
    summarise(Evadidos=n(), .groups="drop")

  ingressantes %>%
    left_join(evadidos,
              by=c("Curriculo Entrada","Periodo de Ingresso")) %>%
    mutate(
      Evadidos=coalesce(Evadidos,0L),
      Taxa=round(100*Evadidos/Ingressantes,2)
    ) %>%
    arrange(`Curriculo Entrada`,`Periodo de Ingresso`)
}

gerar_grafico <- function(tabela, periodo){

  tabela_plot <- tabela %>%
    mutate(
      Periodo=formatar_periodo(`Periodo de Ingresso`),
      Curriculo=factor(`Curriculo Entrada`,
                       levels=c(1999,2017),
                       labels=c("Currículo 1999","Currículo 2017"))
    )

  g <- ggplot(tabela_plot,
              aes(x=Periodo,y=Taxa,fill=Curriculo))+
    geom_col(position="dodge", colour="black", linewidth=.2)+
    geom_text(aes(label=sprintf("%.2f",Taxa)),
              position=position_dodge(.9),
              vjust=-.35,size=3)+
    scale_fill_manual(values=c("#1F77B4","#D62728"))+
    labs(title=paste0("Taxa de evasão cumulativa até o ",periodo,"º período"),
         x="Período de ingresso",
         y="Taxa (%)",
         fill="Currículo")+
    theme_classic()+
    theme(plot.title=element_text(face="bold",hjust=.5),
          legend.position="top",
          axis.text.x=element_text(angle=45,hjust=1))

  ggsave(
    file.path(pasta_graficos,
              paste0("figura_5_",periodo,"_evasao_cumulativa.png")),
    g,width=11,height=6,dpi=300,bg="white"
  )
}

lista_tabelas <- list()

for(i in 1:4){

  cat("\n=================================\n")
  cat("EVASÃO CUMULATIVA ATÉ O",i,"º PERÍODO\n")
  cat("=================================\n")

  tabela <- calcular_periodo_cumulativo(i)

  print(tabela)

  lista_tabelas[[paste0("Periodo_",i)]] <- tabela

  write_csv2(
    tabela,
    file.path(
      pasta_tabelas,
      paste0("evasao_cumulativa_periodo",i,".csv")
    )
  )

  gerar_grafico(tabela,i)

  totais <- tabela %>%
    group_by(`Curriculo Entrada`) %>%
    summarise(
      Ingressantes=sum(Ingressantes),
      Evadidos=sum(Evadidos),
      Taxa=round(100*Evadidos/Ingressantes,2),
      .groups="drop"
    )

  cat("\nTotais do período:\n")
  print(totais)

}

tabela_geral <- bind_rows(lista_tabelas,.id="Periodo")

write_csv2(
  tabela_geral,
  file.path(
    pasta_tabelas,
    "evasao_cumulativa_todos_periodos.csv"
  )
)

cat("\n=====================================\n")
cat("PROCESSAMENTO CONCLUÍDO\n")
cat("=====================================\n")
cat("\nCompare os resultados com a planilha do Prof. Fubica.\n")
cat("Diferenças esperadas: apenas arredondamento (±0,01).\n")
