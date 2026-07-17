# ============================================================
# analise_variaveis.R
# Análise automática das variáveis demográficas e acadêmicas
# ============================================================

library(readr)
library(dplyr)
library(ggplot2)

projeto <- "C:/Users/Big Data/Documents/Master UFCG/Semestre 2026.1/ufcg-evasao-impacto-curriculo/Dados_Script_Usados"

pasta_processados <- file.path(projeto,"dados_processados")
pasta_resultados  <- file.path(projeto,"resultados")

dados <- read_csv2(
 file.path(pasta_processados,"amostra_final_dissertacao.csv"),
 show_col_types=FALSE
)

dados$`Periodo de Evasao`[dados$`Periodo de Evasao`=="-"] <- NA

dados$Periodo_Evasao_Num <- as.numeric(
 ifelse(is.na(dados$`Periodo de Evasao`),NA,
 gsub("\\.","",dados$`Periodo de Evasao`))
)

codigo_semestre <- function(x){
 ano <- x %/% 10
 semestre <- x %% 10
 (ano-2011)*2+semestre
}

dados$indice_ingresso <- codigo_semestre(dados$`Periodo de Ingresso`)
dados$indice_evasao <- ifelse(
 is.na(dados$Periodo_Evasao_Num),
 NA,
 codigo_semestre(dados$Periodo_Evasao_Num)
)

dados$periodo_relativo <- dados$indice_evasao-dados$indice_ingresso

variaveis <- list(
 list(secao="demograficas",
      coluna="Sexo",
      arquivo="sexo",
      titulo="Sexo"),
 list(secao="demograficas",
      coluna="Idade Aproximada no Ingresso",
      arquivo="idade",
      titulo="Faixa Etária"),
 list(secao="academicas",
      coluna="Periodo de Evasao",
      arquivo="periodo_evasao",
      titulo="Período de Evasão"),
 list(secao="academicas",
      coluna="Tipo de Evasao",
      arquivo="tipo_evasao",
      titulo="Tipo de Evasão")
)

formatar_periodo <- function(x){
 x<-as.character(x)
 paste0(substr(x,1,4),".",substr(x,5,5))
}



processar_variavel <- function(info){

variavel <- info$coluna

pasta_tab <- file.path(pasta_resultados,"tabelas",info$arquivo)
pasta_gra <- file.path(pasta_resultados,"graficos",info$arquivo)

dir.create(pasta_tab,recursive=TRUE,showWarnings=FALSE)
dir.create(pasta_gra,recursive=TRUE,showWarnings=FALSE)

ingressantes <- dados |>
 group_by(`Curriculo Entrada`,`Periodo de Ingresso`,.data[[variavel]]) |>
 summarise(Ingressantes=n(),.groups="drop")

lista <- list()

for(i in 1:4){

evadidos <- dados |>
 filter(periodo_relativo==(i-1)) |>
 group_by(`Curriculo Entrada`,`Periodo de Ingresso`,.data[[variavel]]) |>
 summarise(Evadidos=n(),.groups="drop")

tab <- ingressantes |>
 left_join(evadidos,
 by=c("Curriculo Entrada","Periodo de Ingresso",variavel)) |>
 mutate(Evadidos=coalesce(Evadidos,0L),
 Taxa=round(100*Evadidos/Ingressantes,2))

lista[[paste0("P",i)]] <- tab

write_csv2(tab,
 file.path(pasta_tab,
 paste0(info$arquivo,"_periodo",i,".csv")))

plot <- tab |>
 mutate(
 Periodo=formatar_periodo(`Periodo de Ingresso`),
 Curriculo=factor(`Curriculo Entrada`,
 levels=c(1999,2017),
 labels=c("Currículo 1999","Currículo 2017"))
 ) |>
 ggplot(aes(Periodo,Taxa,fill=.data[[variavel]]))+
 geom_col(position="dodge",colour="black",linewidth=.2)+
 geom_text(aes(label=sprintf("%.1f",Taxa)),
 position=position_dodge(.9),
 vjust=-.35,size=3)+
 facet_wrap(~Curriculo,ncol=1)+
 labs(
 title=paste("Taxa de evasão por",info$titulo,"-",i,"º período"),
 x="Período de ingresso",
 y="Taxa de evasão (%)",
 fill=info$titulo)+
 theme_classic()+
 theme(
 legend.position="top",
 axis.text.x=element_text(angle=45,hjust=1),
 plot.title=element_text(face="bold",hjust=.5))

ggsave(
 file.path(pasta_gra,
 paste0("figura_",info$arquivo,"_p",i,".png")),
 plot,
 width=11,
 height=7,
 dpi=300)

}

write_csv2(
 bind_rows(lista,.id="Periodo"),
 file.path(
 pasta_tab,
 paste0(info$arquivo,"_todos_periodos.csv"))
)

cat("\n===================================\n")
cat("Variável:",info$titulo,"\n")
cat("Concluída com sucesso.\n")
cat("===================================\n")

}

for(v in variaveis){
 processar_variavel(v)
}

cat("\nPROCESSAMENTO FINALIZADO.\n")

colnames(dados)
