# =====================================================
# evasao_periodo_exato.R
# Análise da evasão por período exato
# Versão refatorada
# =====================================================

library(readr)
library(dplyr)
library(ggplot2)

projeto <- "C:/Users/Big Data/Documents/Master UFCG/Semestre 2026.1/ufcg-evasao-impacto-curriculo/Dados_Script_Usados"

pasta_processados <- file.path(projeto,"dados_processados")
pasta_resultados  <- file.path(projeto,"resultados")
pasta_tabelas     <- file.path(pasta_resultados,"tabelas")
pasta_graficos    <- file.path(pasta_resultados,"graficos")

dir.create(pasta_tabelas,recursive=TRUE,showWarnings=FALSE)
dir.create(pasta_graficos,recursive=TRUE,showWarnings=FALSE)

dados <- read_csv2(
  file.path(pasta_processados,"amostra_final_dissertacao.csv"),
  show_col_types=FALSE
)

dados <- dados %>%
  mutate(
    `Periodo de Evasao`=ifelse(`Periodo de Evasao`=="-",NA,`Periodo de Evasao`),
    Periodo_Evasao_Num=as.numeric(ifelse(is.na(`Periodo de Evasao`),NA,gsub("\\.","",`Periodo de Evasao`)))
  )

codigo_semestre <- function(x){
  ano <- x %/% 10
  semestre <- x %% 10
  (ano-2011)*2+semestre
}

dados <- dados %>%
 mutate(
  indice_ingresso=codigo_semestre(`Periodo de Ingresso`),
  indice_evasao=ifelse(is.na(Periodo_Evasao_Num),NA,codigo_semestre(Periodo_Evasao_Num)),
  periodo_relativo=indice_evasao-indice_ingresso
 )

ingressantes <- dados %>%
 group_by(`Curriculo Entrada`,`Periodo de Ingresso`) %>%
 summarise(Ingressantes=n(),.groups="drop")

calcular_periodo <- function(periodo){
 evadidos <- dados %>%
  filter(periodo_relativo==periodo) %>%
  group_by(`Curriculo Entrada`,`Periodo de Ingresso`) %>%
  summarise(Evadidos=n(),.groups="drop")

 ingressantes %>%
  left_join(evadidos,by=c("Curriculo Entrada","Periodo de Ingresso")) %>%
  mutate(Evadidos=ifelse(is.na(Evadidos),0,Evadidos),
         Taxa=round(100*Evadidos/Ingressantes,2)) %>%
  arrange(`Curriculo Entrada`,`Periodo de Ingresso`)
}

formatar_periodo <- function(x){
 x <- as.character(x)
 paste0(substr(x,1,4),".",substr(x,5,5))
}

gerar_grafico <- function(tabela,periodo){

 tabela <- tabela %>%
  mutate(
   Periodo=formatar_periodo(`Periodo de Ingresso`),
   Curriculo=factor(`Curriculo Entrada`,
                    levels=c(1999,2017),
                    labels=c("Currículo 1999","Currículo 2017"))
  )

 g <- ggplot(
   tabela,
   aes(x=Periodo,y=Taxa,group=Curriculo,colour=Curriculo)
 )+
 geom_line(linewidth=1)+
 geom_point(size=2.8)+
 geom_text(aes(label=sprintf("%.1f",Taxa)),vjust=-0.7,size=3)+
 scale_colour_manual(values=c("Currículo 1999"="#1F77B4","Currículo 2017"="#D62728"))+
 labs(title=paste("Taxa de evasão no",periodo,"º período"),
      x="Período de ingresso",
      y="Taxa de evasão (%)",
      colour="Currículo")+
 expand_limits(y=0)+
 theme_classic()+
 theme(plot.title=element_text(face="bold",hjust=.5),
       legend.position="top",
       axis.text.x=element_text(angle=45,hjust=1))

 print(g)

 ggsave(file.path(pasta_graficos,
 paste0("figura_5_",periodo,"_evasao_periodo",periodo,".png")),
 g,width=10,height=5.8,dpi=300)
}

for(i in 1:4){

 tabela <- calcular_periodo(i)

 print(tabela)

 write_csv(
   tabela,
   file.path(
    pasta_tabelas,
    paste0("evasao_periodo",i,".csv")
   )
 )

 gerar_grafico(tabela,i)

}

cat("\nProcessamento concluído.\n")



# Teste
dados %>%
  filter(`Periodo de Ingresso` == 20111) %>%
  count(periodo_relativo)
