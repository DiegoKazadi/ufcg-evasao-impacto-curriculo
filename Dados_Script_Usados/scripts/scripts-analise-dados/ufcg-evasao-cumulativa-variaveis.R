# =====================================================
# evasao_cumulativa_variaveis.R
library(readr)
library(dplyr)

projeto <- "C:/Users/Big Data/Documents/Master UFCG/Semestre 2026.1/ufcg-evasao-impacto-curriculo/Dados_Script_Usados"
pasta_processados <- file.path(projeto,"dados_processados")
pasta_resultados <- file.path(projeto,"resultados")
pasta_tabelas <- file.path(pasta_resultados,"tabelas")
dir.create(pasta_tabelas,recursive=TRUE,showWarnings=FALSE)

dados <- read_csv2(file.path(pasta_processados,"amostra_final_dissertacao.csv"),show_col_types=FALSE)

dados <- dados %>%
mutate(`Periodo de Evasao`=ifelse(`Periodo de Evasao`=="-",NA,`Periodo de Evasao`),
Periodo_Evasao_Num=as.numeric(ifelse(is.na(`Periodo de Evasao`),NA,gsub("\\.","",`Periodo de Evasao`))))

codigo_semestre <- function(x){
 ano <- x %/% 10
 semestre <- x %% 10
 (ano-2011)*2+semestre
}

dados <- dados %>%
mutate(indice_ingresso=codigo_semestre(`Periodo de Ingresso`),
indice_evasao=ifelse(is.na(Periodo_Evasao_Num),NA,codigo_semestre(Periodo_Evasao_Num)),
periodo_relativo=indice_evasao-indice_ingresso)

base <- bind_rows(
dados %>% filter(`Curriculo Entrada`==1999,`Periodo de Ingresso`>=20111,`Periodo de Ingresso`<=20142),
dados %>% filter(`Curriculo Entrada`==2017,`Periodo de Ingresso`>=20181,`Periodo de Ingresso`<=20212))

evadidos <- base %>% filter(periodo_relativo>=0,periodo_relativo<=3)

gerar_tabela <- function(variavel,nome){

ing <- base %>% group_by(`Curriculo Entrada`, .data[[variavel]]) %>%
summarise(Ingressantes=n(),.groups="drop")

eva <- evadidos %>% group_by(`Curriculo Entrada`, .data[[variavel]]) %>%
summarise(Evadidos=n(),.groups="drop")

tab <- ing %>%
left_join(eva,by=c("Curriculo Entrada",variavel)) %>%
mutate(Evadidos=coalesce(Evadidos,0L),
Taxa=round(100*Evadidos/Ingressantes,2))

print(tab)

write_csv2(tab,file.path(pasta_tabelas,paste0(nome,"_cumulativo.csv")))
}

gerar_tabela("Sexo","sexo")
gerar_tabela("Faixa Etaria","faixa_etaria")
gerar_tabela("Periodo de Evasao","periodo_evasao")
gerar_tabela("Tipo de Evasao","tipo_evasao")

cat("PROCESSAMENTO CONCLUÍDO\n")
