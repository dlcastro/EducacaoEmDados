#Autor: Daniel Lopes de Castro - Pedagogo e Mestre em Educação
#Website: https://dlcastro.com
#Lattes: http://lattes.cnpq.br/1893509400555503
#Twitter: @euDLCastro


#PACOTES
library(pacman)
p_load(data.table,tidyverse, purrr, readxl, dplyr, stringr)




###############################################################################
############################ PROJETO: Mapa do IDEB ############################
###############################################################################

# INFORMAÇÕES DAS ESCOLAS -------------------------------------------------

#Latitude e Longitude - Obtido em: https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/inep-data/catalogo-de-escolas (22/08/2021)
LatLong <- data.table::fread("Projetos/MapaIDEB/Dados/ESCOLAS.csv",encoding = "UTF-8")%>% 
  select(ID_ESCOLA =`Código INEP`,Latitude, Longitude, ENDERECO = `Endereço`)


#O IDEB é hoje divulgado para Anos Iniciais (AI) e Anos Finais (AF) do Ensino Fundamental, além do Ensino Médio (EM)
Bases <- c("AI","AF","EM")

#IDEB - Obtido em: https://www.gov.br/inep/pt-br/areas-de-atuacao/pesquisas-estatisticas-e-indicadores/ideb/resultados (22/08/2021)
#Baixe as 3 planilhas em nível de escola

Db <- purrr::map_df(Bases, function(x){
  
  db <- readxl::read_excel(paste0("Projetos/MapaIDEB/Dados/IDEB_",x,"_2019.xlsx"),guess_max = 21474836, skip = 9) %>% 
    mutate(CATEGORIA = {{x}})
  
})


# Informações das escolas com Latitude e Longitude
Escolas <- EscolasFinal <-  Db %>% 
  select(ID_ESCOLA,REDE, NO_ESCOLA, CO_MUNICIPIO, NO_MUNICIPIO, SG_UF) %>% 
  group_by(ID_ESCOLA) %>% 
  mutate(n = row_number()) %>% 
  ungroup() %>% 
  filter(n==1) %>% 
  left_join(LatLong, by = "ID_ESCOLA") %>% 
  mutate(FONTE = "INEP")



# Preencher missing de Latitude e Longitude com Google Maps ---------------

#Parte opcional para complementar os dados faltantes de geolocalização

#ATENÇÃO: ESCOLAS SEM LOCALIZAÇÃO SERÃO OBTIDAS POR GEOCODE DO GOOGLE MAPS
#SÃO EM TORNO DE 12MIL CASOS. Pode gerar cobrança dependendo do seu uso.
#Utilize por conta em risco. Caso  queira utilizar, escreva sua API abaixo e
# desmarque os comentários.


# register_google(key = "")  ###colocar a key da API aqui
#
# source("Projetos/MapaIDEB/GoogleGeoCode_MapaIDEB.R")

EscolasFinal <- readRDS("Projetos/MapaIDEB/Dados/GEOCODE_ESCOLAS_BRASIL_FINALIZADO.RDS")

#COORTE NOTAS: https://ajuda.focoescola.com.br/hc/pt-br/articles/360058736853-Como-%C3%A9-definido-o-padr%C3%A3o-de-desempenho-dos-alunos-para-os-resultados-do-Saeb-
  
NOTAS <- Db %>% 
  select(ID_ESCOLA,CATEGORIA, contains(c("NOTA_MATEMATICA","NOTA_PORTUGUES"))) %>%  #selecionar notas do IDEB e o código da escola
  mutate(across(contains("NOTA"), as.numeric)) %>% 
  pivot_longer(contains("NOTA")) %>% 
  na.omit() %>% 
  mutate(ANO = str_extract(name,"\\d{4}"),
         TIPO_NOTA = str_extract(name,"\\MATEMATICA"),
         TIPO_NOTA = ifelse(is.na(TIPO_NOTA),"PORTUGUES",TIPO_NOTA),
         name = paste0("NOTA_",TIPO_NOTA)) %>% 
  rename(NOTA = value) %>% 
  select(-TIPO_NOTA) %>% 
  pivot_wider(values_from = NOTA,names_from = name)%>% 
  mutate(CATEGORIA_PORTUGUES = case_when(CATEGORIA=="AI" & NOTA_PORTUGUES<200 ~ 1,
                                          CATEGORIA=="AI" & NOTA_PORTUGUES>=200 & NOTA_PORTUGUES<=250 ~ 2,
                                          CATEGORIA=="AI" & NOTA_PORTUGUES>250 ~ 3,
                                         
                                         CATEGORIA=="AF" & NOTA_PORTUGUES<275 ~ 1,
                                         CATEGORIA=="AF" & NOTA_PORTUGUES>=275 & NOTA_PORTUGUES<=325 ~ 2,
                                         CATEGORIA=="AF" & NOTA_PORTUGUES>325 ~ 3)) %>% 
  
  mutate(CATEGORIA_MATEMATICA = case_when(CATEGORIA=="AI" & NOTA_MATEMATICA<225 ~ 1,
                                         CATEGORIA=="AI" & NOTA_MATEMATICA>=225 & NOTA_MATEMATICA<275 ~ 2,
                                         CATEGORIA=="AI" & NOTA_MATEMATICA>=275 ~ 3,
                                         
                                         CATEGORIA=="AF" & NOTA_MATEMATICA<300 ~ 1,
                                         CATEGORIA=="AF" & NOTA_MATEMATICA>=300 & NOTA_MATEMATICA<=350 ~ 2,
                                         CATEGORIA=="AF" & NOTA_MATEMATICA>350 ~ 3)
         ) %>% 
  left_join(EscolasFinal, by = "ID_ESCOLA" )


#Salvar bases
map(Bases, function(x){
  
modelo <-   NOTAS %>% 
    filter(CATEGORIA=={{x}})
  
saveRDS(modelo,paste0("Projetos/MapaIDEB/Resultado/0-NOTAS_SAEB_",x,".RDS"))
write.csv(modelo,paste0("Projetos/MapaIDEB/Resultado/0-NOTAS_SAEB_",x,".csv"))

})





# Mapa --------------------------------------------------------------------
#CRIAR NO QGIS
#https://www.qgistutorials.com/en/docs/3/creating_heatmaps.html
