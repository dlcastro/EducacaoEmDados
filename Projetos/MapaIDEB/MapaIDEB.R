#Autor: Daniel Lopes de Castro - Pedagogo e Mestre em Educação
#Website: https://dlcastro.com
#Lattes: http://lattes.cnpq.br/1893509400555503
#Twitter: @euDLCastro


#PACOTES
library(pacman)
p_load(data.table,tidyverse, purrr, readxl, dplyr, stringr)
p_load(tmap, leaflet, ggplot2)



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



  
IDEB <- Db %>% 
  select(ID_ESCOLA,CATEGORIA, contains(c("VL_OBSERVADO"))) %>%  #selecionar notas do IDEB e o código da escola
  mutate(across(contains("VL_OBSERVADO"), as.numeric)) %>% 
  pivot_longer(contains("VL_OBSERVADO")) %>% 
  na.omit() %>% 
  mutate(ANO = substr(name, 14,17)) %>% 
  select(ID_ESCOLA,CATEGORIA, ANO, IDEB = value)%>% 
  left_join(EscolasFinal, by = "ID_ESCOLA" )


saveRDS(IDEB, "Projetos/MapaIDEB/Dados/0-IDEB_FINAL.RDS")
write.csv(IDEB, "Projetos/MapaIDEB/Dados/0-IDEB_FINAL.csv")


# Mapa --------------------------------------------------------------------
# Brazil <- get_map(location = "Brazil",color = "bw", zoom = 4, maptype = "toner-lite")
# IDEB_2019 <- IDEB %>% filter(ANO == 2019 & CATEGORIA == "AI")
# BrazilSHP <- sf::st_read("Projetos/MAPAIDEB/Dados/SHP/UFEBRASIL.shp")
# 
# # ggmap(BrazilSHP) +
# #   stat_density2d(data=IDEB_2019,  aes(x=Longitude, y=Latitude,color = IDEB, fill=..level.., alpha=..level..), geom="polygon")
# 
# tm_shape(BrazilSHP) + tm_borders(col = "blue") 
#          