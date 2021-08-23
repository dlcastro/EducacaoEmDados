#Autor: Daniel Lopes de Castro - Pedagogo e Mestre em Educação
#Website: https://dlcastro.com
#Lattes: http://lattes.cnpq.br/1893509400555503
#Twitter: @euDLCastro


#Parte opcional para complementar os dados faltantes de geolocalização

#ATENÇÃO: ESCOLAS SEM LOCALIZAÇÃO SERÃO OBTIDAS POR GEOCODE DO GOOGLE MAPS
#SÃO EM TORNO DE 12MIL CASOS. Pode gerar cobrança dependendo do seu uso.
#Utilize por conta em risco. Caso não queira utilizar, não escreva sua API abaixo.

#########colocar a key da API aqui:
# register_google(key = "")

EscolasGoogle <- Escolas %>% 
  filter(is.na(Longitude)) %>%
  mutate(N = row_number(),
         END_FINAL = ifelse(is.na(ENDERECO),
                            paste0("Escola ",NO_ESCOLA,", cidade ",NO_MUNICIPIO,", ",SG_UF,", ","Brasil"),
                            paste0("Escola ",NO_ESCOLA,", ", ENDERECO,", ","Brasil")))


GeoCode1 <- map_df(1:3000, function(x){
  INF <- EscolasGoogle %>% filter(N=={{x}})
  pubs <- array(INF$END_FINAL)
  
  pubs_ggmap <- geocode(location = pubs, output = "more", source = "google")
  RESULTADO <- cbind(INF,pubs_ggmap)
})

saveRDS(GeoCode1, "Projetos/MapaIDEB/Dados/GEOCODE_GOOGLE_ESCOLAS_1.RDS")

GeoCode2 <- map_df(3001:6000, function(x){
  INF <- EscolasGoogle %>% filter(N=={{x}})
  pubs <- array(INF$END_FINAL)
  
  pubs_ggmap <- geocode(location = pubs, output = "more", source = "google")
  RESULTADO <- cbind(INF,pubs_ggmap)
})

saveRDS(GeoCode2, "Projetos/MapaIDEB/Dados/GEOCODE_GOOGLE_ESCOLAS_2.RDS")

GeoCode3 <- map_df(6001:nrow(EscolasGoogle), function(x){
  INF <- EscolasGoogle %>% filter(N=={{x}})
  pubs <- array(INF$END_FINAL)
  
  pubs_ggmap <- geocode(location = pubs, output = "more", source = "google")
  RESULTADO <- cbind(INF,pubs_ggmap)
})


saveRDS(GeoCode3, "Projetos/MapaIDEB/Dados/GEOCODE_GOOGLE_ESCOLAS_3.RDS")


GeoCode <- bind_rows(GeoCode1,GeoCode2,GeoCode3)

saveRDS(GeoCode, "Projetos/MapaIDEB/Dados/GEOCODE_GOOGLE_ESCOLAS_FINAL.RDS") 



# Arrumar casos problemáticos ---------------------------------------------

GeoCode <- readRDS("Projetos/MapaIDEB/Dados/GEOCODE_GOOGLE_ESCOLAS_FINAL.RDS")

GeoCodeTratamento <- GeoCode %>% 
  mutate(Brazil = ifelse(word(address,-1)=="brazil",1,0)) %>% 
  filter(is.na(lon) | Brazil==0) %>% 
  mutate(CEP = str_extract(ENDERECO,"\\d{5}([- ]*\\d{3})?"),
         END_FINAL =  ifelse(!is.na(CEP),
                             paste0(NO_MUNICIPIO,", ", SG_UF, ", Brazil, ", "CEP ", CEP),
                             paste0(NO_MUNICIPIO,", ", SG_UF, ", Brazil"))) %>% 
  select(1:END_FINAL) %>% 
  mutate(N = row_number())
  
GeoCodeFaltante <- map_df(1:nrow(GeoCodeTratamento), function(x){
  INF <- GeoCodeTratamento %>% filter(N=={{x}})
  pubs <- array(INF$END_FINAL)
  
  pubs_ggmap <- geocode(location = pubs, output = "more", source = "google")
  RESULTADO <- cbind(INF,pubs_ggmap)
}) %>% 
  mutate(FONTE = "Google Endereço Centralizado")



saveRDS(GeoCodeFaltante, "Projetos/MapaIDEB/Dados/GEOCODE_GOOGLE_ESCOLAS_FALTANTE.RDS") 


# UNIFICAR BASES ----------------------------------------------------------

GeoCodeGoogleOK <- GeoCode %>% 
  mutate(Brazil = ifelse(word(address,-1)=="brazil",1,0)) %>% 
  filter(Brazil==1) %>% 
  mutate(FONTE = "Google Nome Escola e Endereço")

EscolasFinal <- Escolas %>% 
  filter(!is.na(Longitude)) %>% 
  bind_rows(GeoCodeFaltante) %>% 
  bind_rows(GeoCodeGoogleOK) %>% 
  mutate(Latitude = ifelse(is.na(Latitude),lat,Latitude),
         Longitude = ifelse(is.na(Longitude),lon,Longitude)) %>% 
  select(1:FONTE)

saveRDS(EscolasFinal, "Projetos/MapaIDEB/Dados/GEOCODE_ESCOLAS_BRASIL_FINALIZADO.RDS") 
