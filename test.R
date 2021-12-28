library(tidygeocoder)
library(tidyverse)
library(stringr)
library(furrr)

# df <- read.csv('padron_spp.xls',sep=';')
df <- almacenes

## NUMEROS PRIMERO
# df$DOMICILIO_ELECTORAL <-  df$DOMICILIO_ELECTORAL %>%
number_first <- function(df,col){
  df[[col]] <-  df[[col]] %>%
    str_replace('^([a-zA-Z \\.]+)([\\d]+)','\\2 \\1') %>%
    str_replace('  .*','')
  
  return(df)
}

local_nominatim <- function(df,params = params, fill = F , url = 'http://localhost:8080', min_time = 0.01 ){
  
  for (var_name in c('street','city','county','state')){
    parameter <- params[[var_name]]

    if (parameter %in% names(df)){
      assign(var_name, df[[parameter]])
    } else if (fill == T){
      assign(var_name, parameter)
    } else{
      assign(var_name, NULL)
    }
  }
  
  if (fill == T){
    M <- max(sapply(list(street,city,county,state), length))
    for (var_name in c('street','city','county','state') ){
      var <- get(var_name)
      if (length(var) == 1){
        assign(var_name,rep(var,M))
      }
    }
  }
  
  t <- geo(street = street, city = city, county = county, state = state, method = 'osm',api_url = url , min_time = min_time )
  
  return(t)
}

blocks_by_n <- function(df,n){
  s <- split(df, (as.numeric(rownames(df))-1) %/% n)
  return(s)
}

local_nominatim_parallel <- function(df,blocks_of ,cores = availableCores() -1){
  plan(multissesion, workers = cores)
    
  blocks <- blocks_by_n(df,blocks_of)
  geoloc_df <- future_map_dfr(blocks, function(x) local_nominatim(x,params,url = 'http://localhost:9999', min_time = 0.01))
  
  return(geoloc_df)
}

###################################
###################################

params <- list( street = 'DIRECCION',
                city = 'PUENTE ALTO',
                county = 'PROVINCIA DE SANTIAGO',
                state = 'REGION METROPOLITANA')

## NORMAL
t <- local_nominatim(df, params, fill = T, url = 'http://localhost:9999')

## PARALELIZADO




tic()
toc()
