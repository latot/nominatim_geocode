library(tidygeocoder)
library(tidyverse)
library(stringr)
library(furrr)

df <- read.csv('padron_spp.xls',sep=';')
# df <- almacenes

## NUMEROS PRIMERO
# df$DOMICILIO_ELECTORAL <-  df$DOMICILIO_ELECTORAL %>%
number_first <- function(df,col){
  df[[col]] <-  df[[col]] %>%
    str_replace('^([a-zA-Z \\.]+)([\\d]+)','\\2 \\1') %>%
    str_replace('  .*','')
  
  return(df)
}

local_nominatim <- function(df,params = params, url = 'http://localhost:8080', min_time = 0.01 ){
  
  for (var_name in c('street','city','county','state')){
    parameter <- params[[var_name]]

    if (parameter %in% names(df)){
      assign(var_name, df[[parameter]])
    } else{
      assign(var_name, parameter)
    }
  }
  
  Max <- max(sapply(list(street,city,county,state), length))
  Min <- min(sapply(list(street,city,county,state), length))
  if (Min == 1){
    for (var_name in c('street','city','county','state') ){
      var <- get(var_name)
      if (length(var) == 1){
        assign(var_name,rep(var,Max))
      }
    }
  }
  
  t <- geo(street = street, city = city, county = county, state = state, method = 'arcgis' , min_time = min_time , full_results = T, flatten = F)
  
  return(t)
}

blocks_by_n <- function(df,n){
  s <- split(df, (as.numeric(rownames(df))-1) %/% n)
  return(s)
}

local_nominatim_parallel <- function(df,params,blocks_of, url, cores = availableCores() -1){
  plan(multisession, workers = cores)
    
  blocks <- blocks_by_n(df,blocks_of)
  geoloc_df <- future_map_dfr(blocks, function(x) local_nominatim(x,params,url = url, min_time = 0.01))
  
  return(geoloc_df)
}

###################################
###################################

params <- list( street = 'DIRECCION',
                city = 'Puente Alto',
                county = 'Provincia de Santiago',
                state = 'Metropolitana')

## NORMAL
df <- df %>% number_first('DIRECCION')

t <- local_nominatim(head(df,10), params, url = 'http://localhost:9999')
t2 <- local_nominatim(df, params)
t <- local_nominatim_parallel(df, params, blocks_of = 5000, url = 'http://localhost:9999', cores = 10)

## PARALELIZADO



