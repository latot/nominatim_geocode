library(tidygeocoder)
library(tidyverse)
library(stringr)
library(furrr)

# df <- read.csv('padron_spp.xls',sep=';')
df <- almacenes

## NUMEROS PRIMERO
# df$DOMICILIO_ELECTORAL <-  df$DOMICILIO_ELECTORAL %>%
number_first <- function(col){
  df$DIRECCION <-  df$DIRECCION %>%
    str_replace('^([a-zA-Z \\.]+)([\\d]+)','\\2 \\1') %>%
    str_replace('  .*','')
}

local_nominatim <- function(df,params = params, fill = F , url = 'http://localhost:8080', min_time = 0.01 ){
  if ( params$street %in% names(df) ){
    street <- df %>% pull(params$street)
  } else if (fill == T){
    street <- params$street
  } else{
    street <- NULL
  }
  if ( params$city %in% names(df) ){
    city <- df %>% pull(params$city)
  } else if (fill == T){
    city <- params$city
  } else{
    city <- NULL
  }
  if ( params$county %in% names(df) ){
    county <- df %>% pull(params$county)
  } else if (fill == T){
    county <- params$county
  } else{
    county <- NULL
  }
  if ( params$state %in% names(df) ){
    state <- df %>% pull(params$state)
  } else if (fill == T){
    state <- params$state
  } else{
    state <- NULL
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

params <- list( street = 'DIRECCION',
                city = 'PUENTE ALTO',
                county = 'PROVINCIA DE SANTIAGO',
                state = 'REGION METROPOLITANA')

## NORMAL
t <- local_nominatim(df, params, fill = T, url = 'http://localhost:9999')

## PARALELIZADO
plan(multisession, workers = 10)

s <- blocks_by_n(df,5000)

tic()
prueba <- future_map_dfr(s, function(x) local_nominatim(x,params,url = 'http://localhost:9999', min_time = 0.01))
toc()
