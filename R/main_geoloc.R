source('R/funciones_geoloc.R')

### Cargar direcciones






### Configurar Parametros
params <- list( street = 'DIRECCION',
                city = 'Puente Alto',
                county = 'Provincia de Santiago',
                state = 'Metropolitana')

### Fijar Numero Primero
df <- df %>% number_first('DIRECCION')


### Geocoding Normal
t <- local_nominatim(df1, params, url = 'http://localhost:9999')

## PARALELIZADO
t <- local_nominatim_parallel(df2, params, blocks_of = 5000, url = 'http://localhost:9999', cores = 10)