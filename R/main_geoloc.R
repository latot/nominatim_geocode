### Indicar la ruta de las funciones de geoloc en referencia al script
source(paste0(this.path::this.dir(), '/funciones_geoloc.R'))

library(readxl)

### Cargar direcciones

df1 <- read_excel(paste0(this.path::this.dir(), "/../data/SUPERMERCADOS Y ALMACENES.xls"), sheet = "ALMACEN")
df2 <- read.csv(paste0(this.path::this.dir(), '/../data/padron.csv'))

### Fijar Numero Primero
df1 <- df1 %>% number_first('DIRECCION')
df2 <- df2 %>% number_first('DOMICILIO_ELECTORAL')


### Configurar Parametros
params1 <- list(street = 'DIRECCION',
                city = 'Puente Alto',
                county = 'Provincia de Santiago',
                state = 'Metropolitana')

params2 <- list(street = 'DOMICILIO_ELECTORAL',
                city = 'COMUNA',
                county = 'PROVINCA',
                state = 'GREGION')


### Geocoding Normal
t <- local_nominatim(df1, params1, url = 'http://localhost:9999')

###############

### Geocoding Paralelizado
t <- local_nominatim_parallel(df2, params2, blocks_of = 5000, url = 'http://localhost:9999', cores = 10)
