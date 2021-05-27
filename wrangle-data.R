library(readr)
library(tidyverse)
library(lubridate)


ventas <- read_csv("sales_data_sample.csv")

#cambiar nombres de columnas a minúsculas
new_names <- str_to_lower(names(ventas))
names(ventas) <- new_names


#volver factores todas las variables que son factores
ventas <- ventas %>% mutate(status = as.factor(status), productline = as.factor(productline), 
                            dealsize = as.factor(dealsize), territory = as.factor(territory))


#formatear las fechas de orden
formatear_fechas <- function(x){
  x <- str_extract(x, pattern = "\\d+\\/\\d+\\/\\d+")
  mdy(x)
}

ventas <- ventas %>% mutate(orderdate = formatear_fechas(orderdate))


#guardar los datos
save(ventas, file = "ventas.rda")
