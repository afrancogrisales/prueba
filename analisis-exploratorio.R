library(tidyverse)

options(scipen=999) #forzar R a no utilizar notación exponencial para números grandes

load("ventas.rda")


#gráfico de barras de ventas totales por país
ventas %>% group_by(country) %>%
  summarise(orders = n(), sales = sum(sales)) %>%
  mutate(country = reorder(country, sales)) %>%
    ggplot(aes(country, sales)) + geom_col() + ylab("Ventas") + xlab('País')

#gráfico de barras de ventas totales por territorio
ventas %>% group_by(territory) %>%
  summarise(orders = n(), sales = sum(sales)) %>%
  mutate(territory = reorder(territory, sales)) %>%
    ggplot(aes(territory, sales, fill = territory)) + geom_col() + ylab("Ventas") + 
    xlab('Territorio') + theme(legend.position = "none")

 
#gráfico de barras de número de órdenes por país
ventas %>% group_by(country) %>%
  summarise(orders = n(), sales = sum(sales)) %>%
  mutate(country = reorder(country, orders)) %>%
  ggplot(aes(country, orders)) + geom_col() + ylab("Órdenes") + xlab('País')

#gráfico de barras de número de órdenes por territorio
ventas %>% group_by(territory) %>%
  summarise(orders = n(), sales = sum(sales)) %>%
  mutate(territory = reorder(territory, orders)) %>%
  ggplot(aes(territory, orders, fill = territory)) + geom_col() + ylab("Órdenes") + 
  xlab('Territorio') + theme(legend.position = "none")


#gráfico de barras de órdenes por línea de producto
ventas %>% group_by(productline) %>%
  summarise(orders = n()) %>%
  ggplot(aes(productline, orders)) + geom_col() + ylab("Órdenes") + xlab('Línea')

#gráfico de barras de ventas totales por línea de producto
ventas %>% group_by(productline) %>%
  summarise(sales = sum(sales)) %>%
  ggplot(aes(productline, sales, fill = productline)) + geom_col() + ylab("Ventas") + xlab('Línea') +
  theme(legend.position = "none")


#para cada caso, vemos que hacer el análisis con ventas totales o número de órdenes aporta la misma información



#estado de envíos por territorio
ventas %>%
  ggplot(aes(status, fill = status)) + geom_bar() + facet_wrap(~territory) + scale_y_log10() +
  theme(legend.position = "none")

#segunda opción estado de envíos por territorio
ventas %>% 
  ggplot(aes(status, fill = territory)) + geom_bar() + scale_y_log10() 

#estado de los envíos por línea de producto
ventas %>%
  ggplot(aes(status, fill = status)) + geom_bar() + facet_wrap(~productline) + scale_y_log10() +
  theme(legend.position = "none")






