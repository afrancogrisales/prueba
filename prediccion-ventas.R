library(MASS)
library(tidyverse)
library(lubridate)
library(broom)

options(scipen=999) #forzar R a no utilizar notación exponencial para números grandes

load("ventas.rda")

#agregar datos de ventas por día
ventas %>%
  group_by(orderdate) %>%
  summarise(sales = sum(sales)) %>%
  ggplot(aes(orderdate, sales)) + geom_point(alpha = 0.5)

#el agregado por día no nos permite reconocer un patrón para plantear un modelo

#agregar datos de ventas por mes
ventas_por_mes <- ventas %>%
  mutate(aux_day = "01") %>%
  unite(ordermonth, year_id, month_id, aux_day, sep = "-") %>%
  mutate(ordermonth = ymd(ordermonth)) %>% 
  group_by(ordermonth) %>%
  summarise(sales = sum(sales)) 
  
  
#gráfico de ventas por mes 
ventas_por_mes %>%
    ggplot(aes(ordermonth, sales)) + geom_point()

#esta gráfica sugiere plantear un modelo lineal
#podemos ver que en noviembre las ventas totales son mucho mayores que en los demás meses del año, así que
#utilizaremos un método de regresión robusta

#boxplot de ventas mensuales en los años 2003 y 2004
ventas_por_mes %>%
  mutate(year = as.factor(year(ordermonth))) %>%
  filter(year != "2005") %>%
  ggplot(aes(year, sales, fill = year)) + geom_boxplot() + theme(legend.position = "none") + 
  scale_y_log10() + geom_point(alpha = 0.5)

#este boxplot nos permite observar la distribución de las ventas mensuales para cada año
#el puntó más alto de cada distribución corresponde al mes de noviembre


#gráfico de ventas por mes con modelo lineal e intervalos de confianza del 95%
ventas_por_mes %>%
  ggplot(aes(ordermonth, sales)) + geom_point(data = ventas_por_mes) + geom_smooth(method = "rlm")

#modelo lineal de ventas por mes
fit <- ventas_por_mes %>%
  rlm(sales ~ ordermonth, data = .) 
  
#predicción de ventas por mes, de junio de 2005 hasta diciembre de 2005
meses_prediccion <- seq(ymd("2005-06-01"), ymd("2005-12-01"), by = "months")
meses_prediccion <- as_tibble_col(meses_prediccion, column_name = "ordermonth")

prediccion <- predict(fit, newdata = meses_prediccion, interval = "confidence")
prediccion <- as_tibble(prediccion)
prediccion <- bind_cols(meses_prediccion, prediccion)

#gráfico de las predicciones con intervalo de confianza del 95%
prediccion %>%
  ggplot(aes(ordermonth, fit, ymin = lwr, ymax = upr)) + geom_point(size = 2) + geom_errorbar()

#gráfico de las predicciones junto con los datos 
ventas_por_mes %>%
    ggplot(aes(ordermonth, sales)) + geom_point() + geom_smooth(method = "rlm") + 
    geom_point(data = prediccion, aes(ordermonth, fit), size = 2) +
    geom_errorbar(data = prediccion, aes(ordermonth, fit, ymin = lwr, ymax = upr))


#error promedio del mes de noviembre

prediccion_2 <- predict(fit)
prediccion_2 <- as_tibble(prediccion_2)
prediccion_2 <- bind_cols(ventas_por_mes, prediccion_2)

prediccion_2 %>% filter(ordermonth == ymd("2003-11-01") | ordermonth == ymd("2004-11-01")) %>%
  summarise(mean_error = mean(c(sales[1] - value[1], sales[2] - value[2])))






