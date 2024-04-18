library(tidyverse)
library(readxl)
library(lubridate)
library(tseries)
library(car)
library(psych)
library(openxlsx)


# CARGAR DATOS
excel_sheets("DATOS.xlsx")

tabla <- read_excel("DATOS.xlsx", col_names = T)

str(tabla)

tabla <- tabla %>%
  mutate(Date = ymd(Date))

sum(is.na(tabla))

# grafico sin log

tabla %>%
  ggplot(aes(x = Date)) + geom_line(aes(y = IBEX35),color = "red") + geom_line(aes(y = DAX),color = "blue") +
  geom_line(aes(y = CAC40),color = "green") + geom_line(aes(y = NIKKEI),color = "black") +
  geom_line(aes(y = DOW_JONES),color = "orange") 

visualizar <- tabla %>%
  pivot_longer(cols = c("IBEX35","DAX","CAC40","NIKKEI","DOW_JONES"), names_to = "Indice", values_to = "precio")

visualizar %>%
  ggplot(aes(x = Date, y = precio)) + geom_line() + facet_wrap(~Indice, scales = "free_y")



## 


## log prices:
log_prices <- tabla %>%
  mutate(IBEX35 = log(IBEX35), DAX = log(DAX), CAC40 = log(CAC40), NIKKEI = log(NIKKEI), DOW_JONES = log(DOW_JONES))

log_prices %>%
  ggplot(aes(x = Date)) + geom_line(aes(y = IBEX35),color = "red") + geom_line(aes(y = DAX),color = "blue") +
  geom_line(aes(y = CAC40),color = "green") + geom_line(aes(y = NIKKEI),color = "black") +
  geom_line(aes(y = DOW_JONES),color = "orange") 

visualizar_log <- log_prices %>%
  pivot_longer(cols = c("IBEX35","DAX","CAC40","NIKKEI","DOW_JONES"), names_to = "Indice", values_to = "precio")

visualizar_log %>%
  ggplot(aes(x = Date, y = precio)) + geom_line() + facet_wrap(~Indice, scales = "free_y") +
  ylab("Logaritme dels preus en moneda nacional")


# guardar datos:

write.xlsx(tabla, "Precios.xlsx")
write.xlsx(log_prices, "Log_precios.xlsx")



# contrastes 

# ibex 35

library(aTSA)

adf.test(log_prices$IBEX35) # no rechazamos ho

# dax
adf.test(log_prices$DAX)  # no rechazamos ho

# cac
adf.test(log_prices$CAC40)  # no rechazamos ho


## prueba
log_p <- log_prices %>%
  select(!Date)

map(log_p, adf.test)



# predicciones:
log_prices_pred <- log_prices %>%
  mutate(
    change_DAX = diff(DAX),
    change_CAC = diff
    
  )

Ibexp = -0.00029 + 0.25 *(log_prices$DAX[2]-log_prices$DAX[1])
