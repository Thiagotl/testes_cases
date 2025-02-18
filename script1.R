library(tidyverse)
library(forecast)
library(tseries)

source("functions.R")
dados <- read_csv("serie_case.csv")
View(dados)
attach(dados)

dados[,-c(1,2)] <- lapply(dados[,-c(1,2)], function(x) as.numeric(gsub("\\.", "", x)))

dados_ramo_A <- dados |> filter(RAMO == "A")

dados_ramo_A_long <- dados_ramo_A  |>  
  pivot_longer(
    cols = -c(DRE, RAMO), 
    names_to = "Data", 
    values_to = "Valor"
  )  |>  
  mutate(Data = as.Date(paste0(Data, "-01"), format = "%Y-%m-%d"))

ggplot(dados_ramo_A_long, aes(x = Data, y = Valor, color = DRE)) +
  geom_line() +
  labs(title = "Séries Temporais do Ramo A", x = "Ano", y = "Valor") +
  theme_minimal()

serie_lucro <- dados_ramo_A_long |> 
  filter(DRE == 'Lucro')

ts_valor<-ts(serie_lucro$Valor, start = c(2018,01), frequency = 12)
plot(ts_valor)


# Definir o número de observações de teste (12 meses)
n_test <- 12

# Número total de observações
n_total <- length(ts_valor)

# Índice de separação entre treino e teste
n_train <- n_total - n_test

ts_treino <- window(ts_valor, end = c(2023, 12))  
ts_teste <- window(ts_valor, start = c(2024, 1))  #


plot(ts_treino, main = "Série Temporal - Treino", col = "blue", lwd = 2)
plot(ts_teste, main = "Série Temporal - Teste", col = "red", lwd = 2)

md <- forecast::auto.arima(ts_treino)
md
forecast::checkresiduals(md)
fc <- forecast::forecast(md, h=12)
forecast::accuracy(fc,ts_teste)


TSstudio::test_forecast(actual = ts_valor,
                        forecast.obj = fc,
                        test = ts_teste)
