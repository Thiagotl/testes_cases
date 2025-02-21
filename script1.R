library(tidyverse)
library(forecast)
library(tseries)
library(TSstudio)
source("functions.R")

dados <- read_csv("serie_case.csv")

dados<-as.data.frame(dados)
View(dados)
attach(dados)
glimpse(dados)

#dados[,-c(1,2)] <- lapply(dados[,-c(1,2)], function(x) as.numeric(gsub("\\.", "", x)))
# dados <- dados %>%
#   mutate(across(-c(DRE, RAMO), ~ as.numeric(gsub("\\.", "", .)), .names = "converted_{.col}")) %>%
#   select(DRE, RAMO, starts_with("converted_")) %>%
#   rename_with(~ sub("converted_", "", .x))



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

serie_lucro <- dados_ramo_A_long |>  # aqui é premio
  filter(DRE == 'Prêmio')

ts_valor<-ts(serie_lucro$Valor, start = c(2018,01), frequency = 12)
plot(ts_valor)

acf(ts_valor)

tend_determ(ts_valor)

raiz_unit(ts_valor)

sazonalidade(ts_valor)
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
checkresiduals(md)
fc <- forecast(md, h=12)
accuracy(fc,ts_teste)
acf(residuals(md))
pacf(residuals(md))


TSstudio::test_forecast(actual = ts_valor,
                        forecast.obj = fc,
                        test = ts_teste)


teste_a<-auto.arima(ts_valor)
fc_a<-forecast(teste_a, h = 12)

plot(fc_a)



mod_nn <- nnetar(ts_valor)
forecast_nn <- forecast(mod_nn, h = 12)
autoplot(forecast_nn)

checkresiduals(mod_nn)

######

sinistro <- dados_ramo_A_long |> 
  filter(DRE == 'Sinistro')
premio <- dados_ramo_A_long |> 
  filter(DRE == 'Prêmio')
comissao<- dados_ramo_A_long |> 
  filter(DRE == 'Comissão')



covar <- cbind(sinistralidade = sinistro$Valor / premio$Valor ,
         comissionamento = comissao$Valor / premio$Valor)

covar<-as.data.frame(covar)

xreg_treino <- cbind(sinistralidade = covar$sinistralidade[1:length(ts_treino)],
                     comissionamento = covar$comissionamento[1:length(ts_treino)])

xreg_teste <- cbind(sinistralidade = covar$sinistralidade[(length(ts_treino) + 1):length(ts_valor)],
                    comissionamento = covar$comissionamento[(length(ts_treino) + 1):length(ts_valor)])


md_xreg <- auto.arima(as.numeric(ts_treino), xreg = xreg_treino)

summary(md_xreg)
fc_xreg <- forecast(md_xreg, xreg = xreg_teste, h = 12)

TSstudio::test_forecast(actual = ts_valor,
                        forecast.obj = fc_xreg,
                        test = ts_teste)
checkresiduals(fc_xreg)

##### RAMO C


dados_ramo_C <- dados |> filter(RAMO == "C")

dados_ramo_C_long <- dados_ramo_C  |>  
  pivot_longer(
    cols = -c(DRE, RAMO), 
    names_to = "Data", 
    values_to = "Valor"
  )  |>  
  mutate(Data = as.Date(paste0(Data, "-01"), format = "%Y-%m-%d"))

ggplot(dados_ramo_C_long, aes(x = Data, y = Valor, color = DRE)) +
  geom_line() +
  labs(title = "Séries Temporais do Ramo C", x = "Ano", y = "Valor") +
  theme_minimal()

serie_premio_c <- dados_ramo_C_long |> 
  filter(DRE == 'Prêmio')

ts_valor_c<-ts(serie_premio_c$Valor, start = c(2018,01), frequency = 12)
plot(ts_valor_c)


tend_determ(ts_valor_c)

raiz_unit(ts_valor_c)

sazonalidade(ts_valor_c)
# Definir o número de observações de teste (12 meses)
n_test <- 12

# Número total de observações
n_total <- length(ts_valor_c)

# Índice de separação entre treino e teste
n_train <- n_total - n_test

ts_treino <- window(ts_valor_c, end = c(2023, 12))  
ts_teste <- window(ts_valor_c, start = c(2024, 1))  #


plot(ts_treino, main = "Série Temporal - Treino", col = "blue", lwd = 2)
plot(ts_teste, main = "Série Temporal - Teste", col = "red", lwd = 2)

md_c <- forecast::auto.arima(ts_treino)
md_c
checkresiduals(md_c)
fc_c <- forecast(md_c, h=12)
accuracy(fc_c,ts_teste)
acf(residuals(md_c))
pacf(residuals(md_c))


TSstudio::test_forecast(actual = ts_valor_c,
                        forecast.obj = fc_c,
                        test = ts_teste)



seriediff<-diff(ts_valor_c)
teste_c<-auto.arima(seriediff)

checkresiduals(teste_c)

fc_c <- forecast(teste_c, h=12)

plot(fc_c)

mod_nn <- nnetar(ts_valor_c)
forecast_nn <- forecast(mod_nn, h = 12)
autoplot(forecast_nn)




######

sinistro_c <- dados_ramo_C_long |> 
  filter(DRE == 'Sinistro')
premio_c <- dados_ramo_C_long |> 
  filter(DRE == 'Prêmio')
comissao_c<- dados_ramo_C_long |> 
  filter(DRE == 'Comissão')



covar <- cbind(sinistralidade = sinistro_c$Valor / premio_c$Valor ,
               comissionamento = comissao_c$Valor / premio_c$Valor)

covar<-as.data.frame(covar)

xreg_treino <- cbind(sinistralidade = covar$sinistralidade[1:length(ts_treino)],
                     comissionamento = covar$comissionamento[1:length(ts_treino)])

xreg_teste <- cbind(sinistralidade = covar$sinistralidade[(length(ts_treino) + 1):length(ts_valor_c)],
                    comissionamento = covar$comissionamento[(length(ts_treino) + 1):length(ts_valor_c)])


md_xreg <- auto.arima(ts_treino, xreg = xreg_treino)

summary(md_xreg)
fc_xreg <- forecast(md_xreg, xreg = xreg_teste, h = 12)

TSstudio::test_forecast(actual = ts_valor_c,
                        forecast.obj = fc_xreg,
                        test = ts_teste)
checkresiduals(fc_xreg)


md_xreg$fitted


######




dados_ramo_B <- dados |> filter(RAMO == "B")

dados_ramo_B_long <- dados_ramo_B |>  
  pivot_longer(
    cols = -c(DRE, RAMO), 
    names_to = "Data", 
    values_to = "Valor"
  )  |>  
  mutate(Data = as.Date(paste0(Data, "-01"), format = "%Y-%m-%d"))

ggplot(dados_ramo_B_long, aes(x = Data, y = Valor, color = DRE)) +
  geom_line() +
  labs(title = "Séries Temporais do Ramo B", x = "Ano", y = "Valor") +
  theme_minimal()

serie_premio_b <- dados_ramo_B_long |> 
  filter(DRE == 'Prêmio')

ts_valor_b<-ts(serie_premio_b$Valor, start = c(2018,01), frequency = 12)
plot(ts_valor_b)


tend_determ(ts_valor_b)

raiz_unit(ts_valor_b)

sazonalidade(ts_valor_b)



