library(tidyverse)
library(readxl)


#### CARREGAMENTO E CHECAGEM DO BANCO ####

base_produtos <- read_excel("base_produtos.xlsx")
View(base_produtos)

respostas_nps <- read_excel("respostas_nps.xlsx")
View(respostas_nps)

base_produtos <- as.data.frame(base_produtos)
respostas_nps<- as.data.frame(respostas_nps)

attach(base_produtos)
attach(respostas_nps)

glimpse(base_produtos) 
glimpse(respostas_nps)

# AJUSTE DO NOME DAS VARIAVEIS  

base_produtos <- base_produtos |> 
  rename(
    segmento_do_cliente = `Segmento do Cliente`,
    cartao_de_credito = `cartão de crédito`,
    cheque_especial = `cheque especial`,
    credito_pessoal = `crédito pessoal`,
    credito_consignado = `crédito consignado`)

respostas_nps <- respostas_nps |> 
  rename(
    CPF = CPF_FALSO,
    respostas_de_nps = `Resposta de NPS`,
    data_da_resposta = `Data da Resposta`,
    segmento_do_cliente = `Segmento do Cliente`
  ) 


# JUNTANDO OS DADOS 


dados_unidos <- left_join(base_produtos, respostas_nps,
                          by = "CPF")
View(dados_unidos)

# VERIFICANDO OS NA's

dados_unidos |> 
  summarise(across(everything(),~ sum(is.na(.))))

dados_unidos |> 
  filter(if_any(everything(), is.na))

 # os dados estão livre de NA


# ANALISE DESCRITIVA














