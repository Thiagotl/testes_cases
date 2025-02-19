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

dados_unidos <- dados_unidos |> 
  select(-c(segmento_do_cliente.y))

write.csv(dados_unidos, "dados_unidos.csv", row.names = FALSE)

# VERIFICANDO OS NA's

dados_unidos |> 
  summarise(across(everything(),~ sum(is.na(.))))

dados_unidos |> 
  filter(if_any(everything(), is.na))

 # os dados estão livre de NA


# ANALISE DESCRITIVA

table(dados_unidos$segmento_do_cliente.x)

table(respostas_nps$respostas_de_nps)



#DESCRITIVA NPS RESPOSTA

respostas_nps |> 
  summarise(
    total_respostas = n(),
    media = mean(respostas_de_nps, na.rm = TRUE),
    mediana = median(respostas_de_nps, na.rm = TRUE),
    minimo = min(respostas_de_nps, na.rm = TRUE),
    maximo = max(respostas_de_nps, na.rm = TRUE),
    desvio_padrao = sd(respostas_de_nps, na.rm = TRUE),
    Q1 = quantile(respostas_de_nps, 0.25, na.rm = TRUE),
    Q3 = quantile(respostas_de_nps, 0.75, na.rm = TRUE)
  )



# DESCRITIVA NPS CATEGORIZADA
respostas_nps <- respostas_nps |> 
  mutate(nps_categoria = case_when(
    respostas_de_nps >= 9 ~ "Promotor",
    respostas_de_nps >= 7 ~ "Neutro",
    respostas_de_nps >= 0 ~ "Detrator",
    TRUE ~ NA_character_
  ))

respostas_nps |> count(nps_categoria)

prop.table(table(respostas_nps$nps_categoria)) * 100


ggplot(respostas_nps, aes(x = nps_categoria, fill = nps_categoria)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 2) +  
  labs(title = "Distribuição das Categorias de NPS", x = "Categoria NPS", y = "Frequência") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  



# PENAS VERIFICANDO SE BATE COM OS DADOS QUE FORAM UNIDOS

dados_unidos <- dados_unidos |> 
  mutate(nps_categoria = case_when(
    respostas_de_nps >= 9 ~ "Promotor",
    respostas_de_nps >= 7 ~ "Neutro",
    respostas_de_nps >= 0 ~ "Detrator",
    TRUE ~ NA_character_
  ))

dados_unidos |> count(nps_categoria)

prop.table(table(dados_unidos$nps_categoria)) * 100

ggplot(dados_unidos, aes(x = nps_categoria, fill = nps_categoria)) +
  geom_bar() +
  labs(title = "Distribuição das Categorias de NPS", x = "Categoria NPS", y = "Frequência") +
  theme_minimal()

