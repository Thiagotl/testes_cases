library(readxl)
library(tidyverse)

dados <- read_excel("teste.xlsx")
View(teste)



dados <- dados %>%
  mutate(Campanha = as.factor(Campanha),
         Genero = as.factor(Genero))

dados <- data.frame(
  Genero = c("Aedeomyia", "Aedes", "Anopheles", "Culex", "Haemagogus", 
             "Limatus", "Mansonia", "Nyssorhynchus", "Orthopodomyia", 
             "Psorophora", "Sabethes", "Trichoprosopon", "Uranotaenia", "Wyeomyia"),
  Campanha1 = c(0, 12, 4, 142, 0, 5, 0, 10, 11, 4, 2, 0, 8, 0),
  Campanha2 = c(0, 3, 0, 501, 0, 2, 2, 45, 4, 4, 2, 0, 13, 0),
  Campanha3 = c(0, 60, 4, 613, 1, 6, 4, 1, 1, 43, 2, 0, 21, 2),
  Campanha4 = c(1, 48, 1, 407, 0, 6, 0, 1, 1, 20, 0, 2, 6, 1),
  Campanha5 = c(0, 25, 0, 126, 0, 4, 0, 1, 0, 11, 0, 0, 1, 13),
  Campanha6 = c(0, 65, 0, 123, 0, 0, 0, 3, 0, 19, 1, 1, 19, 0)
)

dados_longo <- dados %>%
  pivot_longer(cols = starts_with("Campanha"), 
               names_to = "Campanha", 
               values_to = "Abundancia") %>%
  mutate(Campanha = as.factor(Campanha))


# Realizar a ANOVA
anova_result <- aov(Abundancia ~ Campanha, data = dados_longo)

# Resumo dos resultados da ANOVA
summary(anova_result)




dados_longo %>%
  group_by(Campanha) %>%
  summarise(p_valor = shapiro.test(Abundancia)$p.value)


kruskal_test <- kruskal.test(Abundancia ~ Campanha, data = dados_longo)

kruskal_test


ggplot(dados_longo, aes(x = Campanha, y = Abundancia)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Abundância por Campanha", x = "Campanha", y = "Abundância") +
  theme_minimal()


shapiro.test(residuals(anova_result))

qqnorm(residuals(anova_result))
qqline(residuals(anova_result))
hist(residuals(anova_result), main = "Histograma dos Resíduos", xlab = "Resíduos")


library(car)
leveneTest(Abundancia ~ Campanha, data = dados_longo)

# Transformar para o formato wide
dados_pca <- dados_longo %>%
  pivot_wider(names_from = Campanha, values_from = Abundancia)


# Definir Genero como nomes das linhas
rownames(dados_pca) <- dados_pca$Genero

# Remover a coluna Genero para manter apenas valores numéricos
dados_pca <- dados_pca %>% select(-Genero)



# Realizar o PCA
pca_result <- prcomp(dados_pca, scale. = TRUE)

# Resumo dos resultados
summary(pca_result)

# Visualizar os autovalores
pca_result$sdev^2

# Biplot para visualizar a variabilidade
biplot(pca_result, scale = 0)



# Variância explicada por componente
variancia_explicada <- pca_result$sdev^2 / sum(pca_result$sdev^2)
variancia_explicada

# Gráfico de scree plot
library(ggplot2)

data.frame(Componente = 1:length(variancia_explicada), Variancia = variancia_explicada) %>%
  ggplot(aes(x = Componente, y = Variancia)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_line(aes(group = 1), color = "red") +
  labs(title = "Scree Plot", x = "Componente Principal", y = "Variância Explicada") +
  theme_minimal()



totais <- c(351, 314, 801, 279, 709)
shapiro.test(totais)


dados <- data.frame(
  Ponto = c("Ponto 1", "Ponto 2", "Ponto 3", "Ponto 4", "Ponto 5"),
  Total = c(351, 314, 801, 279, 709)
)
# Kruskal-Wallis
kruskal.test(Total ~ Ponto, data = dados)

modelo <- lm(Total ~ as.numeric(Ponto), data = dados)
summary(modelo)


ggplot(dados, aes(x = Ponto, y = Total)) +
  geom_bar(stat = "identity") +
  labs(title = "Total de Espécimes por Ponto", y = "Total de Espécimes", x = "Ponto")

library(ggplot2)
ggplot(dados, aes(x = Ponto, y = Total)) +
  geom_boxplot() +
  labs(title = "Distribuição do Total de Espécimes por Ponto", y = "Total de Espécimes", x = "Ponto")

dados <- data.frame(
  Ponto = c("Ponto 1", "Ponto 2", "Ponto 3"),
  Campanha1 = c(351, 314, 801),
  Campanha2 = c(279, 709, 401),
  Campanha3 = c(432, 545, 320)
)

# Aplicar o teste de Friedman
friedman_result <- friedman.test(as.matrix(dados[, -1])) # Remover a coluna "Ponto"
print(friedman_result)


wilcox.test(dados$Campanha1, dados$Campanha2, paired = TRUE)

