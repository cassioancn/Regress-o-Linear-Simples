library(readxl)
dados <- read_excel("~/1UNISINOS/ALURA/RSTUDIO/Regressão Linear/regressaolinear_r-master/dados.xlsx")
View(dados)

#Plotar os gráficos de dispersão
plot(dados$area, dados$preco, main = "Diagrama de Dispersão", xlab = "Área", ylab = "Preço", pch = 19, col = "blue")
cor(dados$area, dados$preco)
cor.test(dados$area, dados$preco)

plot(dados$tempo, dados$preco, main = "Diagrama de Dispersão", xlab = "Tempo", ylab = "Preço", pch = 10, col = "red")
cor(dados$tempo, dados$preco)     
cor.test(dados$tempo, dados$preco)

#Plotar o boxplot de preço
boxplot(dados$preco)

#Identificar a casa do outlier pelo pacote car
install.packages("car")
library(car)
Boxplot(dados$preco)

#Descobrir o valor do outlier pela casa dele
dados$preco[79]

#Descobrir quais são as casas do terceiro quartil (caixa de cima)
which(dados$preco > quantile(dados$preco, 0.75))

##Equação da reta

mod1 = lm(preco ~ area, data = dados)
mod1

## Fórmulada da reta para estimar o preço em razão da área. Substituir os coefs na fórmula : preco = b0 + b1*area

preco_70 = 502347 + 7851*(70)
preco_70

## Outro modo de fazer a mesma previsão

mod1$coefficients[[1]] + mod1$coefficients[[2]]*70

##Gerar o gráfico disso

plot(dados$area, dados$preco, main = "Diagrama e reta", xlab = "Área (m²)", ylab = "Preço (R$)", pch = 19, col = "blue") + abline(mod1, col = "red")

##Validar o modelo: verificar se a área influi no preço das casas

summary(mod1)

##Outro mpétodo de validar os dados
names(summary(mod1))

summary(mod1)$r.squared

##Verficar o tempo em relação ao tempo

mod2 = lm(preco ~ tempo, data = dados)
mod2

summary(mod2)

###Verificar os resíduos do modelo

plot(mod1$residuals)

identify(mod1$residuals, n = 2)
mod1$residuals[82]

###Remover uma posição desejada

dados_59 = dados[-59,]

###Remover duas casas desejadas

dados_59_82 = dados[c(-59, -82),]


###Teste de independência dos erros do modelo

install.packages("lmtest")
library(lmtest)

dwtest(mod1)

plot(mod1$fitted.values, mod1$residuals)
bptest(mod1)

plot(mod1, 2)

shapiro.test(mod1$residuals)

####Previsão

dados_novos = data.frame(area = c(60, 70))
predict(mod1, newdata = dados_novos)####Vai retornar os valores estimados para o preço em função das áreas requisitadas no vetor

####Previsão de um intervalo


predict(mod1, newdata = dados_novos, interval = "prediction") #fit=estimativa pontual; lwr=limite inferior; upr=limite superior

####Predição com intervalo de confiânça

predict(mod1, newdata = dados_novos, interval = "confidence")
