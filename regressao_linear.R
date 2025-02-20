library(readxl)
dados <- read_excel("~/1UNISINOS/ALURA/RSTUDIO/Regress�o Linear/regressaolinear_r-master/dados.xlsx")
View(dados)

#Plotar os gr�ficos de dispers�o
plot(dados$area, dados$preco, main = "Diagrama de Dispers�o", xlab = "�rea", ylab = "Pre�o", pch = 19, col = "blue")
cor(dados$area, dados$preco)
cor.test(dados$area, dados$preco)

plot(dados$tempo, dados$preco, main = "Diagrama de Dispers�o", xlab = "Tempo", ylab = "Pre�o", pch = 10, col = "red")
cor(dados$tempo, dados$preco)     
cor.test(dados$tempo, dados$preco)

#Plotar o boxplot de pre�o
boxplot(dados$preco)

#Identificar a casa do outlier pelo pacote car
install.packages("car")
library(car)
Boxplot(dados$preco)

#Descobrir o valor do outlier pela casa dele
dados$preco[79]

#Descobrir quais s�o as casas do terceiro quartil (caixa de cima)
which(dados$preco > quantile(dados$preco, 0.75))

##Equa��o da reta

mod1 = lm(preco ~ area, data = dados)
mod1

## F�rmulada da reta para estimar o pre�o em raz�o da �rea. Substituir os coefs na f�rmula : preco = b0 + b1*area

preco_70 = 502347 + 7851*(70)
preco_70

## Outro modo de fazer a mesma previs�o

mod1$coefficients[[1]] + mod1$coefficients[[2]]*70

##Gerar o gr�fico disso

plot(dados$area, dados$preco, main = "Diagrama e reta", xlab = "�rea (m�)", ylab = "Pre�o (R$)", pch = 19, col = "blue") + abline(mod1, col = "red")

##Validar o modelo: verificar se a �rea influi no pre�o das casas

summary(mod1)

##Outro mp�todo de validar os dados
names(summary(mod1))

summary(mod1)$r.squared

##Verficar o tempo em rela��o ao tempo

mod2 = lm(preco ~ tempo, data = dados)
mod2

summary(mod2)

###Verificar os res�duos do modelo

plot(mod1$residuals)

identify(mod1$residuals, n = 2)
mod1$residuals[82]

###Remover uma posi��o desejada

dados_59 = dados[-59,]

###Remover duas casas desejadas

dados_59_82 = dados[c(-59, -82),]


###Teste de independ�ncia dos erros do modelo

install.packages("lmtest")
library(lmtest)

dwtest(mod1)

plot(mod1$fitted.values, mod1$residuals)
bptest(mod1)

plot(mod1, 2)

shapiro.test(mod1$residuals)

####Previs�o

dados_novos = data.frame(area = c(60, 70))
predict(mod1, newdata = dados_novos)####Vai retornar os valores estimados para o pre�o em fun��o das �reas requisitadas no vetor

####Previs�o de um intervalo


predict(mod1, newdata = dados_novos, interval = "prediction") #fit=estimativa pontual; lwr=limite inferior; upr=limite superior

####Predi��o com intervalo de confi�n�a

predict(mod1, newdata = dados_novos, interval = "confidence")
