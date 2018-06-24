#~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~
# Disciplina: Delineamento amostral e base estatistica para estudos em Botanica  #
# Prof. Luciana Franci (lucianafranci@gmail.com)                                 #
# Aula 1. Teste de hipóteses e análises univariadas                              #
#~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~


# 1. Definindo o diretorio ------------------------------------------------
#setwd

# 2. Teste de normalidade ----------------------------------------------------

#Gerando os dados: 30 valores com media igual a 15 e desvio padrao igual a 3
set.seed(12)
medida <- rnorm(300, mean = 15, sd = 3) #funcao rnorm gera uma amostra de observacoes de uma distribuicao normal
medida
max(medida)
min(medida)

# Fazendo um histograma
par(mfrow = c(1,2)) # para gerar graficos paralelos
hist(medida, xlim = c(5,25), main = "Histograma da Medida")

#Conferindo valores de media e desvio padrao
media <- mean(medida)
desvio <- sd(medida)

# Para centralizar o grafico podemos substrair a media dos dados e dividir pelo desvio padrao,
# assim, teremos media igual a 0 e 95% dos valores estarao entre 2 desvios padrao.
# Desse modo sera mais facil de comparar com uma curva normal

#normalizando os dados
medida.ajus <- (medida - media)/desvio

#Conferindo se media = 0 e desvio padrao = 1
mean(medida.ajus)
sd(medida.ajus)

#histograma com distribuição normal
hist(medida.ajus, xlim = c(-5,5), prob = T, main = "Histograma da Medida")
curve(dnorm(x, mean = 0,sd = 1),-4, 4, lwd = 2, col = "red", add = T) #funcao dnorm fornece a densidade probabilistica para cada valor de uma variavel normal

par(mfrow = c(1,1))

#Teste de Shapiro-Wilk para normalidade
shapiro.test(medida)
shapiro.test(medida.ajus)

#Teste de normalidade visual
par(mfrow = c(1,2))
hist(medida)
qqnorm(medida)
qqline(medida)

par(mfrow = c(1,1))

# 3. Qui-quadrado: teste de independencia ------------------------------------------------------------
data("iris")
head(iris)
summary(iris)
str(iris)

#Vamos converter a coluna Sepal.Width para categorica
#Vamos criar duas categorias: 'Sepala grande' e 'Sepala pequena', referentes ao valor da mediana, ou seja, maior ou menor que a mediana
summary(iris$Sepal.Width)

shapiro.test(iris$Sepal.Width)

iris$tamanho_sepala <- as.factor(
  ifelse(iris$Sepal.Width > 3,
         "Sepala grande",
         "Sepala pequena"))

# Agora vamos criar uma tabela de contigencia, ou seja, quantos maiores e menores cada especie possui
cont <- table(iris$tamanho_sepala, iris$Species)
cont

#Nossa hipotese nula e que a categoria de tamanho de sepala nao tem efeito sobre as especies
#Nossa hipotese alternativa e que a categoria de tamanho tem efeito sobre as especies

chi2 <- chisq.test(cont)
chi2

#Rejeita ou nao a hipotese nula?


levels(iris$Species)
#Fazendo um grafico para ver o resultado
par(mfrow = c(1,1))
boxplot(iris$Sepal.Width ~ iris$Species, ylab = "Largura de petala (cm)", xlab = "Especies de Iris",
        names = c("I. setosa", "I. versicolor", "I. virginica"), notch = TRUE)

# 4. Teste t: teste de medias entre duas amostras -----------------------------------------------------------------

#Nao pareado
#Vamos supor que temos um grupo tratamento no qual não regamos as plantas e um grupo controle que foi regado. Queremo saber se as taxas de crescimento em altura diferem entre tratamento e controle
set.seed(2)
tratamento <- rnorm(30, 0.5, 0.3)
tratamento
controle <- rnorm(30, 2.5, 1.5)
controle

plot(tratamento)
shapiro.test(tratamento)

plot(controle)
shapiro.test(controle)

#Nossa hipotese nula e que a media de crescimento em tamanho nao difere entre tratamento e controle
#Nossa hipotese alternativa e que a media de tamanho difere entre tratamento e controle

#Teste bicaudal, ou seja, apenas testando se as medias de crescimento diferem
teste_t_bicaudal <- t.test(tratamento, controle, paired = FALSE, alternative = "two.sided")
teste_t_bicaudal

#Teste unicaudal testando se a media de crescimento do tratamento e maior que a do controle
teste_t_unimaior <- t.test(tratamento, controle, paired = FALSE, alternative = "greater")
teste_t_unimaior

#Teste unicaudal testando se a media de crescimento do tratamento e menor que a do controle
teste_t_unimenor <- t.test(tratamento, controle, paired = FALSE, alternative = "less")
teste_t_unimenor

#Visualizando
boxplot(tratamento, controle, xlab = "Experimentos", ylab = "Crescimento (mm)",
        names = c("Tratamento", "Controle"), col = c("white", "grey"))

#Histograma
par(mfrow=c(1,2))
hist(tratamento)
hist(controle)
par(mfrow=c(1,1))

#Pareado
#Supondo que temos um experimento no qual queremos saber se apos expor algumas plantas ao sol, a media de tamanho aumenta em relacao ao tempo anterior

antes <- rnorm(30, 10, 1.3)
depois <- rnorm(30, 20, 1.5)

plot(antes)
shapiro.test(antes)

plot(depois)
shapiro.test(depois)

#Nossa hipotese nula e que a media dos tamanhos e igual antes e depois do tratamento 

#Teste bicaudal, ou seja, apenas testando se as medias de tamanho diferem
pareado_bicaudal <- t.test(depois, antes, paired = TRUE, alternative = "two.sided")
pareado_bicaudal

#Teste unicaudal testando se a media de tamanho depois e maior que a de antes
pareado_unimaior <- t.test(depois, antes, paired = TRUE, alternative = "greater")
pareado_unimaior

#Teste unicaudal testando se a media de tamanho depois e menor que a de antes
teste_t_unimenor <- t.test(depois, antes, paired = TRUE, alternative = "less")
teste_t_unimenor

#Visualizando
boxplot(antes, depois, ylab = "Tamanho (cm)",
        names = c("Antes", "Depois"), col = c("white", "grey"))

# 5. ANOVA ----------------------------------------------------------------
#Anova unifatorial (one-way ANOVA)

#Nossa hipotese nula e que a media de tamanho de petala nao difere entre especies
#Nossa hipotese alternativa e que a media de tamanho difere entre as especies

#Vamos testar se o comprimento de sepala varia entre as especies

uni_iris <- aov(Sepal.Length ~ Species, data = iris)
uni_iris

summary(uni_iris)

#Testando a normalidade dos residuos
shapiro.test(residuals(uni_iris))

#Testando a homocedasticidade dos residuos
plot(residuals(uni_iris))

#Para sabermos quais grupos diferem, podemos usar um teste post-hoc. O teste post-hoc para ANOVA é o Tukey HDS
TukeyHSD(uni_iris)

#Grafico para visualizar os resultados
boxplot(iris$Sepal.Length ~ iris$Species, ylab = "Comprimento de sepala (cm)", xlab = "Especies de Iris")

#ANOVA bifatorial
#Primeiro vamos abrir os dados "npk", no qual ha local de producao (block), aplicacao ou nao de N, P e K, e producao de peras em libras por plot
data("npk")
str(npk)

#Vamos investigar se a producao de peras foi influenciada pela aplicacao de NPK. Como a aplicacao foi aleatoria, precisamos testar se o local influencia o resultado
#Nossa hipotese nula e de que a producao nao foi influencia pela aplicacao de NPK.
multi_npk <- aov(yield ~ block + N*P*K, data = npk)
summary(multi_npk)

#Testando a normalidade dos residuos
shapiro.test(residuals(multi_npk))

#Conferindo quais locais e tratamentos diferiram
TukeyHSD(multi_npk)
plot(multi_npk)

#Visualizando o resultado
boxplot(yield ~ block, data = npk, xlab = "Blocos", ylab = "Produção de pêra por parcela")

# 6. Correlacao de Pearson ------------------------------------------------
#Supondo que estamos investigando se ha relacao entre a area foliar e a altura de plantas jovens de palmito 

#Vamos gerar alguns dados:
set.seed(1)
area_foliar <- sort(rnorm(50, 8, 2))
altura <- sort(rnorm(50, 10, 5))

#Usando a funcao 'cor' para fazer a correlacao
cor(area_foliar, altura, method = "pearson")

#Visualizando
plot(area_foliar ~ altura, xlab = "Altura (cm)", ylab = "Area foliar total (cm2)")

# 7. Regressão linear -----------------------------------------------------
#Temos um experimento de taxa de crescimento de algas em laboratorio so diferentes condicoes de intensidade de luz e queremos saber se o crescimento está relacionado com a luz. Isto e, se tem relacao causal!
#Baseado em  Fussmann et al. (2000) (https://ms.mcmaster.ca/~bolker/emdbook/lab1.html)
luz = c(20,20,20,20,21,24,44,60,90,94,101)
taxa_crescimento = c(1.73,1.65,2.02,1.89,2.61,1.36,2.37,2.08,2.69,2.32,3.67)

#Vamos explorar os dados:
plot(taxa_crescimento ~ luz, xlab = "Intensidade de luz", ylab = "Taxa de crescimento (mm)")

#Fazendo um modelo linear:
mod <- lm(taxa_crescimento ~ luz)
summary(mod)

#Testando as premissas:
shapiro.test(residuals(mod))
plot(residuals(mod))

#ou
plot(mod)

#Visualizando
plot(taxa_crescimento ~ luz, xlab = "Intensidade de luz", ylab = "Taxa de crescimento (mm)")
#Vamos colocar a linha da regressao ao grafico
abline(mod)
#Colocando a equacao da reta no grafico
text(40, 3.5, "Crescimento = 1.5 + 0.01Luz")
text(27.5, 3.3, expression(paste(r^2 ~ "=" ~0.47)))

# 8. Mann-Whitney ---------------------------------------------------------
data(iris)

#Vamos testar se o comprimento de petalas difere entre as especies Iris setosa e Iris versicolor

isetosa <- iris[iris$Species == "setosa", ]$Petal.Length
iversicolor <- iris[iris$Species == "versicolor", ]$Petal.Length

#Bicaudal
wilcox.test(isetosa, iversicolor, paired = FALSE, alternative = "two.sided")

#Unicaudal, setosa < versicolor
wilcox.test(isetosa, iversicolor, paired = FALSE, alternative = "less")

#Unicaudal, setosa > versicolor
wilcox.test(isetosa, iversicolor, paired = FALSE, alternative = "greater")

#Visualizando
boxplot(isetosa, iversicolor, names = c("Iris setosa", "Iris versicolor"),
        ylab = "Comprimento de pétala (cm)")

# 9. Wilcoxon -------------------------------------------------------------
#Supondo que medimos o tamanho em metros de plantas juvenis em 100 parcelas no tempo x e queremos saber se houve mudanca na média do tamanho do diametro dessas mesmas plantas no tempo x+1
#Vamos gerar alguns dados
set.seed(1)
parcelas_antes <- sample(1:10, size = 100, replace = TRUE)/100
parcelas_depois <- parcelas_antes + c(sample(1:20, size = 100, replace = TRUE)/100)

shapiro.test(parcelas_antes)
shapiro.test(parcelas_depois)

#Bicaudal
wilcox.test(parcelas_antes, parcelas_depois, paired = TRUE, alternative = "two.sided")

#Unicaudal, antes < depois
wilcox.test(parcelas_antes, parcelas_depois, paired = TRUE, alternative = "less")

#Unicaudal
wilcox.test(parcelas_antes, parcelas_depois, paired = TRUE, alternative = "greater")

#Visualizando, antes > depois
boxplot(parcelas_antes, parcelas_depois, names = c("Tempo x", "Tempo x + 1"),
                                                   ylab = "Média de tamanho de plantas por parcela")

# 10. Kruskal-Wallis -------------------------------------------------------
iris_petal <- kruskal.test(Petal.Length ~ Species, data = iris)
iris_petal

#Para sabermos quais grupos diferem, podemos fazer um teste post-hoc. Nesse caso usaremos o teste de Dunn. Esse teste tambem pode ser usado quando as amostras tem tamanhos diferentes 
#Para isso precisamos do pacote 'dunn.test'
#install.packages("dunn.test")
library(dunn.test)

#usaremos a correcao de bonferroni para controlar o erro do Tipo I. Esse metodo e conservador, ele divide o valor de alfa (0.05) pelo numero de fatores, no nosso caso sao as 3 especies
dunn.test(iris$Petal.Length, iris$Species, method = "bonferroni")

#Visualizando
boxplot(iris$Petal.Length ~ iris$Species, ylab = "Comprimento de sepala (cm)", xlab = "Especies de Iris")

# 11. Correlacao de Spearman ------------------------------------------------
data(iris)

#Testando a normalidade
shapiro.test(iris$Petal.Length)
shapiro.test(iris$Petal.Width)

#Vamos testar se há correlacao entre a largura e o comprimento das petalas do genero Iris
cor(iris$Petal.Length, iris$Petal.Width, method = "spearman")

#Visualizando
plot(iris$Petal.Length, iris$Petal.Width, xlab = "Comprimento de pétala", ylab = "Largura de pétala")

#Agora vamos ver como isso varia entre as especies
plot(iris$Petal.Length, iris$Petal.Width, pch = 16, col = c("red","green3","blue")[iris$Species],
     xlab = "Comprimento de pétala (cm)", ylab = "Largura de pétala (cm)")
legend("bottomright", legend=c("Iris setosa", "Iris versicolor", "Iris virginica"),
       col = c("red","green3","blue"),  box.lty = 0, pch = 16)