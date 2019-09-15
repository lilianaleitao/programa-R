###########################################################
#######       PGM522- Análise de Experimentos       ####### 
#######                Liliana Leitão               #######
###########################################################

#Lista de exercício 2 - Questão 10

#Simular um conjunto de dados com distribuição normal com 10 elementos, media = 10, variância = 1000

#Criando População 1
População1<-rnorm(n=10, mean = 10, sd = 31.62);População1

#Média
mean(População1)
#Variância
var(População1)

#Criando População 2
População2<-rnorm(n=50, mean = 10, sd = 31.62);População2

#Média
mean(População2)
#Variância
var(População2)

#Criando População 3
População3<-rnorm(n=1000, mean = 10, sd = 31.62);População3

#Média
mean(População3)
#Variância
var(População3)

#boxplot das três populações
boxplot(População1, População2,População3, 
        col= c("green","blue", "yellow"),
        ylab = "dados",
        main= "Populações")
legend("bottomleft", legend=c("População 1", "População 2", "População 3"),
       fill=c("green","blue", "yellow"), bty="n")
        

#col = c > colorir o gráfico
#ylab = título do eixo y
#xlab = título do eixo x
#main = título do gráfico
#legenda> "bottomright" (abaixo-direta), "bottom" (abaixo-centro), "bottomleft", "left", "topleft", "top", "topright", "right"e "center"


#Distribuição normal 
curve(dnorm(x,10,31.62), 10-3*31.62,10+3*31.62)


#histograma com curva normal
hist(População1, col="green", prob=T)
curve(expr = dnorm(x,mean=10,sd=31.62),add=T, col="red",lwd=3) #lwd=espessura da linha

hist(População2, col="blue", prob=T)
curve(expr = dnorm(x,mean=10,sd=31.62),add=T, col="red",lwd=3) #lwd=espessura da linha

hist(População3, col="yellow", prob=T)
curve(expr = dnorm(x,mean=10,sd=31.62),add=T, col="red",lwd=3)

        
#Salvar como mensagem de texto
write.csv(População1, file = "População1")

#Lista 2 - Questão 11

#Simular duas amostras com distribuição normal com tamanho populacional de 100.
#População A: media = 10, variância = 1000
#População B: media = 1000, variância = 10

#Criando População A
PopulaçãoA<-rnorm(n=100, mean = 10, sd = 31.62);PopulaçãoA

#Estatística descritiva
mean(PopulaçãoA)
var(PopulaçãoA)
median(PopulaçãoA)
#moda
d<-table(PopulaçãoA);d
d[d == max(d)]
max(PopulaçãoA)
min(PopulaçãoA)
#amplitude
menor<-min(PopulaçãoA);menor
maior<-max(PopulaçãoA);maior
amplitude<-maior-menor; amplitude
sd(PopulaçãoA)
#erro padrão da média
sd(PopulaçãoA)/sqrt(length(PopulaçãoA))
#coeficiente de variação
(sd(PopulaçãoA)/mean(PopulaçãoA))*100

#histograma população A
hist(PopulaçãoA, col="pink", prob=T)
curve(expr = dnorm(x,mean=10,sd=31.62),add=T, col="red",lwd=3) #lwd=espessura da linha

#boxplot população A
boxplot(PopulaçãoA, 
        col= c("pink"),
        ylab = "dados",
        main= "População A")

#Criando População B
PopulaçãoB<-rnorm(n=100, mean = 1000, sd = 3.162);PopulaçãoB

#Estatística descritiva
mean(PopulaçãoB)
var(PopulaçãoB)
median(PopulaçãoB)
#moda
E<-table(PopulaçãoB);E
E[E == max(E)]
max(PopulaçãoB)
min(PopulaçãoB)
#amplitude
menor<-min(PopulaçãoB);menor
maior<-max(PopulaçãoB);maior
amplitude<-maior-menor; amplitude
#desvio padrão
sd(PopulaçãoB)
#erro padrão da média
sd(PopulaçãoB)/sqrt(length(PopulaçãoB))
#coeficiente de variação
(sd(PopulaçãoB)/mean(PopulaçãoB))*100

#histograma população B
hist(PopulaçãoB, col="#c8a2c8", prob=T)
curve(expr = dnorm(x,mean = 1000, sd = 3.162),add=T, col="red",lwd=3) #lwd=espessura da linha

#"#c8a2c8" = cor lilás

#boxplot população B

boxplot(PopulaçãoA, 
        col= c("#c8a2c8"),
        ylab = "dados",
        main= "População B")