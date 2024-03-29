###########################################################
#######       PGM522- An�lise de Experimentos       ####### 
#######                Liliana Leit�o               #######
###########################################################

#Lista de exerc�cio 2 - Quest�o 10

#Simular um conjunto de dados com distribui��o normal com 10 elementos, media = 10, vari�ncia = 1000

#Criando Popula��o 1
Popula��o1<-rnorm(n=10, mean = 10, sd = 31.62);Popula��o1

#M�dia
mean(Popula��o1)
#Vari�ncia
var(Popula��o1)

#Criando Popula��o 2
Popula��o2<-rnorm(n=50, mean = 10, sd = 31.62);Popula��o2

#M�dia
mean(Popula��o2)
#Vari�ncia
var(Popula��o2)

#Criando Popula��o 3
Popula��o3<-rnorm(n=1000, mean = 10, sd = 31.62);Popula��o3

#M�dia
mean(Popula��o3)
#Vari�ncia
var(Popula��o3)

#boxplot das tr�s popula��es
boxplot(Popula��o1, Popula��o2,Popula��o3, 
        col= c("green","blue", "yellow"),
        ylab = "dados",
        main= "Popula��es")
legend("bottomleft", legend=c("Popula��o 1", "Popula��o 2", "Popula��o 3"),
       fill=c("green","blue", "yellow"), bty="n")
        

#col = c > colorir o gr�fico
#ylab = t�tulo do eixo y
#xlab = t�tulo do eixo x
#main = t�tulo do gr�fico
#legenda> "bottomright" (abaixo-direta), "bottom" (abaixo-centro), "bottomleft", "left", "topleft", "top", "topright", "right"e "center"


#Distribui��o normal 
curve(dnorm(x,10,31.62), 10-3*31.62,10+3*31.62)


#histograma com curva normal
hist(Popula��o1, col="green", prob=T)
curve(expr = dnorm(x,mean=10,sd=31.62),add=T, col="red",lwd=3) #lwd=espessura da linha

hist(Popula��o2, col="blue", prob=T)
curve(expr = dnorm(x,mean=10,sd=31.62),add=T, col="red",lwd=3) #lwd=espessura da linha

hist(Popula��o3, col="yellow", prob=T)
curve(expr = dnorm(x,mean=10,sd=31.62),add=T, col="red",lwd=3)

        
#Salvar como mensagem de texto
write.csv(Popula��o1, file = "Popula��o1")

#Lista 2 - Quest�o 11

#Simular duas amostras com distribui��o normal com tamanho populacional de 100.
#Popula��o A: media = 10, vari�ncia = 1000
#Popula��o B: media = 1000, vari�ncia = 10

#Criando Popula��o A
Popula��oA<-rnorm(n=100, mean = 10, sd = 31.62);Popula��oA

#Estat�stica descritiva
mean(Popula��oA)
var(Popula��oA)
median(Popula��oA)
#moda
d<-table(Popula��oA);d
d[d == max(d)]
max(Popula��oA)
min(Popula��oA)
#amplitude
menor<-min(Popula��oA);menor
maior<-max(Popula��oA);maior
amplitude<-maior-menor; amplitude
sd(Popula��oA)
#erro padr�o da m�dia
sd(Popula��oA)/sqrt(length(Popula��oA))
#coeficiente de varia��o
(sd(Popula��oA)/mean(Popula��oA))*100

#histograma popula��o A
hist(Popula��oA, col="pink", prob=T)
curve(expr = dnorm(x,mean=10,sd=31.62),add=T, col="red",lwd=3) #lwd=espessura da linha

#boxplot popula��o A
boxplot(Popula��oA, 
        col= c("pink"),
        ylab = "dados",
        main= "Popula��o A")

#Criando Popula��o B
Popula��oB<-rnorm(n=100, mean = 1000, sd = 3.162);Popula��oB

#Estat�stica descritiva
mean(Popula��oB)
var(Popula��oB)
median(Popula��oB)
#moda
E<-table(Popula��oB);E
E[E == max(E)]
max(Popula��oB)
min(Popula��oB)
#amplitude
menor<-min(Popula��oB);menor
maior<-max(Popula��oB);maior
amplitude<-maior-menor; amplitude
#desvio padr�o
sd(Popula��oB)
#erro padr�o da m�dia
sd(Popula��oB)/sqrt(length(Popula��oB))
#coeficiente de varia��o
(sd(Popula��oB)/mean(Popula��oB))*100

#histograma popula��o B
hist(Popula��oB, col="#c8a2c8", prob=T)
curve(expr = dnorm(x,mean = 1000, sd = 3.162),add=T, col="red",lwd=3) #lwd=espessura da linha

#"#c8a2c8" = cor lil�s

#boxplot popula��o B

boxplot(Popula��oA, 
        col= c("#c8a2c8"),
        ylab = "dados",
        main= "Popula��o B")