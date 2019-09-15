###########################################################
#######       PGM522- Análise de Experimentos       ####### 
#######                Liliana Leitão               #######
###########################################################

#Definindo diretório
setwd("C:\\Users\\Liliana Leitão\\Desktop\\Análise de Experimentos\\R studio\\Rotinas R")

#Lista de exercício 2 - Questão 9

#Correlação e Regressão

#Criando vetores para análise
V1<-c (282, 618, 545, 603, 370, 598, 603, 646, 439, 541)
V2<-c (614, 357, 415, 373, 419, 346, 649, 439, 381, 423)
V3<-c (436, 380, 415, 427, 504, 303, 401, 294, 377, 291)
V4<-c (335, 349, 384, 495, 299, 256, 259, 272, 291, 341)
V5<-c (263, 521, 191, 212, 370, 568, 431, 487, 95, 671)

#Criando data.frame composto pelos vetores 
valores<-data.frame(V1,V2,V3,V4,V5);valores

#Explorando os dados
str(valores)

#Gráfico dispersão das variáveis
plot(valores)

#Boxplot das variáveis
boxplot(valores[,-c(V1,V2,V3,V4,V5)], horizontal=F)

#Coeficientes de Correlação
# Pearson
cor(valores)

#exportando para excel
write.csv2(valores, "CorPearson.csv")

# Spearman
cor((valores), method= "spearman")

# Kendall
cor((valores), method = "kendall")

#Gráfico de correlação
library(GGally)
p <- ggpairs(valores,
             lower=list(continuous=wrap("smooth", colour="red")),
             diag=list(continuous=wrap("barDiag", fill="blue")))
p <- p + theme(legend.position = "topright", 
               panel.grid.major = element_blank(), 
               axis.ticks = element_blank()) + ggtitle("Variáveis V1,V2,V3,V4,V5")
p

#Regressão
valores<-data.frame(V1,V2)

regressao<-lm(V1~V2, data=valores)
regressao  #coeficiente de intercept

#Modelo Regressão
abline(regressao)

#Coeficiente de determinação
cor(V1,V2)^2



