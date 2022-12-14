################################################################################
######################## prepara??o e ajuste dos dados #########################
################################################################################


# Limpando o environment 
rm(list=ls())  # Apagando todos os objetos


# Instalando os pacotes 
# install.packages("haven")
# install.packages("matrixStats")
# install.packages("caret")
# install.packages("dplyr") 


# Pacotes a serem usados 
library(haven) # utilizado para importar a base
library(matrixStats) # Utilizado para calcular o desvio padr?o
library(caret) # Utilizado para calcular com vari?ncias zero ou pr?ximo de zero, al?m disso, a matriz de correla??o 
library(dplyr) # Utilizada para ajudar a tirar as vari?veis pela m?dia


# Pode ser que um dos processos necessite de mais mem?ria
memory.limit(9999999999) # Aumentando a mem?ria de forma que n?o trave, aqui 
# al? de usar a mem?ria f?sica do computados, utiliza a mem?oria virtual, 


# Diret?rio
getwd() # onde estar o diret?rio de trabalho 
setwd("C:/Users/marci/Documents/MEGA/Mega/1. Bullying/Dados e do-file") # Mudando o diret?rio


# Importando a base
base <- read_dta("Dados R/Empatiafinal.dta")


# Criando algumas vari?veis
y1 <- base$PV1MATH
y2 <- base$PV1READ
y3 <- base$PV1SCIE
t1 <- base$ST207Q01HA
t2 <- base$ST207Q02HA
t3 <- base$ST207Q03HA
t4 <- base$ST207Q04HA
t5 <- base$ST207Q05HA


# Criando a f?rmula e, posteriormente, a matriz
f <- as.formula( ~ -1+PV1MATH+PV1READ+PV1SCIE+t1+t2+t3+t4+t5
                 +(norte+nordeste+sul+sudeste
                   +ano1+ano2+ano3
                   +poly(AGE,2,raw=F)
                   +ST004D01T
                   +edm3+edm4_5+edm6+edp3+edp4_5+edp6
                   +qltc1+qltc2+qltc3 
                   +poly(HOMEPOS,2,raw=F)
                   +poly(CULTPOSS,2,raw=F)
                   +ST034Q01TA+ST034Q02TA+ST034Q03TA+ST034Q04TA+ST034Q05TA+ST034Q06TA
                   +poly(WEALTH,2,raw=F)
                   +poly(RESILIENCE,2,raw=F)
                   +poly(HISEI,2,raw=F)
                   +efs+eft
                   +ST160Q02IA+ST160Q04IA+ST160Q05IA
                   +ST036Q05TA+ST036Q06TA
                   +ST181Q02HA+ST181Q03HA+ST181Q04HA
                   +ST183Q01HA+ST183Q02HA+ST183Q03HA
                   +ST123Q02NA+ST123Q03NA+ST123Q04NA
                   +ST038Q03NA+ST038Q04NA+ST038Q05NA+ST038Q06NA+ST038Q07NA+ST038Q08NA
                   +ST212Q01HA+ST212Q02HA+ST212Q03HA
                   +ST182Q03HA+ST182Q05HA
                   +poly(ST016Q01NA,2,raw=F)
                   +PA003Q01TA+PA003Q04HA+PA003Q05IA
                   +PA159Q01HA
                   +efft+effpg
                   +PA154Q01IA+PA154Q02IA+PA154Q04IA
                   +PA018Q03NA
                   +dcp1+dp1+ihca1+qps1
                   +lesc2+lesc3+lesc4+lesc5
                   +SC013Q01TA
                   +poly(TOTAT,2,raw=F)
                   +turma1+turma2+turma3
                   +SC017Q05NA+SC017Q07NA
                   +SC156Q05HA
                   +SC036Q01TA+SC036Q02TA+SC036Q03NA
                   +SC061Q07TA+SC061Q08TA+SC061Q09TA+SC061Q10TA
                   +SC052Q01NA+SC052Q03HA)^2
) 
# O -1 significa sem intercepto 
# e o )^3 significa intera??es de segunda ordem. As vari?veis que foram retiradas
# para n?o cair na armadilha de vari?vel dummy foram centro_oeste, grade7,
# qltc1, lesc1, SC003Q01TA. As vari?veis edm2 e edp2 n?o foi necess?rio, pois 
# ainda existe outro n?vel.
x1=model.matrix(f, base) # Gerando matrix atrav?s da f?rmula.


# Deletando vari?veis duplicadas
colunas_duplicadas_1 <- duplicated(t(x1)) # Colunas duplicadas
sum(colunas_duplicadas_1, na.rm=TRUE)
x2 <- x1[, !colunas_duplicadas_1] # Gerando newx sem os duplicados
colunas_duplicadas_2 <- duplicated(t(x2)) # Testando se tem colunas duplicadas
sum(colunas_duplicadas_2, na.rm=TRUE)
rm(f, colunas_duplicadas_1, colunas_duplicadas_2) # excluindo os excessos


# Retirando vari?veis com pequeno desvio padr?o
remover_col_1 <- nearZeroVar(x2, freqCut=95/5, uniqueCut=10, names=TRUE) # Gera a lista com o nome das colunas que tem vari?ncia zero ou pr?xima de zero.
# nearzeroVar <- nearZeroVar(x2, freqCut=95/5, uniqueCut=5, saveMetrics=TRUE) # Gera um data frame com colunas voltadas para a frequ?ncia, vari?ncia zero e quase zero.
# sum(nearzeroVar$nzv, na.rm=TRUE) # Contando a quantidade de vari?veis com vari?ncias pr?ximas de zero.
x2 <- x2[, !colnames(x2) %in% remover_col_1]
rm(remover_col_1)


# Verificando a variabilidade
dp <- colSds(x2) # Encontrando o desvio padr?o
# View(dp) # N?o h? nenhuma vari?vel com dp zero, devido ao processo acima.
rm(dp)


# Matriz de correla??o
cor_mat_x2 <- cor(x2) # gerando a matriz de correla??o
# View(cor_mat_x2_2)
indice_x2 <- findCorrelation(cor_mat_x2, cutoff=0.99, exact=F, names=F) # removendo a coluna 
# do indece que tem a correl??o maior ou igual a 0.99
remover_col_2 <- colnames(cor_mat_x2)[indice_x2]# retornando colunas que ser?o removidas
# CAso precise ver View(para_serem_removidos_x2_2)
x2 <- x2[,!colnames(x2) %in% remover_col_2] # Gerando um novo objeto, retirando as vari?veis com alta correla??o
rm(cor_mat_x2,indice_x2, remover_col_2)


# Criando o data.table para ser usado no DML
## De in?cio para trabalhar com o m?todo ? necess?rio criar um data.table, assim
## transformou-se a matriz de dados em data.table.
x3 <- as.data.table(x2)
# class(x3)
# View(x3)
## Antes de continuar tem que ajeitar o nome das vari?veis, assim torna-se v?lidos
names(x3) <- make.names(names(x3),unique = TRUE)


# Criando um data.table sem os tratamentos e as vari?veis de resultados
xe <- x3 # Replicandos os dados de X3
xe <- xe[,-c(1:8)] # Excluindo as vari?veis de desempenho e tratamento


# Salvando os dados processados
save.image("C:/Users/ma/Desktop/Bullying/Dados e do-file/Dados R/Vers?o final/Principal/Com transforma??es.RData")