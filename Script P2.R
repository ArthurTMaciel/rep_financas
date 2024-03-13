
#'------------------------------------------------------------------------------
#' Script para executar os scripts da EAPI
#' Autor : Arthur Maciel, Leonardo Claudino, Henrique Rosseti
#' Data : 05/23
#'Atualização:
#'------------------------------------------------------------------------------

# Limpando ambiente ------------------------------------------------------------

rm(list = objects())

# Lista de pacotes a serem carregados-------------------------------------------

library(PerformanceAnalytics)
library(sandwich)
library(lmtest)
library(readxl)


# Codigo ------------------------------------------------------------------

JBS_V3 <- read_excel("JBS_V3.xlsx")
datasa=as.matrix(JBS_V3[,1])
datasa=as.Date(datasa)
datasa <- format(datasa, "%d %m %Y")
#"01/01/2013"  dia mes ano

fatoresrisco <- read_excel("fatoresrisco.xlsx")
View(fatoresrisco)
dim(fatoresrisco)
#year;month;day
datasf=as.Date(fatoresrisco$data)
View(datasf)
datasf <- format(datasf, "%d %m %Y")
cbind(datasa,datasf)

# Calcula os retornos dos ativos a partir dos preços de fechamento.
calret=function(y){
  return(diff(log(y)))
}

# Calcula os retornos diários dos ativos.
rtrno=calret(JBS_V3$Fechamento)
datasa=datasa[(-1)]

# Combina as datas dos fatores de risco e dos retornos para encontrar as datas correspondentes.
index=match(datasf,datasa)
cbind(datasa[index],datasf)


colnames(fatoresrisco)
View(fatoresrisco)

# Extrai os fatores de risco relevantes.
rf=fatoresrisco[,3]
Rm=fatoresrisco[,2]
SMB=fatoresrisco[,4]
HML=fatoresrisco[,5]
IML=fatoresrisco[,6]
WML=fatoresrisco[,7]

# Calcula os retornos excedentes dos ativos em relação à taxa livre de risco.
ria=rtrno[index]-rf

# Plot dos retornos dos ativos em relação ao tempo.
ts.plot(tail(ria,90))

#Divulgação do evento em 17/052017
dataevento="2017-05-17"

datainijane="2012-05-16"
datainijane <- as.Date(datainijane)
datainijane <- format(datainijane, "%d %m %Y")
datainijane
jandepois=22
#jandepois=350

# Configura datas
dt1ddps=as.Date(dataevento)+1
dt1ddps<- format(dt1ddps, "%d %m %Y")
dt22ddps=as.Date(dataevento)+jandepois
dt22ddps<- format(dt22ddps, "%d %m %Y")

dt1ddps
dt22ddps

# Filtra os dados para o período anterior e posterior ao evento e plota os gráficos correspondentes.
par(mfrow = c(2, 1))# Configura o layout do gráfico com duas linhas
indexant = datasf < dataevento & datasf > datainijane# Define uma condição para filtrar os dados
indexant = t(indexant)
ts.plot(ria[indexant])# Plota um gráfico de séries temporais com os dados filtrados pelo índice "indexant"
indexdep = dt22ddps <= dt1ddps & datasf >= datasf# Define uma condição para filtrar os dados
indexdep = t(indexdep)
ts.plot(ria[indexdep])# Plota um gráfico de séries temporais com os dados filtrados pelo índice "indexdep"
#####################################
#estimaçao do modelo de retornos

# Forma 1 - Retornos constantes.

meanret=mean(ria[indexant],na.rm=T)
anormalret=ria[indexdep]-meanret
anormalret=anormalret[is.na(anormalret)==F]
canormalret=cumsum(anormalret)
ts.plot(canormalret)
abline(h=meanret)
t1=t.test(canormalret)
t1
# Conclusão é que existe diferença estatisticamente significante da média, ela nao é igual a  zero. A informaçõ nao foi totalmente incorporada, dif entre retornos nos dif periodos.

# Forma 2 - Resíduos CAPM.

model1=lm(ria[indexant]~Rm[indexant])
alpha1=coef(model1)[1]
beta1=coef(model1)[2]
anormalretCAPM=ria[indexdep]-alpha1-beta1*Rm[indexdep]
predretCAPM=alpha1+beta1*Rm[indexdep]

ts.plot(cbind(anormalretCAPM,predretCAPM),col=1:2)
anormalretCAPM
anormalretCAPM=anormalretCAPM[is.na(anormalretCAPM)==F]
canormalretCAPM=cumsum(anormalretCAPM)
ts.plot(canormalretCAPM)

t2=t.test(canormalretCAPM)
t2

mi=rep(1,length(canormalretCAPM))

r1lm=lm(canormalretCAPM~mi-1)
summary(r1lm)

coeftest(r1lm)
coeftest(r1lm, vcov=kernHAC(r1lm))

# Forma 3 - Resíduos APT.

model2=lm(ria[indexant]~Rm[indexant]+SMB[indexant]+HML[indexant])
alpha_2=coef(model2)[1]
beta1_2=coef(model2)[2]
beta2_2=coef(model2)[3]
beta3_2=coef(model2)[4]
anormalretAPT=ria[indexdep]-alpha_2-beta1_2*Rm[indexdep]-beta1_2*SMB[indexdep]-beta1_2*HML[indexdep]
anormalretAPT

predretAPT=alpha_2+beta1_2*Rm[indexdep]+beta1_2*SMB[indexdep]+beta1_2*HML[indexdep]

ts.plot(cbind(anormalretAPT,predretAPT),col=1:2)

anormalretAPT=anormalretAPT[is.na(anormalretAPT)==F]
canormalretAPT=cumsum(anormalretAPT)
t2=t.test(canormalretAPT)
t2

mi=rep(1,length(canormalretAPT))

r2lm=lm(canormalretAPT~mi-1)
summary(r2lm)


coeftest(r2lm)
coeftest(r2lm, vcov=kernHAC(r2lm))