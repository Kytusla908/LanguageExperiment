# aritmetica elemental-----
a <- 4
b <- 5
c <- a/b

# funcions incorporades-----
log10(c)
exp(c)
sin(c)

# vectors i operacions amb vectors-----
x <- c(1,2,3)
y <- c(4,5,6)

# metodes estadistics------
mean(x)
sd(x)
max(y)
min(x)

# sequencies i repeticions------
v1 <- c(1:15)
v2 <- seq(0,3*pi,0.1)
sinv2 <- sin(v2)

#grafics-----
plot(v2,sinv2)
lines(v2,cos(v2),col='red')

# matrius-------
m <- x %*% t(y)
class(m)
class(x)
m2 <- matrix(data=c(4,8,12,5,10,15,6,12,18),nrow=3,ncol=3)
m3 <- array(data=1:12,dim=c(2,2,3))

#dataframe-------
sexe     <- rep(c('home','dona'),each=5)
edat     <- c(19,21,32,28,26,19,22,20,38,26)
diabetic <- c(1,1,0,0,0,0,1,0,1,0)
pes      <- c(54,67,74.2,69,58,62,90.5,NA,65,66) 
mydata   <- data.frame(sexe,edat,diabetic,pes) #Això genera un nou set de dades 

#Muntar un data.frame------
sexe <- rep(c('O1','O2','O3'),each=5)
trac <- c(rep(1,8),rep(2,8),rep(3,8))                
resp <- c(4,2,6,6,5,6,2,6,7,6,5,7,6,4,7,5,9,12,9,11,10,11,9,10)    
myData <- data.frame(trac,resp)

#Operacions dataframe-------
mydata$pes
mydata[,1]
mydata[2,4]
mydata[2:5,4]
mydata[2:5,3:4]
mydata$pes*100

# Emmagatzemar les dades, el fitxer es pot obrir amb Excel---------------------------
write.table(mydata,'mydata.csv',sep=';',dec=',',row.names=F)

rm(c) # Elimina la variable c
rm(a,b) # Elimina les variables a i b
# rm(list=ls()) Si l'executem esborra totes les varibles de memoria

# ajuda del metode t.test
?t.test         
help(t.test)

#Entrada de dades a ma-------------------------------------------------
trac <- c(rep(1,8),rep(2,8),rep(3,8))                
resp <- as.factor(c(4,2,6,6,5,6,2,6,7,6,5,7,6,4,7,5,9,12,9,11,10,11,9,10))    
myData <- data.frame(trac,resp)

#Importar taula de dades---------------------------------------------------------------
dades <- read.table('ph.csv',sep=';',dec=',',header=T)

#boxplot
boxplot(dades$oisnet, dades$issatop, names=colmanes(dades), col=c('green','cyan'))

#visualitzem els primers registres
head(ph)

#visualitzem tot el data.frame
View(ph)

#Comparacio de variancies poblacionals amb un test F-> MAI amb dades APARELLADES-------
var.test(dades$Normals,dades$Preclam)

#Comparacio de mitjanes poblacionals amb un test t ----------
t.test(dades$Normals,dades$Preclam,var.equal=T,paired = T,conf.level = 0.95,alternative='two.sided') 
t.test(dades$Normals,dades$Preclam,mu=14)

#CARREGA DE LLIBRERIRES-------------------------------------
library(car) #Necessari xq conté la formula de leveneTest
library(agricolae)  # Llibreria necessari per executar Tuckey
library(mixlm)
library(pwr)

#Definició d'una dada com a factor--------------------
myData[,1]  <- as.factor(myData[,1])
myData$trac <- as.factor(myData$trac)

#Nombre significatius de cada test, el últim  és el global (suma 1 cada vegada
#que una de les comparacions no s'accepta la H0, es a dir, error de tipus I)
nS_G

# Descriptiva global i per grups----------------------------------------------
summary(myData$trac)                 # rèpliques en cada nivell
summary(myData$resp)                 # summary global
by(myData$resp,myData$trac, summary) # summary per nivell
by(myData$resp,myData$trac, sd)      # desv. estandard per nivell

# ANOVA DE UN FACTOR----------------------------------------------------------------------------------------------------
data.aov <- aov(resp ~ trac, myData)  #~ ->Del set myData, la variable resp està modelada per trac           
anova(data.aov) 

# Estimacions dels parametres ANOVA----------------------------
model.tables(data.aov,type='mean') #Mitjana (mu)

model.tables(data.aov,type='effects') #Alfa (Distància a la mitjana global)

confint(aov(resp~trac-1,myData))

# Test d'homocedasticitat (igualtat de variàncies)----------------------------------------------
bartlett.test(resp ~ trac, myData)
plot(dades1.aov,which = 1) #GRAFIC DE RESIDUS

library(car) #Necessari xq conté la formula de leveneTest
leveneTest(resp ~ trac, myData)

# Test de normalitat SOBRE ELS RESIDUS (NO sobre les dades originals!)-------------------------
shapiro.test(data.aov$residuals)
plot(data.aov,which=2) #QQPLOT
qqnorm(data.aov$residuals,main='QQ-plot',xlab='Quantils teòrics',ylab='Quantils observats')
qqline(data.aov$residuals)


# COMPARACIONS MULTIPLES-------------------------------------------------------------------------
TukeyHSD(data.aov)                          # Paquet base
library(agricolae)                          # Llibreria necessari per executar Tuckey
HSD.test(data.aov,"trac",group=T,console=T) # Forma alternativa de fer Tuckey (més complet)
qtukey(0.95,nmeans=3,df=(21),nranges=1,lower.tail=T)*sqrt(54.25/21/8)
LSD.test(data.aov,"trac",group=T,console=T)


#MODELS ALEATORIS-------------------------------------------------------------------------

library(nlme)

#Calcular les components de la variància 
data.lme <- lme(resp ~ 1, random = ~ 1 | trac, data = myData,method = "REML")
varcomp   <- VarCorr(data.lme)
varcompN  <- as.double(varcomp)

#Calcular els percentatges de variabilitat
paste('Component variància factor',  round(varcompN[1]/sum(varcompN[1:2])*100,4),'%')
paste('Component variància residual',round(varcompN[2]/sum(varcompN[1:2])*100,4),'%')

# OBTENCIO MSE I sigmaA---------------------------------------------
taulaAnova <- anova(data.aov)
MSE <-  taulaAnova[2,3]
sigmaA <- (taulaAnova[1,3]-MSE)/((taulaAnova[2,1]+taulaAnova[1,1]+1)/(taulaAnova[1,1]+1))
paste('Component variància factor',  round(sigmaA/(sigmaA+MSE)*100,4),'%')
paste('Component variància residual',round(MSE/(sigmaA+MSE)*100,4),'%')

#Alternativa per obtenir MSE i sigmaA
library(mixlm)
dades9.lm<-lm(densitat~r(operari),dades9)
Anova(dades9.lm,type='III')

dades9.lm<-mixlm::lm(densitat~r(operari), data=dades9)
mixlm::Anova(dades9.lm,type='III')

#CALCUL DE MIDA MOSTRAL--------------------------------------------------------------------

#pwr.anova.test(k = , n = , f = , sig.level = , power = ) 
# f = delta/sigma, on delta és la diferencia que és vol detectar 
# entre la mitjana d'un grup i la mitjana global
library(pwr)
fval<-2/2.6
pwr.anova.test(k=3,f0=fval,sig.level=0.05,power=0.8) #parametre cohen
fval<-1/2.6
pwr.anova.test(k=3,f=fval,sig.level=0.05,power=0.8)

n<-15
a<-3
alfa<-0.05
delta<-1.5
MSE<-taulaAnova[2,3]
fval<-sqrt((2*delta^2)/(a*MSE))


#KRUSKAL-WALLIS--------------------------------------------------------
x   <- c(2.9, 3.0, 2.5, 2.6, 3.2) # normal subjects 
y   <- c(3.8, 2.7, 4.0, 2.4) # with obstructive airway disease 
z   <- c(2.8, 3.4, 3.7, 2.2, 2.0) # with asbestosis 
all  <- c(x, y, z) 
grup <- factor(rep(1:3, c(5,4,5)), labels = c("Normals", "EPOC", "Asbestosi")) 
dades1<-data.frame(grup,all)
kruskal.test(all, grup)
kruskal.test(dades1$all,dades1$grup)

#(CAL CARREGAR AGRICOLAE)
library(agricolae)
data.kru <- kruskal.test(efic~temp, dades1)
kruskal.test(efic~temp, dades1)
kruskal(dades1$efic,dades1$temp,p.adj="bonferroni",console=T)

data.kru <- kruskal.test(all~grup, dades1)
kruskal.test(all~grup, dades1)
kruskal(dades1$all,dades1$grup,p.adj="bonferroni",console=T)



#ANOVA DE 2 O MÉS FACTORS CREUATS----------------------------------
anticos<-c(6.1,8.2,6.3,6.5,7.5,11.0,9.5,7.1,6.6,8.8,6.3,6.6,4.3,5.2,5.6,6.2,3.6,5.2,4.4,5.6,2.9,5.1,3.5,10.2,4.0,
           4.9,3.1,7.1,2.3,12.4,4.0,3.8,2.2,3.0,2.3,3.0,2.1,3.7,2.5,3.6,1.8,3.8,2.4,3.1,2.3,2.9,2.2,3.3)
vacuna<-gl(4,12)
dosis<-factor(rep(1:3,times=16))
immuno<-data.frame(anticos,vacuna,dosis)
rbind(head(immuno),tail(immuno))  #rbind simplement uneix les dues columnes que volem veure

immuno.aov<-aov(anticos~vacuna+dosis+vacuna:dosis,immuno)
immuno.aov<-aov(anticos~vacuna*dosis,immuno)
'Si no vols la interacció poses +' -> immuno.aov<-aov(anticos~vacuna+dosis,immuno) 
#CAL COMPROVAR PREMISSES
anova(immuno.aov)
interaction.plot(immuno$vacuna,immuno$dosis,immuno$anticos)

#Estimació de paràmetres
model.tables(immuno.aov,type='mean')
model.tables(immuno.aov,type = 'effect')

#Comparacions múltiples
library(agricolae)
HSD.test(immuno.aov,'vacuna',console=T)
HSD.test(immuno.aov,'dosis',console=T)


#ANOVA 2 FACTORS AMB INTERACCIÓ SIGNIFICATIVA----------------------------------
temps<-c(4.35,3.86,4.62,5.14,4.8,3.71,3.9,3.4,5.97,6.05,6.32,
         6.07,1.35,1.62,0.34,0.63,1.08,1.51,1.61,0.94,0.84,0.81,1.67,0.96)
admin<-gl(2,12)
farmac<-factor(rep(rep(1:3,each=4),2))
somnol<-data.frame(temps,admin,farmac)
summary(somnol)
interaction.plot(somnol$farmac,somnol$admin,somnol$temps)
somnol.aov<-aov(temps~farmac*admin,somnol)
somnol.aov<-aov(temps~farmac+admin+farmac:admin,somnol)
bartlett.test(temps~interaction(admin,farmac),somnol)
plot(somnol.aov,which=1)
shapiro.test(somnol.aov$residuals)
plot(somnol.aov,which=2)
anova(somnol.aov)

library(agricolae)
HSD.test(somnol.aov,c('admin','farmac'),console=T)

#TRANSFORMACIÓ XQ NO NORMALITAT NI HOMOSCEDASTICITAT--------------------
immuno.aov.trans<-aov(log(anticos)~vacuna*dosis,immuno)
anova(immuno.aov.trans)
shapiro.test(immuno.aov.trans$residuals)
plot(immuno.aov.trans,which=2)
bartlett.test(log(anticos)~interaction(vacuna,dosis),immuno)
plot(immuno.aov.trans,which=1)


#STRINGS--------------------------
dades<-read.table('Hexobarbital.csv',header = T,sep=';',stringsAsFactors = T)


#UNA SOLA RÈPLICA PER NIVELL D'INTERACCIÓ--------------------------------
anticos<-c(0.31,0.82,0.43,0.45,0.36,0.92,0.44,0.56,0.22,0.30,0.23,0.30)
vacuna<-gl(3,4)
dosis<-factor(rep(1:4,times=3))
immuno1<-data.frame(anticos,vacuna,dosis)
immuno1.aov<-aov(anticos~vacuna+dosis,immuno1)
anova(immuno1.aov)


#ANOVA 2 FACTORS ALEATORIS--------------------------------------
compt<-c(343,344,341,339,365,364,359,361,321,325,317,322)
prep<-gl(3,4)
analist<-factor(rep(rep(1:2,each=2),3))
dades<-data.frame(prep,analist,compt)
dades
res.aov<-aov(compt~prep*analist,data=dades)
anova(res.aov)
analist

#MODIFICACIO TAULA A MA PER A 2 FACTORS ALEATORIS--------------------------------------
#MSE
taula<-anova(res.aov)
taula[1,4]<-taula[1,3]/taula[3,3]
taula[2,4]<-taula[2,3]/taula[3,3]
#P-VALORS
taula[1,5]<-1-pf(taula[1,4],taula[1,1],taula[3,1])
taula[2,5]<-1-pf(taula[2,4],taula[2,1],taula[3,1])
taula

#PACKAGE MIXLM dona els valors ja correctes (VC=components de la variància)
library(mixlm)
Anova(lm(compt~r(prep)*r(analist),data=dades),type='III')
#Alternativament
    dades.lm<-lm(compt~r(prep)*r(analist),data=dades)
    Anova(dades.lm,type='III')


# MODEL RESTRINGIT-------------------------------------
taula[1,4]<-taula[1,3]/taula[3,3]
taula[1,5]<-1-pf(taula[1,4],taula[1,1],taula[3,1])

library(mixlm)
Anova(lm(adhesio~cell*r(experiment),data=myData,unrestricted=F),type='III')
  #Ojo el 'unrestricted'

# Funcio Anova del package mixlm----------------------------------
Anova(aov(anticos~vacuna*dosis,immuno_1),type='III')
Anova(aov(anticos~dosis*vacuna,immuno_1),type='III')


#FACTORS JERARQUITZATS-------------------------
estres <- c(rep(1,9),rep(2,9))
gabia  <- c(rep(1,3),rep(2,3),rep(3,3),rep(1,3),rep(2,3),rep(3,3))                
glic   <- c(0.55,0.61,0.49,0.59,0.65,0.39,0.48,0.59,0.41,0.11,0.20,0.09,0.12,0.09,0.09,0.22,0.30,0.20) 
myData$estres <- as.factor(myData$estres)
myData$gabia <- as.factor(myData$gabia)
myData <- data.frame(estres,gabia,glic)
data.aov <- aov(glic ~ estres + gabia %in% estres, myData)  
auxAnova <- summary(data.aov)[[1]]                                  # com el segon factor (niat) es aleatori 
auxAnova[1,4] <- auxAnova$Mean[1]/auxAnova$Mean[2]                  # calcula la F fent el quocient MSA/MSB(A)
auxAnova[1,5] <- 1-pf(auxAnova[1,4], auxAnova$Df[1],auxAnova$Df[2]) # calcul del p-valor
auxAnova

  # alternativa, utilitzar mixlm
library(mixlm)
results1 <- lm(glic ~ estres + r(gabia) %in% estres, unrestricted=F, myData)
Anova(results1,type="III")



#ANOVA FIX FIX ALEATORI(NIAT)------------------------------------
        #Exemple 4.2
ex2Data <- read.table('Exemple 4.2.csv',sep=";",header=TRUE, dec=",", na.strings=' ');
ex2Data[, 1:3] <- lapply(ex2Data[,1:3],as.factor)

  #2. aov table and correction
ex2.aov <- aov(resp ~ reagent * temp + batch %in% reagent + batch %in% reagent:temp, ex2Data)
auxAnova <- anova(ex2.aov) 

  #CORRECCIONS: C factor is random and nested in A 
auxAnova[1,4]  <- auxAnova$Mean[1]/auxAnova$Mean[4]                               
auxAnova[1,5]  <- 1-pf(auxAnova[1,4], auxAnova$Df[1],auxAnova$Df[4])              
auxAnova[2,4]  <- auxAnova$Mean[2]/auxAnova$Mean[5]                               
auxAnova[2,5]  <- 1-pf(auxAnova[2,4], auxAnova$Df[2],auxAnova$Df[5])              
auxAnova[3,4]  <- auxAnova$Mean[3]/auxAnova$Mean[5]                              
auxAnova[3,5]  <- 1-pf(auxAnova[3,4], auxAnova$Df[3],auxAnova$Df[5])
model.tables(ex2.aov)
  
  #3. mixlm version
results2 <- lm(resp ~ reagent * temp + r(batch)%in%reagent+r(batch)%in%reagent:temp, unrestricted=F, ex2Data)
Anova(results2,type="III")

  #4. lmer version
ex2Data$batch2 <- as.factor(c(1,1,1,2,2,2,1,1,1,2,2,2,3,3,3,4,4,4,3,3,3,4,4,4,5,5,5,6,6,6,5,5,5,6,6,6))
ex2.lmer       <- lmer(resp ~ reagent * temp +(1|batch2) + (1|batch2:temp),ex2Data)
summary(ex2.lmer)
anova(ex2.lmer)


#ANOVA FIX FIX ALEATORI(NIAT a interacció) | A,B,C(A:B)------------------------------------
          #Exemple 4.3
ex3Data <- read.table('Exemple 4.2.csv',sep=";",header=TRUE, dec=",", na.strings=' ')
data.aov3 <- aov(resp ~ reagent * dilut + batch %in% reagent:dilut, ex3Data) 
# batch %in% (reagent:dilut)
auxAnova       <- anova(data.aov3)
auxAnova[1,4]  <- auxAnova$Mean[1]/auxAnova$Mean[4]
auxAnova[1,5]  <- 1-pf(auxAnova[1,4], auxAnova$Df[1],auxAnova$Df[4])
auxAnova[2,4]  <- auxAnova$Mean[2]/auxAnova$Mean[4]
auxAnova[2,5]  <- 1-pf(auxAnova[2,4], auxAnova$Df[2],auxAnova$Df[4])
auxAnova[3,4]  <- auxAnova$Mean[3]/auxAnova$Mean[4]
auxAnova[3,5]  <- 1-pf(auxAnova[3,4], auxAnova$Df[3],auxAnova$Df[4])
auxAnova

model.tables(data.aov3,type='effects')$tables$reagent # alpha_i
model.tables(data.aov3,type='effects')$tables$dilut   # beta_j
model.tables(data.aov3,type='effects')$tables$"reagent:dilut" # alpha beta_ij


#ANOVA A,B(A),C(B(A))------------------------------------
      #Exemple 4.4
ex4Data <- read.table('Exemple 4.2.csv',sep=";",header=TRUE, dec=",", na.strings=' ')
data.aov4 <- aov(resp ~ reagent+formt%in%reagent + batch%in%(reagent/formt),ex4Data)
  #Equivalent syntax -> batch%in%(formt%in%reagent)
data.aov4 <- aov(resp ~ reagent+formt%in%reagent + batch%in%(formt%in%reagent),ex4Data)

auxAnova <- anova(data.aov4)
auxAnova[1,4]<-auxAnova[1,3]/auxAnova[3,3]
auxAnova[1,5]<-1-pf(auxAnova[1,4],auxAnova[1,1],auxAnova[3,1])
auxAnova[2,4]<-auxAnova[2,3]/auxAnova[3,3]
auxAnova[2,5]<-1-pf(auxAnova[2,4],auxAnova[2,1],auxAnova[3,1])
auxAnova

#DADES EN BLANC (NA.STRINGS=)-------------------------------------------------
  #En alguns casos els valors buits no es posen NA però R només les pot llegir com a NA
dades <- read.table('Problema_T4_3.csv',sep=";",header=T, dec=",", na.strings=' ',stringsAsFactors = T)
  #Fòrmula en buble -> Per a i=1:3 per a columnes de dades, converteix-los en factors
for ( i in 1:3) dades[,i]<-as.factor(dades[,i])

#MODELS AMB MESURES REPETIDES (1WT)---------------------------------
    #1. Import csv
ex5Data <- read.table('Exemple 4.5.csv',sep=";",header=TRUE, dec=",", na.strings=' ');

    #2. Load library ez
library(ez)

    #3. Defining factors
ex5Data[,1] <- as.factor(ex5Data[,1])
ex5Data[,2] <- as.factor(ex5Data[,2])

    #4. Repeated measure
ezANOVA(data=ex5Data,within=.(time), 
        wid=.(indiv),
        between=NULL, 
        dv=.(measure),
        detailed=T,
        type=3) 

#MODELS AMB MESURES REPETIDES (1WT+1BT)---------------------------------
    #1. Import dataset
ex6Data <- read.table('Exemple 4.6.csv',sep=";",header=TRUE, dec=",", na.strings=' ');

    #2. Load library
library(ez)
  
    #3. Defining factors
ex6Data[,2] <- as.factor(ex6Data[,2])    # factor treatment = BS
ex6Data[,3] <- as.factor(ex6Data[,3])    # factor time      = WS
ex6Data[,5] <- as.factor(ex6Data[,5])    # individual. 5th col doest not restatr codes

    #4. Individual nested to treatment: 1BS+ 1WS
ezANOVA(data=ex6Data,within=.(time), 
        wid=.(indiv2),
        between=.(treatment), 
        dv=.(measure),
        detailed=TRUE,
        type=3) 

    #5. using Anova from car for Levene's test 
library(car) 

  # define subset at time 1 and perform test
dataT <- ex6Data[ex6Data$time==1,]
leveneTest(measure~treatment,dataT)  

  # define subset at time 2 and perform test
dataT <- ex6Data[ex6Data$time==2,]
leveneTest(measure~treatment,dataT)  

  # define subset at time 3 and perform test
dataT <- ex6Data[ex6Data$time==3,]
leveneTest(measure~treatment,dataT)  

    #6. using ezPlot for a descriptive summary
group_plot <- ezPlot( data =ex6Data , 
                      dv = .(measure) , 
                      wid = .(indiv2), 
                      between = .(treatment) , within=.(time),
                      x = .(treatment) , 
                      do_lines = FALSE , x_lab = 'Group' , split=.(treatment),
                      y_lab = 'RT (ms)' )
    #Show the plot. 
print(group_plot)
group_descriptives <- ezStats(data =ex6Data,dv = .(measure),wid = .(indiv2),between = .(treatment),within=.(time))
    #Show the descriptives. 
print(group_descriptives)


#MODELS LINEALS MIXTES (1WT)----------------------------------
library(nlme)
    
    #MODEL SIMETRIA COMPOSADA (ESFERICITAT)
lme_fit1 <- lme(measure~time,random=~1|indiv,data=ex5Data,correlation=corCompSymm())
summary(lme_fit1)
anova(lme_fit1)

    #MODEL AUTO-REGRESSIVA
lme_fit2 <- lme(measure~time,random=~1|indiv,data=ex5Data,correlation=corAR1())
summary(lme_fit2)
anova(lme_fit2)

    #MODEL TOTALMENT LLIURE
lme_fit3 <- lme(measure~time,random=~1|indiv,data=ex5Data,correlation=corSymm())
summary(lme_fit3)
anova(lme_fit3)


#MODELS LINEALS MIXTES (1WT+1BT)----------------------------------
lme_fit1 <- lme(measure~time*treatment,random=~1|indiv2,data=ex6Data,correlation=corCompSymm())
summary(lme_fit1)
anova(lme_fit1)

lme_fit2 <- lme(measure~time*treatment,random=~1|indiv2,data=ex6Data,correlation=corAR1())
summary(lme_fit2)
anova(lme_fit2)

lme_fit3 <- lme(measure~time*treatment,random=~1|indiv2,data=ex6Data,correlation=corSymm())
summary(lme_fit3)
anova(lme_fit3)


#REGRESSIÓ LINEAL MÚLTIPLE-------------------------------------------------
library(datasets); data(swiss); library(car)
scatterplotMatrix(swiss,main = "Swiss data")
swiss.lm<-lm(Fertility~.,data=swiss)
  #el "." es un caracter comodin per evitar haver d'estriure totes les variables independents
summary(swiss.lm)

  #Càlcul manual (Dona el mateix que dona lm)
  X<-as.matrix(swiss[,2:6])
  ones<-rep(1,nrow(swiss))
  X<-cbind(ones,X)
  solve(t(X)%*%X)%*%t(X)%*%swiss[,1]


#Comparació de dos models (regressions) lineals--------------------------------
swiss.lm.full<-lm(Fertility~Agriculture+Examination+Education+Catholic+Infant.Mortality,data=swiss)
swiss.lm.1<-lm(Fertility~Examination+Education+Catholic+Infant.Mortality,data=swiss)
anova(swiss.lm.1,swiss.lm.full)


swiss.lm.full<-lm(Fertility~Agriculture+Examination+Education+Catholic+Infant.Mortality,data=swiss)
swiss.lm.2<-lm(Fertility~Examination+Education+Catholic,data=swiss)
anova(swiss.lm.2,swiss.lm.full)


#Variància explicada per la regressió---------------------------
R2<-SSR/(SSR+SSE)
R2

R<-cor(swiss[,1],predict(swiss.lm))
R^2


#Predicció i Interval de confiança-----------------------------
confint(swiss.lm,level=0.95)

predict(swiss.lm,as.data.frame(swiss[1,2:6]))
predict(swiss.lm,as.data.frame(swiss[1,2:6]),interval='confidence')
predict(swiss.lm,as.data.frame(swiss[1,2:6]),interval='prediction')
new<-data.frame('Agriculture'=50,'Examination'=10,'Education'=10,'Catholic'=5,'Infant.Mortality'=2)
predict(swiss.lm,new,interval='confidence')


#Multicol·linealitat (mirem correlacions 2 a 2)---------------------------------
cor(swiss[,2:6])
library(corrgram)
corrgram(swiss[,2:6])

  #VIF
  vif(swiss.lm)

#Validació per conjunt entrenament i conjunt validació-----------------------------------------
library(lasso2)
data("Prostate")
head(Prostate)
set.seed(1234)

  #Dividir la mostra en 2 grups aleatòriament
train = sample(1:nrow(Prostate), nrow(Prostate)/2)
trainP<-Prostate[train,]
testP<-Prostate[-train,]

  #Ajust del model sobre conjunt entrenament
fit1.lm<-lm(lcavol~.,trainP)
summary(fit1.lm)

  #Càlcul de la predicció sobre els mateixos individus
pr1<-predict(fit1.lm)
(rmse<-sqrt(sum((trainP$lcavol - pr1)^2)/(48-8-1)))
plot(trainP$lcavol,pr1,xlab='Valor observat',ylab='Valor predit')

mean((trainP$lcavol - pr1)^2) #Promig de l'error de predicció (=SSE=errors quadràtics de l'error)

  #Càlcul de la predicció però amb el CONJUNT VALIDACIÓ
pr2<-predict(fit1.lm,newdata=testP[,-1])     
(rmse<-sqrt(sum((testP$lcavol - pr2)^2)/(49-8-1)))
plot(testP$lcavol,pr2)

cor(pr2,testP[,1])^2 #R^2

mean((testP$lcavol - pr2)^2) #Promig de l'error de predicció (=SSE=errors quadràtics de l'error)


#Validació creuada-------------------------------------
library(caret)
train.control <- trainControl(method = "cv", number = 10)
(fit.cv<-train(lcavol~.,Prostate,method='lm',trControl=train.control))
fit.cv$results$RMSE
pr3<-predict(fit.cv)
mean((Prostate$lcavol - pr3)^2)


#Leave One Out Cross-Validation (LOOCV)--------------------------------
train.control <- trainControl(method = "LOOCV")
(fit.cv<-train(lcavol~.,Prostate,method='lm',trControl=train.control))
fit.cv$results$RMSE
pr4<-predict(fit.cv)
mean((Prostate$lcavol - pr4)^2)


#ANCOVA------------------------------------------
fev<-read.table('fev.csv',header=T,sep=';',dec=',')
tail(fev)

summary(fev)
plot(fev[,1:3])
boxplot(FEV~Smoke,fev,col='orange')

fev.anova.smoke<-aov(FEV~Smoke,fev)
anova(fev.anova.smoke)
model.tables(fev.anova.smoke,type='mean')

fev.smoke.lm<-lm(FEV~Age,fev)
summary(fev.smoke.lm)

fev.ancova<-lm(FEV~Age+Smoke,fev)  
# Resultat equivalent per l'ANOVA fent servir la instrucció fev.ancova<-aov(FEV~Age+Smoke,fev)
anova(fev.ancova)
summary(fev.ancova) 
# Si hem fet servir aov podem accedir als coeficients amb coefficients(fev.ancova)

#Comparacions múltiples ANCOVA-------------------------------
library(multcomp)
summary(glht(fev.ancova,linfct=mcp(Smoke='Tukey')))

#Comprovació premisses ANCOVA----------------------------------------
plot(fev.ancova)

library(lattice)
xyplot(FEV~Age|Smoke, data=fev, type=c("p", "r"))

fev.ancova.full<-lm(FEV~Age*Smoke,fev)
anova(fev.ancova.full)
summary(fev.ancova.full)








