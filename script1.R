rm(list=ls())
library(tidyverse)
library(lmPerm)
library(gridExtra)
library(grid)

#This example has to do with iron retention in mice. Two different 
#treatments, each at three different levels, were fed to mice. The 
#treatments were tagged with radioactive iron, so that the percentage 
#of iron retained could be measured after a fixed period of time. 
#The data is presented in a table as follows:

rm(list = ls())
setwd("D:/Estatística/Tópicos Espaciais Estatística - COMPUTACIONAL/Trabalhos/Perm/datasets/")

dados<-read.table("irondata.txt",header = T,sep=";")
dados$interacao<-paste0(dados$treatment,dados$level)
head(dados)

anova<-aov(retention ~ treatment*level,data=dados)
summary(anova)

# Número de interações -----------------------------------------------
require(utils)
combin<-expand.grid(dados$retention, dados$treatment,dados$level,
                    stringsAsFactors=T)
dim(combin)

# Realizando a permutalçao para ANOVA --------------------------------------------
n <- length(dados$retention) #sample size n
aux <- anova(lm(retention ~ treatment*level,data=dados))$F[1:3]   #Observed statistic 
N <- 10000   # we will do 2000 permutations
T.perm <- list()   #A vector to save permutated statistic
set.seed(666)
for(i in 1:N) {
  y.perm <- sample(dados$retention, n, replace = F)   #permute data
  modelo<-lm(y.perm ~ treatment*level, data=dados)
  T.perm[[i]] <- anova(lm(modelo))$F[1:3]   #Permuted statistic
}

novo<-data.frame(matrix(unlist(T.perm), nrow=N, byrow=T))
names(novo)<-c("treatment","level","interacao")

# Verificando o valor da dist F
pf(df1 = 2 ,df2 =102 ,q = aux[2],lower.tail = T)
qf(df1 = 2 ,df2 =102 ,p = 0.00000001,lower.tail = T)

(sum(novo$treatment >= aux[1])+1)/(N+1)
(sum(novo$level >= aux[2])+1)/(N+1)
(sum(novo$interacao >= aux[3])+1)/(N+1)

# Gráficos das permutações ----------------------------------------------

# Nível ------
nivel <- data.frame(level=novo$level,id=novo$level>aux[2])
densit1<-ggplot(nivel,aes(x=level,fill=id)) + 
  geom_histogram(aes(),binwidth=0.1,colour="black") +
  geom_vline(aes(xintercept=aux[2]),   
             color="red", linetype="dashed", size=1) +
  xlab("Permutações Nível")+ylab("f") +
  scale_fill_manual(values = c("#003399", "#990000")) +
  scale_x_continuous(breaks=seq(0,20,2)) +
  coord_cartesian(xlim=c(-1,18))+ 
  ggtitle("Nível") + theme(legend.position="none") +
  annotate("text", x = aux[2], y = 300, angle = 90, label = aux[2], 
           vjust = -0.5, parse = TRUE,colour="red",size=3)
x11();densit1

# Tratamento ------
trat <- data.frame(treatment=novo$treatment,id=novo$treatment>aux[1])
densit2<-ggplot(trat,aes(x=treatment,fill=id)) + 
  geom_histogram(aes(),binwidth=0.1,colour="black") +
  geom_vline(aes(xintercept=aux[1]),   
             color="red", linetype="dashed", size=1) +
  xlab("Permutações Tratamento")+ylab("f") +
  scale_fill_manual(values = c("#003399", "#990000")) +
  ggtitle("Tratamento") + theme(legend.position="none") +
  annotate("text", x = aux[1], y = 600, angle = 90, label = aux[1], 
         vjust = -0.5, parse = TRUE,colour="red",size=3)
x11();densit2

# Interação ------
interac <- data.frame(interacao=novo$interacao,id=novo$interacao>aux[3])
densit3<-ggplot(interac,aes(x=interacao,fill=id)) + 
  geom_histogram(aes(),binwidth=0.1,colour="black") +
  geom_vline(aes(xintercept=aux[3]),   
             color="red", linetype="dashed", size=1) +
  xlab("Permutações Interação")+ylab("f") +
  scale_fill_manual(values = c("#003399", "#990000")) +
  scale_x_continuous(breaks=seq(0,10,1)) +
  ggtitle("Interação") + theme(legend.position="none") +
  annotate("text", x = aux[3], y = 900, angle = 90, label = aux[3], 
           vjust = 1.2, parse = TRUE,colour="red",size=3)
x11();densit3

# Dist F ------
fs<-qf(p=0.95,df1 =2,df2 =102)
f<-rf(df1 =2,df2 =102,n=10000)
dff <- data.frame(f=f,id=(f > fs | t < -fs))
densit4<-ggplot(dff,aes(x=f,fill=id)) + 
  geom_histogram(aes(),binwidth=0.1,colour="black") +
  geom_vline(aes(xintercept=fs),   
             color="red", linetype="dashed", size=1) +
  xlab("Distribuição F")+ylab("f") +
  scale_fill_manual(values = c("#003399", "#990000")) +
  scale_x_continuous(breaks=seq(0,10,1)) +
  ggtitle("Distribuição F,  P(Fc>F)") + theme(legend.position="none") +
  annotate("text", x = fs, y = 300, angle = 90, label = fs, 
           vjust = -0.5, parse = TRUE,colour="red",size=3)
x11();densit4

anovagrid<-grid.arrange(densit1,densit2,densit3,densit4, ncol=2)

dir<-"D:/Estatística/Tópicos Espaciais Estatística - COMPUTACIONAL/Trabalhos/Perm/permutacao/anovaF.jpeg"
ggsave(anovagrid,file=dir,width=10, height=8,dpi=300)

# ANOVA permutation library ------------------------------------------
set.seed(666)
mod2 <- aovp(retention ~ treatment+level, data=dados)
summary(mod2)

#=============================================================================
#=============================================================================

# Permutação para regressão linear ----------------------------------------

n <- length(dados$retention) #sample size n
auxlm <- summary(lm(retention ~ treatment+level,data=dados))$coefficients[,3]   #Observed statistic 

N <- 10000   # we will do 2000 permutations
Tpermlm <- list()   #A vector to save permutated statistic
set.seed(666)
for(i in 1:N) {
  y.perm <- sample(dados$retention, n, replace=F)   #permute data
  modelo <- summary(lm(y.perm ~ treatment+level,data=dados))
  Tpermlm[[i]] <- modelo$coefficients[,3]   #Permuted statistic
}

novo1<-data.frame(matrix(unlist(Tpermlm), nrow=N, byrow=T))
head(novo1)

names(novo1)<-c("Intercept","treatmentFe3","levellow","levelmedium")

# Valores de probabilidade, valor.p ---------------------------

summary(lm(retention ~ treatment+level,data=dados))
2*(sum(novo1$treatmentFe3 >= auxlm[2])+1)/(N+1)
2*(sum(novo1$levellow >= auxlm[3])+1)/(N+1)
2*(sum(novo1$levelmedium >= auxlm[4])+1)/(N+1)

0.00019998

# Gráficos das permutações reg linear-------------------------------------------

# tratFe3 ------
tratFe3 <- data.frame(treatmentFe3=novo1$treatmentFe3,
        id=(novo1$treatmentFe3 > auxlm[2] | novo1$treatmentFe3 < -auxlm[2]))
tdensit1<-ggplot(tratFe3,aes(x=treatmentFe3,fill=id)) + 
  geom_histogram(aes(),binwidth=0.1,colour="black") +
  geom_vline(aes(xintercept=auxlm[2]),   
             color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=-auxlm[2]),   
             color="red", linetype="dashed", size=1) +
  xlab("Permutações Fe3+")+ylab("f") +
  scale_fill_manual(values = c("#003399", "#990000")) +
  ggtitle("Fe3+") + theme(legend.position="none") +
  annotate("text", x = auxlm[2], y = 308, angle = 90, label = auxlm[2], 
           vjust = 1.2, parse = TRUE,colour="red",size=3) +
  annotate("text", x = -auxlm[2], y = 300, angle = 90, label = -auxlm[2], 
           vjust = -0.5, parse = TRUE,colour="red",size=3)
x11();tdensit1

# Low ------
dflevellow <- data.frame(levellow=novo1$levellow,
                      id=(novo1$levellow > auxlm[3] | novo1$levellow < -auxlm[3]))
tdensit2<-ggplot(dflevellow,aes(x=levellow,fill=id)) + 
  geom_histogram(aes(),binwidth=0.1,colour="black") +
  geom_vline(aes(xintercept=auxlm[3]),   
             color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=-auxlm[3]),   
             color="red", linetype="dashed", size=1) +
  xlab("Permutações nível baixo")+ylab("f") +
  scale_fill_manual(values = c("#003399", "#990000")) +
  ggtitle("Radioatividade nível baixo") + theme(legend.position="none") +
  annotate("text", x = auxlm[3], y = 308, angle = 90, label = auxlm[3], 
           vjust = 1.2, parse = TRUE,colour="red",size=3) +
  annotate("text", x = -auxlm[3], y = 300, angle = 90, label = -auxlm[3], 
           vjust = -0.5, parse = TRUE,colour="red",size=3)
x11();tdensit2

# Medium ------
dflevelmedium <- data.frame(levelmedium=novo1$levelmedium,
                         id=(novo1$levelmedium > auxlm[4] | novo1$levelmedium < -auxlm[4]))
tdensit3<-ggplot(dflevelmedium,aes(x=levelmedium,fill=id)) + 
  geom_histogram(aes(),binwidth=0.1,colour="black") +
  geom_vline(aes(xintercept=auxlm[4]),   
             color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=-auxlm[4]),   
             color="red", linetype="dashed", size=1) +
  xlab("Permutações médio")+ylab("f") +
  scale_fill_manual(values = c("#003399", "#990000")) +
  ggtitle("Radioatividade nível médio") + theme(legend.position="none") +
  annotate("text", x = auxlm[4], y = 308, angle = 90, label = auxlm[4], 
           vjust = 1.2, parse = TRUE,colour="red",size=3) +
  annotate("text", x = -auxlm[4], y = 300, angle = 90, label = -auxlm[4], 
           vjust = -0.5, parse = TRUE,colour="red",size=3)
x11();tdensit3

# Distribuição t----------------------------------------------
ts<-qt(p=0.975,df=104)
t<-rt(n =10000 ,df = 104)
dft <- data.frame(t=t,id=(t > ts | t < -ts))
tdensit4<-ggplot(dft,aes(x=t,fill=id)) + 
  geom_histogram(aes(),binwidth=0.1,colour="black") +
  geom_vline(aes(xintercept=ts),   
             color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=-ts),   
             color="red", linetype="dashed", size=1) +
  xlab("t-student")+ylab("f") +
  scale_fill_manual(values = c("#003399", "#990000")) +
  ggtitle("Distribuição t-student gl=106, P(T>|t|)") + theme(legend.position="none") +
  annotate("text", x = -ts, y = 300, angle = 90, label = -ts, 
           vjust = 1.2, parse = TRUE,colour="red",size=3) +
  annotate("text", x = ts, y = 302, angle = 90, label = ts, 
           vjust = -0.5, parse = TRUE,colour="red",size=3)
x11();tdensit4

lmgrid<-grid.arrange(tdensit1,tdensit2,tdensit3,tdensit4, ncol=2)

dir<-"D:/Estatística/Tópicos Espaciais Estatística - COMPUTACIONAL/Trabalhos/Perm/permutacao/lmt.jpeg"
ggsave(lmgrid,file=dir,width=10, height=8,dpi=300)

# ANOVA permutation library ------------------------------------------
set.seed(666)
mod3 <- lmp(retention ~ treatment+level, data=dados)
summary(mod3)
x11()
par(mfrow=c(2,2))
plot(mod3)
