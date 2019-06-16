
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")

library(ggpubr)

tdados<-dados[,2:3]

t.test(retention~treatment,data=tdados)
t0<-t.test(retention~treatment,data=tdados)$statistic

p <- ggboxplot(tdados, x = "treatment", y = "retention",
         fill = "treatment", colour="treatment",palette =c("#003399", "#990000"), 
         add = "jitter",width = 0.9) +
  xlab("Tratamento")
p


box1<-ggplot(tdados, aes(treatment,retention,fill=treatment)) +
  geom_boxplot(colour ="black") + geom_jitter(alpha=0.6) +
  scale_fill_manual(values = c("#003399", "#990000")) +
  labs(x = "Tipo de ferro",y="Retenção") + guides(colour=FALSE,fill=FALSE)
box1

library(plyr)
cdat <- ddply(tdados, "treatment", summarise, rating.mean=mean(retention))
hist<-ggplot(tdados, aes(retention, fill=treatment)) +
  geom_histogram(binwidth=2, alpha=.5, position="identity") +
  geom_vline(data=cdat, aes(xintercept=rating.mean,  colour=treatment),
             linetype="dashed", size=1) +
  scale_fill_manual(values = c("#003399", "#990000"),name="Ferro") +
  scale_colour_manual(values = c("#003399", "#990000"),name="Ferro") +
  xlab("Retenção") + ylab("f")
hist

desc<-grid.arrange(box1,hist, ncol=2)

dir<-"D:/Estatística/Tópicos Espaciais Estatística - COMPUTACIONAL/Trabalhos/Perm/permutacao/desc.jpeg"

ggsave(desc, file=dir, width=10, height=5,dpi=300)

#Permutação teste t ------------------------------------------------------

b<-dim(tdados)[1]
N<-10000
dist<-replicate(N,t.test(retention~sample(treatment,b,replace=F),data=tdados)$statistic)
2*(sum(dist<t0)+1)/(N+1)

tnovo<-data.frame(dist=dist,id=dist<t0 | dist>-t0)
tt<-ggplot(tnovo,aes(x=dist,fill=id)) + 
  geom_histogram(aes(),binwidth=0.1,colour="black") +
  geom_vline(aes(xintercept=t0),   
             color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=-t0),   
             color="red", linetype="dashed", size=1) +
  xlab("Permutações")+ylab("f") +
  scale_fill_manual(values = c("#003399", "#990000")) +
  theme(legend.position="none") +
  annotate("text", x = t0, y = 300, angle = 90, label = t0, 
           vjust = -0.5, parse = TRUE,colour="red",size=3) +
  annotate("text", x = -t0, y = 305, angle = 90, label = -t0, 
           vjust = 1.2, parse = TRUE,colour="red",size=3) +
  ggtitle("Permutações para t0")
x11();tt

tt2<-grid.arrange(tt,tdensit4, ncol=2)

dir<-"D:/Estatística/Tópicos Espaciais Estatística - COMPUTACIONAL/Trabalhos/Perm/permutacao/ttest.jpeg"
ggsave(tt2,file=dir,width=10, height=5,dpi=300)


dir<-"D:/Estatística/Tópicos Espaciais Estatística - COMPUTACIONAL/Trabalhos/Perm/permutacao/ttest.jpeg"
ggsave(tt,file=dir,width=10, height=8,dpi=300)


cdat$rating.mean[2]-cdat$rating.mean[1]
