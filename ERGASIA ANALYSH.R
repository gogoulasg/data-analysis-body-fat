fat<-read.table(file.choose())
View(fat)
str(fat)
names(fat)<-c("case_number","Brozek_Fat","Siri_Fat","Density","Age","Weight","Height","BMI","FFW","Perifereia_laimou",
"Perifereia_stithous","Perifereia_koilias","Perifereia_gofwn","Perifereia_mirwn","Perifereia_gonatou","Perifereia_astragalou"
,"Perifereia_dikefalou","Perifereia_xeriou","Perifereia_karpwn")
head(fat)
summary(fat)


##############ALLAGH MONADWN###########
fat$Weight<-round(fat$Weight*0.4536,2)
fat$Height<-round(fat$Height*2.54,2)
fat$FFW<-round(fat$FFW*0.4536,2)
summary(fat)


##################ALLAGH SFALMATWN#####################
fat[fat$Height==74.93,]
fat$Height[42]=176.35
Brozek2=(457/fat$Density)-414.2
round(Brozek2,1)
fat$Brozek_Fat
fat[abs(Brozek2-fat$Brozek_Fat)>0.5,]
fat$Brozek_Fat[48]=14.3
fat$Brozek_Fat[76]=14.3
fat$Brozek_Fat[96]=1.6
Siri2=(495/fat$Density)-450
round(Siri2,1)
fat$Siri_Fat
fat[abs(Siri2-fat$Siri_Fat)>0.5,]
fat$Siri_Fat[48]=14.1
fat$Siri_Fat[76]=14.1
fat$Siri_Fat[96]=0.37



###############PERIGRAFIKA METRA########################
#######################################################
########################################################

####################POSOTIKES####################
summary(fat)
install.packages("psych")
library(psych)
describe(fat[,2:19])
summary(fat[,2:19])
apply(fat[,2:19],2,quantile)
apply(fat[,2:19],2,mean) 
apply(fat[,2:19],2,median) 
apply(fat[,2:19],2,var) 
apply(fat[,2:19],2,sd) 
apply(fat[,2:19],2,max) 
apply(fat[,2:19],2,min) 
apply(fat[,2:19],2,quantile) 
apply(fat[,2:19],2,range) 
apply(fat[,2:19],2,skew) 
apply(fat[,2:19],2,kurtosi)
cv<-function(x){sd(x)/mean(x)}
apply(fat[,2:19],2,cv)
kurtosi<-function(x)
{
n<-length(x)
z<-(x-mean(x))/sd(x)
sum(z^4)/n-3
}
skew<-function(x)
{
n<-length(x)
z<-(x-mean(x))/sd(x)
sum(z^3)/n
}



################ISTOGRAMMATA-POSOTIKES############
#######AGE########
par(mfrow=c(3,3))
hist(fat$Age,probability=T,main="���������� �������",xlab="������",ylab="���������",col="gray")
x<-fat$Age
m1<-mean(x)
s1<-sd(x)
curve(dnorm(x,m1,s1),add=T,lwd="3",col="red")
#########Brozek####
hist(fat$Brozek_Fat,probability=T,main="���������� Brozek_Fat",xlab="�����",ylab="���������",ylim=c(0,0.06),col="gray")
x<-fat$Brozek_Fat
m1<-mean(x)
s1<-sd(x)
curve(dnorm(x,m1,s1),add=T,lwd="3",col="red")
##############Siri_Fat########
hist(fat$Siri_Fat,probability=T,main="���������� Siri_Fat",xlab="�����",ylab="���������",ylim=c(0,0.06),col="gray")
x<-fat$Siri_Fat
m1<-mean(x)
s1<-sd(x)
curve(dnorm(x,m1,s1),add=T,lwd="3",col="red")
##########Density######
hist(fat$Density,probability=T,main="���������� Density",xlab="Density",ylab="���������",ylim=c(0,25),col="gray")
x<-fat$Density
m1<-mean(x)
s1<-sd(x)
curve(dnorm(x,m1,s1),add=T,lwd="3",col="red")
##########Weight#######
hist(fat$Weight,probability=T,main="���������� ������",xlab="�����",ylab="���������",col="gray")
x<-fat$Weight
m1<-mean(x)
s1<-sd(x)
curve(dnorm(x,m1,s1),add=T,lwd="3",col="red")
##########Height#######
hist(fat$Height,probability=T,main="���������� �����",xlab="����",ylab="���������",ylim=c(0,0.06),col="gray")
x<-fat$Height
m1<-mean(x)
s1<-sd(x)
curve(dnorm(x,m1,s1),add=T,lwd="3",col="red")
###############BMI#########
hist(fat$BMI,probability=T,main="���������� ���",xlab="���",ylab="���������",ylim=c(0,0.12),col="gray")
x<-fat$BMI
m1<-mean(x)
s1<-sd(x)
curve(dnorm(x,m1,s1),add=T,lwd="3",col="red")
##############FFW###########
hist(fat$FFW,probability=T,main="���������� FFW",xlab="FFW",ylab="���������",ylim=c(0,0.06),col="gray")
x<-fat$FFW
m1<-mean(x)
s1<-sd(x)
curve(dnorm(x,m1,s1),add=T,lwd="3",col="red")



####################KATHGORIKES######################
fat$PBMI<-rep(1,dim(fat)[1])
fat$PBMI[which(fat$BMI<=25)]="Fysiologikos"
fat$PBMI[which(fat$BMI>25&fat$BMI<30)]="Ypervaros" 
fat$PBMI[which(fat$BMI>=30)]="Paxysarkos"
table(fat$PBMI)
prop.table(table(fat$PBMI))*100
summary(fat$BMI)
which(fat$PBMI=="Ellipovarhs")
fat[fat$case_number==182,]
fat$PBrozek_Fat=rep(1,dim(fat)[1])
fat$PBrozek_Fat[which(fat$Brozek_Fat<5)]="Isxno"
fat$PBrozek_Fat[which(fat$Brozek_Fat>=5&fat$Brozek_Fat<25)]="Fysiologiko"
fat$PBrozek_Fat[which(fat$Brozek_Fat>=25)]="Ypshlo"
table(fat$PBrozek_Fat)
prop.table(table(fat$PBrozek_Fat))*100
fat$PSiri_Fat=rep(1,dim(fat)[1])
fat$PSiri_Fat[which(fat$Siri_Fat<5)]="Isxno"
fat$PSiri_Fat[which(fat$Siri_Fat>=5&fat$Siri_Fat<25)]="Fysiologiko"
fat$PSiri_Fat[which(fat$Siri_Fat>=25)]="Ypshlo"
table(fat$PSiri_Fat)
prop.table(table(fat$PSiri_Fat))*100
fat$PSiri_Fat=as.factor(fat$PSiri_Fat)
fat$PBrozek_Fat=as.factor(fat$PBrozek_Fat)
fat$PBMI=as.factor(fat$PBMI)
str(fat)
par(mfrow=c(2,2))
barplot(table(fat$PBMI),main="���",ylim=c(0,140),col="red",las=2,names=c("������������","����������","���������"))
barplot(table(fat$PBrozek_Fat),main="Brozek",ylim=c(0,200),col="red",las=2,names=c("�����������","�����","�����"))
barplot(table(fat$PSiri_Fat),main="Siri",ylim=c(0,200),col="red",las=2,names=c("���/��","�����","�����"))

####################################################################
#######################ELEGXOI#####################################
#################################################################
install.packages("agricolae")
library(agricolae)  
install.packages("normtest")
library(normtest)
install.packages("car")
library(car)

#########################################
############KATHGORIKES###########################
##################################

#############################
#########PBrozek-Fat############
########################

###########DENSITY########
anova1<-aov(fat$Density~fat$PBrozek_Fat)
summary(anova1)
shapiro.test(anova1$res)#p-value<0.05
skewness.norm.test(anova1$res)#p-value>0.05
kurtosis.norm.test(anova1$res)#p-value>0.05
leveneTest(fat$Density~fat$PBrozek_Fat)
#eteroskedastikothta
oneway.test(fat$Density~fat$PBrozek_Fat,var.equal=FALSE)
pairwise.t.test(fat$Density,fat$PBrozek_Fat,p.adjust.method="bonferroni")
boxplot(fat$Density~fat$PBrozek_Fat,col=2:4)
#############AGE###############
anova2<-aov(fat$Age~fat$PBrozek_Fat)
summary(anova2)
shapiro.test(anova2$res)#p-value>0.05
var.test(fat$Age~fat$PBrozek_Fat)
leveneTest(fat$Age~fat$PBrozek_Fat)
#omoskedastikothta
oneway.test(fat$Age~fat$PBrozek_Fat,var.equal=TRUE)
pairwise.t.test(fat$Age,fat$PBrozek_Fat,p.adjust.method="bonferroni")
boxplot(fat$Age~fat$PBrozek_Fat,col=2:4)
###############WEIGHT################
anova3<-aov(fat$Weight~fat$PBrozek_Fat)
shapiro.test(anova3$res)#p-value<0.05
skewness.norm.test(anova3$res)#p-value<0.05
kurtosis.norm.test(anova3$res)#p-value<0.05
kruskal.test(fat$Weight~fat$PBrozek_Fat)#p-value<0.05
pairwise.wilcox.test(fat$Weight,fat$PBrozek_Fat,p.adjust.method="bonferroni")
boxplot(fat$Weight~fat$PBrozek_Fat,col=2:4)
################BMI########################
anova4<-aov(fat$BMI~fat$PBrozek_Fat)
summary(anova4)
shapiro.test(anova4$res)#p-value<0.05
skewness.norm.test(anova4$res)#p=value<0.05
kurtosis.norm.test(anova4$res)#p-value<0.05
kruskal.test(fat$BMI~fat$PBrozek_Fat)#p-value<0.05
pairwise.wilcox.test(fat$BMI,fat$PBrozek_Fat,p.adjust.method="bonferroni")
boxplot(fat$BMI~fat$PBrozek_Fat,col=2:4)
#################FFW###########################
anova5<-aov(fat$FFW~fat$PBrozek_Fat)
summary(anova5)
shapiro.test(anova5$res)#p-value<0.05
skewness.norm.test(anova5$res)#p=value<0.05
kurtosis.norm.test(anova5$res)#p-value<0.05
kruskal.test(fat$FFW~fat$PBrozek_Fat)#p-value>0.05
boxplot(fat$FFW~fat$PBrozek_Fat)
##################Perifereia_laimou##############
anova6<-aov(fat$Perifereia_laimou~fat$PBrozek_Fat)
summary(anova6)
shapiro.test(anova6$res)#p-value<0.05
skewness.norm.test(anova6$res)#p-value<0.05
kurtosis.norm.test(anova6$res)#p-value<0.05
kruskal.test(fat$Perifereia_laimou~fat$PBrozek_Fat)#p-value<0.05
pairwise.wilcox.test(fat$Perifereia_laimou,fat$PBrozek_Fat,p.adjust.method="bonferroni")
boxplot(fat$Perifereia_laimou~fat$PBrozek_Fat,col=2:4)
######################Perifereia_stithous##############
anova7<-aov(fat$Perifereia_stithous~fat$PBrozek_Fat)
summary(anova7)
shapiro.test(anova7$res)
skewness.norm.test(anova7$res)#p-value<0.05
kurtosis.norm.test(anova7$res)#p-value>0.05
kruskal.test(fat$Perifereia_stithous~fat$PBrozek_Fat)#p-value<0.05
pairwise.wilcox.test(fat$Perifereia_stithous,fat$PBrozek_Fat,p.adjust.method="bonferroni")
boxplot(fat$Perifereia_stithous~fat$PBrozek_Fat,col=2:4)
##########################Perifereia_koilias#################
anova8<-aov(fat$Perifereia_koilias~fat$PBrozek_Fat)
summary(anova8)
shapiro.test(anova8$res)
skewness.norm.test(anova8$res)#p-value<0.05
kurtosis.norm.test(anova8$res)#p-value<0.05
kruskal.test(fat$Perifereia_koilias~fat$PBrozek_Fat)#p-value<0.05
pairwise.wilcox.test(fat$Perifereia_koilias,fat$PBrozek_Fat,p.adjust.method="bonferroni")
boxplot(fat$Perifereia_koilias~fat$PBrozek_Fat)
#########################Perifereia_gofwn#######################
anova9<-aov(fat$Perifereia_gofwn~fat$PBrozek_Fat)
summary(anova9)
shapiro.test(anova9$res)
skewness.norm.test(anova9$res)#p-value<0.05
kurtosis.norm.test(anova9$res)#p-value<0.05
kruskal.test(fat$Perifereia_gofwn~fat$PBrozek_Fat)#p-value<0.05
pairwise.wilcox.test(fat$Perifereia_gofwn,fat$PBrozek_Fat,p.adjust.method="bonferroni")
boxplot(fat$Perifereia_gofwn~fat$PBrozek_Fat)
#############################Perifereia_mirwn##################
anova10<-aov(fat$Perifereia_mirwn~fat$PBrozek_Fat)
summary(anova10)
shapiro.test(anova10$res)
skewness.norm.test(anova10$res)#p-value<0.05
kurtosis.norm.test(anova10$res)#p-value<0.05
kruskal.test(fat$Perifereia_mirwn~fat$PBrozek_Fat)#p-value<0.05
pairwise.wilcox.test(fat$Perifereia_mirwn,fat$PBrozek_Fat,p.adjust.method="bonferroni")
boxplot(fat$Perifereia_mirwn~fat$PBrozek_Fat)
############################Perifereia_gonatou############
anova11<-aov(fat$Perifereia_gonatou~fat$PBrozek_Fat)
summary(anova11)
shapiro.test(anova11$res)#p-value<0.05
skewness.norm.test(anova11$res)#p-value<0.05
kurtosis.norm.test(anova11$res)#p-value<0.05
kruskal.test(fat$Perifereia_gonatou~fat$PBrozek_Fat)#p-value<0.05
pairwise.wilcox.test(fat$Perifereia_gonatou,fat$PBrozek_Fat,p.adjust.method="bonferroni")
boxplot(fat$Perifereia_gonatou~fat$PBrozek_Fat)
###############Perifereia_astragalou###########
anova12<-aov(fat$Perifereia_astragalou~fat$PBrozek_Fat)
summary(anova12)
shapiro.test(anova12$res)
skewness.norm.test(anova12$res)#p-value<0.05
kurtosis.norm.test(anova12$res)#p-value<0.05
kruskal.test(fat$Perifereia_astragalou~fat$PBrozek_Fat)#p-value<0.05
pairwise.wilcox.test(fat$Perifereia_astragalou,fat$PBrozek_Fat,p.adjust.method="bonferroni")
boxplot(fat$Perifereia_astragalou~fat$PBrozek_Fat)
#################Perifereia_dikefalou###########
anova13<-aov(fat$Perifereia_dikefalou~fat$PBrozek_Fat)
summary(anova13)
shapiro.test(anova13$res)#p-value>0.05
var.test(fat$Perifereia_dikefalou~fat$PBrozek_Fat)
leveneTest(fat$Perifereia_dikefalou~fat$PBrozek_Fat)#p-value>0.05
#omoskedastikothta
oneway.test(fat$Perifereia_dikefalou~fat$PBrozek_Fat,var.equal=TRUE)
pairwise.t.test(fat$Perifereia_dikefalou,fat$PBrozek_Fat,p.adjust.method="bonferroni")
boxplot(fat$Perifereia_dikefalou~fat$PBrozek_Fat,col=2:4)
##################Perifereia_xeriou###########
anova14<-aov(fat$Perifereia_xeriou~fat$PBrozek_Fat)
summary(anova14)
shapiro.test(anova14$res)
skewness.norm.test(anova14$res)#p-value>0.05
kurtosis.norm.test(anova14$res)#p-value<0.05
kruskal.test(fat$Perifereia_xeriou~fat$PBrozek_Fat)#p-value<0.05
pairwise.wilcox.test(fat$Perifereia_xeriou,fat$PBrozek_Fat,p.adjust.method="bonferroni")
boxplot(fat$Perifereia_xeriou~fat$PBrozek_Fat)
################Perifereia_karpwn#############
anova15<-aov(fat$Perifereia_karpwn~fat$PBrozek_Fat)
summary(anova15)
shapiro.test(anova15$res)#p-value>0.05
var.test(fat$Perifereia_karpwn~fat$PBrozek_Fat)
leveneTest(fat$Perifereia_karpwn~fat$PBrozek_Fat)#p-value>0.05
#omoskedastikothta
oneway.test(fat$Perifereia_karpwn~fat$PBrozek_Fat,var.equal=TRUE)
pairwise.t.test(fat$Perifereia_karpwn,fat$PBrozek_Fat,p.adjust.method="bonferroni")
boxplot(fat$Perifereia_karpwn~fat$PBrozek_Fat,col=2:4)
#################Height########
anova16<-aov(fat$Height~fat$PBrozek_Fat)
summary(anova16)
shapiro.test(anova16$res)#p-value>0.05
leveneTest(fat$Height~fat$PBrozek_Fat)#p-value<0.05
#eteroskedastikothta
oneway.test(fat$Height~fat$PBrozek_Fat,var.equal=FALSE)#p-value>0.05
boxplot(fat$Height~fat$PBrozek_Fat,main="����-�����",col=2:4)
############Siri_Fat####
anova17<-aov(fat$Siri_Fat~fat$PBrozek_Fat)
summary(anova17)
shapiro.test(anova17$res)#p-value>0.05
leveneTest(fat$Siri_Fat~fat$PBrozek_Fat)#p-value<0.05
#eteroskedastikothta
oneway.test(fat$Siri_Fat~fat$PBrozek_Fat,var.equal=FALSE)
pairwise.t.test(fat$Siri_Fat,fat$PBrozek_Fat,p.adjust.method="bonferroni")
boxplot(fat$Siri_Fat~fat$PBrozek_Fat,col=2:4)


##############################################
####################PSiri_Fat##############################


####################Brozek_Fat#########
anovas1<-aov(fat$Brozek_Fat~fat$PSiri_Fat)
summary(anovas1)
shapiro.test(anovas1$res)#p-value<0.05
skewness.norm.test(anovas1$res)#p-value>0.05
kurtosis.norm.test(anovas1$res)#p-value>0.05
leveneTest(fat$Brozek_Fat~fat$PSiri_Fat)#p-value<0.05
#eteroskedastikothta
oneway.test(fat$Brozek_Fat~fat$PSiri_Fat,var.equal=FALSE)
pairwise.t.test(fat$Brozek_Fat,fat$PSiri_Fat,p.adjust.method="bonferroni")
boxplot(fat$Brozek_Fat~fat$PSiri_Fat,col=2:4)
###################Density###########
anovas2<-aov(fat$Density~fat$PSiri_Fat)
summary(anovas2)
shapiro.test(anovas2$res)#p-value<0.05
skewness.norm.test(anovas2$res)#p-value>0.05
kurtosis.norm.test(anovas2$res)#p-value>0.05
leveneTest(fat$Density~fat$PSiri_Fat)#p-value<0.05
#eteroskedastikothta
oneway.test(fat$Density~fat$PSiri_Fat,var.equal=TRUE)
pairwise.t.test(fat$Density,fat$PSiri_Fat,p.adjust.method="bonferroni")
boxplot(fat$Density~fat$PSiri_Fat,col=2:4)
##############Age################
anovas3<-aov(fat$Age~fat$PSiri_Fat)
summary(anovas3)
shapiro.test(anovas3$res)#p-value>0.05
leveneTest(fat$Age~fat$PSiri_Fat)#p-value>0.05
#omoskedastikothta
oneway.test(fat$Age~fat$PSiri_Fat,var.equal=TRUE)
pairwise.t.test(fat$Age,fat$PSiri_Fat,p.adjust.method="bonferroni")
boxplot(fat$Age~fat$PSiri_Fat)
#################Weight##############
anovas4<-aov(fat$Weight~fat$PSiri_Fat)
summary(anovas4)
shapiro.test(anovas4$res)#p-value<0.05
skewness.norm.test(anovas4$res)#p-value<0.05
kurtosis.norm.test(anovas4$res)#p-value<0.05
kruskal.test(fat$Weight~fat$PSiri_Fat)#p-value<0.05
pairwise.wilcox.test(fat$Weight,fat$PSiri_Fat,p.adjust.method="bonferroni")
boxplot(fat$Weight~fat$PSiri_Fat,col=2:4)
########################Height############
anovas5<-aov(fat$Height~fat$PSiri_Fat)
summary(anovas5)
shapiro.test(anovas5$res)
leveneTest(fat$Height~fat$PSiri_Fat)#p-value>0.05
#omoskedastikothta
oneway.test(fat$Height~fat$PSiri_Fat,var.equal=TRUE)#p-value>0.05
boxplot(fat$Height~fat$PSiri_Fat,main="����-�����",col=2:4)
##############BMI###############
anovas6<-aov(fat$BMI~fat$PSiri_Fat)
summary(anovas6)
shapiro.test(anovas6$res)
skewness.norm.test(anovas6$res)#p-value<0.05
kurtosis.norm.test(anovas6$res)#p-value<0.05
kruskal.test(fat$BMI~fat$PSiri_Fat)#p-value<0.05
pairwise.wilcox.test(fat$BMI,fat$PSiri_Fat,p.adjust.method="bonferroni")
boxplot(fat$BMI~fat$PSiri_Fat,col=2:4)
#############FFW#############
anovas7<-aov(fat$FFW~fat$PSiri_Fat)
summary(anovas7)
shapiro.test(anovas7$res)
skewness.norm.test(anovas7$res)#p-value<0.05
kurtosis.norm.test(anovas7$res)#p-value<0.05
kruskal.test(fat$FFW~fat$PSiri_Fat)#p-value>0.05
boxplot(fat$FFW~fat$PSiri_Fat,main="FFW-�����",col=2:4)
##############Perifereia_laimou###########
anovas8<-aov(fat$Perifereia_laimou~fat$PSiri_Fat)
summary(anovas8)
shapiro.test(anovas8$res)
skewness.norm.test(anovas8$res)#p-value<0.05
kurtosis.norm.test(anovas8$res)#p-value<0.05
kruskal.test(fat$Perifereia_laimou~fat$PSiri_Fat)#p-value<0.05
pairwise.wilcox.test(fat$Perifereia_laimou,fat$PSiri_Fat,p.adjust.method="bonferroni")
boxplot(fat$Perifereia_laimou~fat$PSiri_Fat)
#####################Perifereia_stithous#########
anovas9<-aov(fat$Perifereia_stithous~fat$PSiri_Fat)
summary(anovas9)
shapiro.test(anovas9$res)
skewness.norm.test(anovas9$res)#p-value<0.05
kurtosis.norm.test(anovas9$res)#p-value>0.05
kruskal.test(fat$Perifereia_stithous~fat$PSiri_Fat)#p-value<0.05
pairwise.wilcox.test(fat$Perifereia_stithous,fat$PSiri_Fat,p.adjust.method="bonferroni")
boxplot(fat$Perifereia_stithous~fat$PSiri_Fat)
#################Perifereia_koilias#############
anovas10<-aov(fat$Perifereia_koilias~fat$PSiri_Fat)
summary(anovas10)
shapiro.test(anovas10$res)
skewness.norm.test(anovas10$res)#p-value<0.05
kurtosis.norm.test(anovas10$res)#p-value>0.05
kruskal.test(fat$Perifereia_koilias~fat$PSiri_Fat)#p-value<0.05
pairwise.wilcox.test(fat$Perifereia_koilias,fat$PSiri_Fat,p.adjust.method="bonferroni")
boxplot(fat$Perifereia_koilias~fat$PSiri_Fat,col=2:4)
###################Perifereia_gofwn###########
anovas11<-aov(fat$Perifereia_gofwn~fat$PSiri_Fat)
summary(anovas11)
shapiro.test(anovas11$res)
skewness.norm.test(anovas11$res)#p-value<0.05
kurtosis.norm.test(anovas11$res)#p-value>0.05
kruskal.test(fat$Perifereia_gofwn~fat$PSiri_Fat)#p-value<0.05
pairwise.wilcox.test(fat$Perifereia_gofwn,fat$PSiri_Fat,p.adjust.method="bonferroni")
boxplot(fat$Perifereia_gofwn~fat$PSiri_Fat)
##################Perifereia_mirwn###########
anovas12<-aov(fat$Perifereia_mirwn~fat$PSiri_Fat)
summary(anovas12)
shapiro.test(anovas12$res)
skewness.norm.test(anovas12$res)#p-value<0.05
kurtosis.norm.test(anovas12$res)#p-value>0.05
kruskal.test(fat$Perifereia_mirwn~fat$PSiri_Fat)#p-value<0.05
pairwise.wilcox.test(fat$Perifereia_mirwn,fat$PSiri_Fat,p.adjust.method="bonferroni")
boxplot(fat$Perifereia_mirwn~fat$PSiri_Fat)
#####################Perifereia_gonatou###########3
anovas13<-aov(fat$Perifereia_gonatou~fat$PSiri_Fat)
summary(anovas13)
shapiro.test(anovas13$res)
skewness.norm.test(anovas13$res)#p-value>0.05
kurtosis.norm.test(anovas13$res)#p-value>0.05
kruskal.test(fat$Perifereia_gonatou~fat$PSiri_Fat)#p-value<0.05
pairwise.wilcox.test(fat$Perifereia_gonatou,fat$PSiri_Fat,p.adjust.method="bonferroni")
boxplot(fat$Perifereia_gonatou~fat$PSiri_Fat)
#################Perifereia_astragalou###########
anovas14<-aov(fat$Perifereia_astragalou~fat$PSiri_Fat)
summary(anovas14)
shapiro.test(anovas14$res)
skewness.norm.test(anovas14$res)#p-value>0.05
kurtosis.norm.test(anovas14$res)#p-value>0.05
kruskal.test(fat$Perifereia_astragalou~fat$PSiri_Fat)#p-value<0.05
pairwise.wilcox.test(fat$Perifereia_astragalou,fat$PSiri_Fat,p.adjust.method="bonferroni")
boxplot(fat$Perifereia_astragalou~fat$PSiri_Fat)
###################Perifereia_dikefalou##############
anovas15<-aov(fat$Perifereia_dikefalou~fat$PSiri_Fat)
summary(anovas15)
shapiro.test(anovas15$res)
skewness.norm.test(anovas15$res)#p-value<0.05
kurtosis.norm.test(anovas15$res)#p-value>0.05
kruskal.test(fat$Perifereia_dikefalou~fat$PSiri_Fat)#p-value<0.05
pairwise.wilcox.test(fat$Perifereia_dikefalou,fat$PSiri_Fat,p.adjust.method="bonferroni")
boxplot(fat$Perifereia_dikefalou~fat$PSiri_Fat)
#################Perifereia_xeriou############
anovas16<-aov(fat$Perifereia_xeriou~fat$PSiri_Fat)
summary(anovas16)
shapiro.test(anovas16$res)
skewness.norm.test(anovas16$res)#p-value>0.05
kurtosis.norm.test(anovas16$res)#p-value<0.05
kruskal.test(fat$Perifereia_xeriou~fat$PSiri_Fat)#p-value<0.05
pairwise.wilcox.test(fat$Perifereia_xeriou,fat$PSiri_Fat,p.adjust.method="bonferroni")
boxplot(fat$Perifereia_xeriou~fat$PSiri_Fat)
###################Perifereia_karpwn###########
anovas17<-aov(fat$Perifereia_karpwn~fat$PSiri_Fat)
summary(anovas17)
shapiro.test(anovas17$res)
leveneTest(fat$Perifereia_karpwn~fat$PSiri_Fat)#p-value>0.05
#omoskedastikothta
oneway.test(fat$Perifereia_karpwn~fat$PSiri_Fat,var.equal=TRUE)#p-value<0.05
pairwise.t.test(fat$Perifereia_karpwn,fat$PSiri_Fat,p.adjust.method="bonferroni")
boxplot(fat$Perifereia_karpwn~fat$PSiri_Fat,col=2:4)



########################################
###################PBMI#######################



###########Brozek_Fat##############
anovab1<-aov(fat$Brozek_Fat~fat$PBMI)
summary(anovab1)
shapiro.test(anovab1$res)#p-value>0.05
leveneTest(fat$Brozek_Fat~fat$PBMI)#p-value>0.05
#omoskedastikothta
oneway.test(fat$Brozek_Fat~fat$PBMI,var.equal=TRUE)#p-value<0.05
pairwise.t.test(fat$Brozek_Fat,fat$PBMI,p.adjust.method="bonferroni")
boxplot(fat$Brozek_Fat~fat$PBMI,col=2:4)
###############Siri_Fat#############
anovab2<-aov(fat$Siri_Fat~fat$PBMI)
summary(anovab2)
shapiro.test(anovab2$res)#p-value>0.05
leveneTest(fat$Siri_Fat~fat$PBMI)#p-value>0.05
#omoskedastikothta
oneway.test(fat$Siri_Fat~fat$PBMI,var.equal=TRUE)#p-value<0.05
pairwise.t.test(fat$Siri_Fat,fat$PBMI,p.adjust.method="bonferroni")
boxplot(fat$Siri_Fat~fat$PBMI)
############Density############3
anovab3<-aov(fat$Density~fat$PBMI)
summary(anovab3)
shapiro.test(anovab3$res)#p-value>0.05
leveneTest(fat$Density~fat$PBMI)#p-value>0.05
#omoskedastikothta
oneway.test(fat$Density~fat$PBMI,var.equal=TRUE)#p-value<0.05
pairwise.t.test(fat$Density,fat$PBMI,p.adjust.method="bonferroni")
boxplot(fat$Density~fat$PBMI,col=2:4)
###################Age###############
anovab4<-aov(fat$Age~fat$PBMI)
summary(anovab4)
shapiro.test(anovab4$res)#p-value<0.05
skewness.norm.test(anovab4$res)#p-value>0.05
kurtosis.norm.test(anovab4$res)#p-value>0.05
leveneTest(fat$Age~fat$PBMI)#p-value>0.05
#omoskedastikothta
oneway.test(fat$Age~fat$PBMI,var.equal=TRUE)#p-value>0.05
boxplot(fat$Age~fat$PBMI)
####################Weight#############
anovab5<-aov(fat$Weight~fat$PBMI)
summary(anovab5)
shapiro.test(anovab5$res)#p-value<0.05
skewness.norm.test(anovab5$res)#p-value<0.05
kurtosis.norm.test(anovab5$res)#p-value<0.05
kruskal.test(fat$Weight~fat$PBMI)#p-value<0.05
pairwise.wilcox.test(fat$Weight,fat$PBMI,p.adjust.method="bonferroni")
boxplot(fat$Weight~fat$PBMI,col=2:4)
###################Height##############
anovab6<-aov(fat$Height~fat$PBMI)
summary(anovab6)
shapiro.test(anovab6$res)#p-value>0.05
leveneTest(fat$Height~fat$PBMI)#p-value>0.05
#omoskedastikothta
oneway.test(fat$Height~fat$PBMI,var.equal=TRUE)#p-value>0.05
boxplot(fat$Height~fat$PBMI)
##################FFW##############
anovab7<-aov(fat$FFW~fat$PBMI)
summary(anovab7)
shapiro.test(anovab7$res)#p-value<0.05
skewness.norm.test(anovab7$res)#p-value<0.05
kurtosis.norm.test(anovab7$res)#p-value<0.05
kruskal.test(fat$FFW~fat$PBMI)#p-value<0.05
pairwise.wilcox.test(fat$FFW,fat$PBMI,p.adjust.method="bonferroni")
boxplot(fat$FFW~fat$PBMI,main="FFW-���",col=2:4)
##################Perifereia_laimou##########
anovab8<-aov(fat$Perifereia_laimou~fat$PBMI)
summary(anovab8)
shapiro.test(anovab8$res)#p-value<0.05
skewness.norm.test(anovab8$res)#p-value<0.05
kurtosis.norm.test(anovab8$res)#p-value<0.05
kruskal.test(fat$Perifereia_laimou~fat$PBMI)#p-value<0.05
pairwise.wilcox.test(fat$Perifereia_laimou,fat$PBMI,p.adjust.method="bonferroni")
boxplot(fat$Perifereia_laimou~fat$PBMI)
##################Perifereia_stithous##############
anovab9<-aov(fat$Perifereia_stithous~fat$PBMI)
summary(anovab9)
shapiro.test(anovab9$res)#p-value<0.05
skewness.norm.test(anovab9$res)#p-value<0.05
kurtosis.norm.test(anovab9$res)#p-value<0.05
kruskal.test(fat$Perifereia_stithous~fat$PBMI)#p-value<0.05
pairwise.wilcox.test(fat$Perifereia_stithous,fat$PBMI,p.adjust.method="bonferroni")
boxplot(fat$Perifereia_stithous~fat$PBMI)
##################Perifereia_koilias###########
anovab10<-aov(fat$Perifereia_koilias~fat$PBMI)
summary(anovab10)
shapiro.test(anovab10$res)#p-value<0.05
skewness.norm.test(anovab10$res)#p-value<0.05
kurtosis.norm.test(anovab10$res)#p-value<0.05
kruskal.test(fat$Perifereia_koilias~fat$PBMI)#p-value<0.05
pairwise.wilcox.test(fat$Perifereia_koilias,fat$PBMI,p.adjust.method="bonferroni")
boxplot(fat$Perifereia_koilias~fat$PBMI,col=2:4)
##################Perifereia_gofwn#############
anovab11<-aov(fat$Perifereia_gofwn~fat$PBMI)
summary(anovab11)
shapiro.test(anovab11$res)#p-value<0.05
skewness.norm.test(anovab11$res)#p-value<0.05
kurtosis.norm.test(anovab11$res)#p-value<0.05
kruskal.test(fat$Perifereia_gofwn~fat$PBMI)#p-value<0.05
pairwise.wilcox.test(fat$Perifereia_gofwn,fat$PBMI,p.adjust.method="bonferroni")
boxplot(fat$Perifereia_gofwn~fat$PBMI)
########################Perifereia_mirwn##########3
anovab12<-aov(fat$Perifereia_mirwn~fat$PBMI)
summary(anovab12)
shapiro.test(anovab12$res)#p-value<0.05
skewness.norm.test(anovab12$res)#p-value<0.05
kurtosis.norm.test(anovab12$res)#p-value<0.05
kruskal.test(fat$Perifereia_mirwn~fat$PBMI)#p-value<0.05
pairwise.wilcox.test(fat$Perifereia_mirwn,fat$PBMI,p.adjust.method="bonferroni")
boxplot(fat$Perifereia_mirwn~fat$PBMI)
####################Perifereia_gonatou#############
anovab13<-aov(fat$Perifereia_gonatou~fat$PBMI)
summary(anovab13)
shapiro.test(anovab13$res)#p-value>0.05
leveneTest(fat$Perifereia_gonatou~fat$PBMI)#p-value<0.05
#eteroskedastikothta
oneway.test(fat$Perifereia_gonatou~fat$PBMI,var.equal=FALSE)#p-value<0.05
pairwise.t.test(fat$Perifereia_gonatou,fat$PBMI,p.adjust.method="bonferroni")
boxplot(fat$Perifereia_gonatou~fat$PBMI)
################Perifereia_astragalou###########
anovab14<-aov(fat$Perifereia_astragalou~fat$PBMI)
summary(anovab14)
shapiro.test(anovab14$res)#p-value<0.05
skewness.norm.test(anovab14$res)#p-value<0.05
kurtosis.norm.test(anovab14$res)#p-value<0.05
kruskal.test(fat$Perifereia_astragalou~fat$PBMI)#p-value<0.05
pairwise.wilcox.test(fat$Perifereia_astragalou,fat$PBMI,p.adjust.method="bonferroni")
boxplot(fat$Perifereia_astragalou~fat$PBMI)
#############Perifereia_dikefalou#########
anovab15<-aov(fat$Perifereia_dikefalou~fat$PBMI)
summary(anovab15)
shapiro.test(anovab15$res)#p-value<0.05
skewness.norm.test(anovab15$res)#p-value>0.05
kurtosis.norm.test(anovab15$res)#p-value<0.05
kruskal.test(fat$Perifereia_dikefalou~fat$PBMI)#p-value<0.05
pairwise.wilcox.test(fat$Perifereia_dikefalou,fat$PBMI,p.adjust.method="bonferroni")
boxplot(fat$Perifereia_dikefalou~fat$PBMI,col=2:4)
##################Perifereia_xeriou#########
anovab16<-aov(fat$Perifereia_xeriou~fat$PBMI)
summary(anovab16)
shapiro.test(anovab16$res)#p-value<0.05
skewness.norm.test(anovab16$res)#p-value>0.05
kurtosis.norm.test(anovab16$res)#p-value<0.05
kruskal.test(fat$Perifereia_xeriou~fat$PBMI)#p-value<0.05
pairwise.wilcox.test(fat$Perifereia_xeriou,fat$PBMI,p.adjust.method="bonferroni")
boxplot(fat$Perifereia_xeriou~fat$PBMI)
###################Perifereia_karpwn###########
anovab17<-aov(fat$Perifereia_karpwn~fat$PBMI)
summary(anovab17)
shapiro.test(anovab17$res)#p-value>0.05
leveneTest(fat$Perifereia_karpwn~fat$PBMI)#p-value>0.05
#omoskedastikothta
oneway.test(fat$Perifereia_karpwn~fat$PBMI,var.equal=TRUE)#p-value<0.05
pairwise.t.test(fat$Perifereia_karpwn,fat$PBMI,p.adjust.method="bonferroni")
boxplot(fat$Perifereia_karpwn~fat$PBMI,col=2:4)


#######################################################
###################POSOTIKES###############################
################################################
shapiro.test(fat$Brozek_Fat)
shapiro.test(fat$Siri_Fat)
shapiro.test(fat$Density)
shapiro.test(fat$Age)#p-value<0.05
shapiro.test(fat$Weight)#p-value<0.05
shapiro.test(fat$Height)
shapiro.test(fat$BMI)#p-value<0.05
shapiro.test(fat$FFW)#p-value<0.05
shapiro.test(fat$Perifereia_laimou)#p-value<0.05
shapiro.test(fat$Perifereia_stithous)#p-value<0.05
shapiro.test(fat$Perifereia_koilias)#p-value<0.05
shapiro.test(fat$Perifereia_gofwn)#p-value<0.05
shapiro.test(fat$Perifereia_mirwn)#p-value<0.05
shapiro.test(fat$Perifereia_gonatou)#p-value<0.05
shapiro.test(fat$Perifereia_astragalou)#p-value<0.05
shapiro.test(fat$Perifereia_dikefalou)#p-value<0.05
shapiro.test(fat$Perifereia_xeriou)#p-value<0.05
shapiro.test(fat$Perifereia_karpwn)

##############kanonikh katanomh################
cor.test(fat$Brozek_Fat,fat$Siri_Fat,method="pearson")
plot(fat$Brozek_Fat,fat$Siri_Fat,col="red")
cor.test(fat$Brozek_Fat,fat$Density,method="pearson")
plot(fat$Brozek_Fat,fat$Density,col="red")
cor.test(fat$Brozek_Fat,fat$Height,method="pearson")#p-value=0.48
plot(fat$Brozek_Fat,fat$Height,col="red")
cor.test(fat$Brozek_Fat,fat$Perifereia_karpwn,method="pearson")#low 
plot(fat$Brozek_Fat,fat$Perifereia_karpwn,col="blue")
cor.test(fat$Siri_Fat,fat$Perifereia_karpwn,method="pearson")#low
cor.test(fat$Siri_Fat,fat$Density,method="pearson")
cor.test(fat$Siri_Fat,fat$Height,method="pearson")
plot(fat$Siri_Fat,fat$Height,col="blue")
cor.test(fat$Density,fat$Height,method="pearson")
plot(fat$Density,fat$Height,col="red")
cor.test(fat$Density,fat$Perifereia_karpwn,method="pearson")
plot(fat$Density,fat$Perifereia_karpwn,col="blue")
cor.test(fat$Height,fat$Perifereia_karpwn,method="pearson")
plot(fat$Height,fat$Perifereia_karpwn,col="blue")

##############den akoloythoun kanonikh katanomh###############
cor.test(fat$BMI,fat$Brozek_Fat,method="spearman")
plot(fat$BMI,fat$Brozek_Fat,col="red")
cor.test(fat$BMI,fat$Siri_Fat,method="spearman")
cor.test(fat$BMI,fat$Density,method="spearman")
plot(fat$BMI,fat$Density,col="blue")
cor.test(fat$BMI,fat$Height,method="spearman")
plot(fat$BMI,fat$Height)
cor.test(fat$BMI,fat$Perifereia_karpwn,method="spearman")
plot(fat$BMI,fat$Perifereia_karpwn,col="red")
cor.test(fat$BMI,fat$Weight,method="spearman")
cor.test(fat$BMI,fat$Perifereia_laimou,method="spearman")
cor.test(fat$BMI,fat$Age,method="spearman")
plot(fat$BMI,fat$Age)
cor.test(fat$BMI,fat$FFW,method="spearman")
plot(fat$BMI,fat$FFW,col="red")
cor.test(fat$BMI,fat$Perifereia_gofwn,method="spearman")
cor.test(fat$BMI,fat$Perifereia_koilias,method="spearman")
plot(fat$BMI,fat$Perifereia_koilias,col="red")
cor.test(fat$FFW,fat$Weight,method="spearman")
cor.test(fat$FFW,fat$Brozek_Fat,method="spearman")
plot(fat$FFW,fat$Brozek_Fat)
cor.test(fat$FFW,fat$Siri_Fat,method="spearman")
cor.test(fat$FFW,fat$Density,method="spearman")
plot(fat$FFW,fat$Density)
cor.test(fat$Perifereia_koilias,fat$Perifereia_karpwn,method="spearman")
plot(fat$Perifereia_koilias,fat$Perifereia_karpwn,col="red")
cor.test(fat$Perifereia_karpwn,fat$Perifereia_laimou,method="spearman")
cor.test(fat$Perifereia_karpwn,fat$Perifereia_gofwn,method="spearman")
plot(fat$Perifereia_karpwn,fat$Perifereia_gofwn,col="red")
cor.test(fat$Perifereia_koilias,fat$Perifereia_gofwn,method="spearman")
plot(fat$Perifereia_koilias,fat$Perifereia_gofwn,col="red")
cor.test(fat$Perifereia_koilias,fat$Perifereia_laimou,method="spearman")
plot(fat$Perifereia_koilias,fat$Perifereia_laimou,col="red")
cor.test(fat$Perifereia_gofwn,fat$Perifereia_laimou,method="spearman")

#######################kathgorikes###############
tab1<-table(fat$PBMI,fat$PBrozek_Fat) 
#���� 2 ���� �2
chi1<-chisq.test(tabl) 
chi1 
chi1$expected 
#�� ��� �������� ������ ��� ������ ��� ������������ ����� ��� ������ �� ����� ���� ��� 5
#�� ��� ����� ���� fisher
fisher1<-fisher.test(tab1) 
fisher1 
CrossTable(fat$PBMI,fat$PBrozek_Fat) 
barplot(tab1,xlab="Brozek",ylab="BMI",beside=T,col=c("blue","red"),ylim=c(0,120))


tab2<-table(fat$PBMI,fat$PSiri_Fat) 
#���� 2 ���� �2
chi2<-chisq.test(tab2) 
chi2 
chi2$expected 
#�� ��� �������� ������ ��� ������ ��� ������������ ����� ��� ������ �� ����� ���� ��� 5
#�� ��� ����� ���� fisher
fisher2<-fisher.test(tab2) 
fisher2
CrossTable(fat$PBMI,fat$PSiri_Fat) 
barplot(tab2,xlab="Siri",ylab="BMI",beside=T,col=c("blue","red"),ylim=c(0,120))

tab3<-table(fat$PBrozek_Fat,fat$PSiri_Fat) 
#���� 2 ���� �2
chi3<-chisq.test(tab3) 
chi3 
chi3$expected 
#�� ��� �������� ������ ��� ������ ��� ������������ ����� ��� ������ �� ����� ���� ��� 5
#�� ��� ����� ���� fisher
fisher3<-fisher.test(tab3) 
fisher3
CrossTable(fat$PBrozek_Fat,fat$PSiri_Fat) 
barplot(tab3,xlab="Siri",ylab="Brozek",beside=T,col=c("blue","red"),ylim=c(0,200))


################## MONTELO 1####################
MS2<-step(model, direction="both")
#back
#forward
model<-lm((fat$Brozek_Fat)~Age +  FFW + Height + Perifereia_laimou + Perifereia_gonatou + Perifereia_astragalou  + Perifereia_dikefalou + Perifereia_xeriou + Perifereia_karpwn ,data=fat)
vif(model)
summary(model)
MS2<-step(model, direction="both")
������ 1: ��������� ���������Time
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(fat[,c(2:19)], histogram=TRUE, pch=19)
������ 2: full model
model<-lm((fat$Brozek_Fat)~FFW+Height+Perifereia_laimou + Perifereia_gonatou + Perifereia_astragalou+Perifereia_dikefalou,data=fat)
fullmodel
������ 3: ����������� ����� ��� ����������� ��������
summary(model)
������ 4: ������� �������������������� ��� �� ���������� �������
install.packages("car")
library(car)
vif(model)
par(mfrow=c(2,2))
������ 5: ������� ������������� ��� �� ���������� �������
shapiro.test(rstandard(model)) 
install.packages("nortest")
library(nortest)
lillie.test(rstandard(model)) 
plot(model,which=2)
������ 6: ������� ������������������ ��� �� ���������� �������
plot(model$fit, rstandard(model), cex=2, col='blue', ylim = c(-4, 4))
abline(h= 1.96, col='red', lwd=2, lty=2)
abline(h=-1.96, col='red', lwd=2, lty=2)

quantcut <- function(x, digits=6){ cut(x, breaks=quantile(x),  include.lowest=TRUE, dig.lab = digits) }
qfits <- quantcut(model$fit )
leveneTest(rstandard(model), qfits) 

������ 7: ������� ������� ����� ��� ����������� ��������
plot(model,which=4)
abline(h=4/(nrow(fat)-2-1),col='red',lty=2)
install.packages("car")
library(car)
outlierTest(model)
plot(model,which=5)

######################montelo 2###################
MS3<-step(model2, direction="both")
#back
#forward
model2<-lm((fat$Siri_Fat)~FFW+Height+Perifereia_laimou + Perifereia_gonatou + Perifereia_astragalou+Perifereia_dikefalou,data=fat)
vif(model2)
summary(model2)
MS3<-step(model2, direction="both")
������ 1: ��������� ���������Time
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(fat[,c(3:10)], histogram=TRUE, pch=19)
������ 2: full model
model2<-lm((fat$Siri_Fat)~FFW+Height+Perifereia_laimou + Perifereia_gonatou + Perifereia_astragalou+Perifereia_dikefalou,data=fat)
������ 3: ����������� ����� ��� ����������� ��������
summary(model2)
������ 4: ������� �������������������� ��� �� ���������� �������
install.packages("car")
library(car)
vif(model2)
par(mfrow=c(2,2))
������ 5: ������� ������������� ��� �� ���������� �������
shapiro.test(rstandard(model2)) 
install.packages("nortest")
library(nortest)
lillie.test(rstandard(model2)) 
plot(model2,which=2)
������ 6: ������� ������������������ ��� �� ���������� �������
plot(model2$fit, rstandard(model2), cex=2, col='blue', ylim = c(-4, 4))
abline(h= 1.96, col='red', lwd=2, lty=2)
abline(h=-1.96, col='red', lwd=2, lty=2)

quantcut <- function(x, digits=6){ cut(x, breaks=quantile(x),  include.lowest=TRUE, dig.lab = digits) }
qfits <- quantcut(model2$fit )
leveneTest(rstandard(model2), qfits) 

������ 7: ������� ������� ����� ��� ����������� ��������
plot(model2,which=4)
abline(h=4/(nrow(fat)-2-1),col='red',lty=2)
install.packages("car")
library(car)
outlierTest(model2)
plot(model2,which=5)

