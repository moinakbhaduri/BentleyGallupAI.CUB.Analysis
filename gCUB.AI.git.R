#---library(CUB)---#
#---AI concerns (Bentley-Gallup study, Wave 3, 2024)---#
#---uncertainty-feeling analysis through CUB models---#
#---Moinak Bhaduri, Bentley Math Sciences, mbhaduri@bentley.edu---#

#--Full data are available from Bentley's ATC in an Excel spreadsheet---#
#--This data is defined to be "Gallup2024Data" in the codes below---#

g24.r.data=data.frame(Gallup2024Data)[,c(47:55)]

aug.vars=cbind(Gallup2024Data$Q38,Gallup2024Data$DEMO_GENDER,Gallup2024Data$DEMO_AGE,
               Gallup2024Data$DEMO_EDUCATION,Gallup2024Data$DEMO_RACE)
colnames(aug.vars)<-c("Pol","Gender","Age","Educ","Race")
g24.r.data.aug=cbind(g24.r.data,aug.vars)
colnames(g24.r.data.aug)<-c(colnames(g24.r.data),colnames(aug.vars))

l1=which(is.na(as.numeric(rowSums(g24.r.data.aug))==TRUE))
l2=which(as.numeric(rowSums(g24.r.data.aug))<0)

loc=c(l1,l2)

g24.r.cl.aug=g24.r.data.aug[-loc,]
g24.r.cl.aug=g24.r.cl.aug[-which(g24.r.cl.aug$Q55A==-98),]
g24.r.cl.aug=g24.r.cl.aug[-which(g24.r.cl.aug$Q55B==-98),]
g24.r.cl.aug=g24.r.cl.aug[-which(g24.r.cl.aug$Q55H==-98),]
g24.r.cl.aug=g24.r.cl.aug[-which(g24.r.cl.aug$Q55I==-98),]
g24.r.cl.aug=g24.r.cl.aug[-which(g24.r.cl.aug$Q54==-98),]
g24.r.cl.aug=g24.r.cl.aug[-which(g24.r.cl.aug$Q44==-98),]
g24.r.cl.aug=g24.r.cl.aug[-which(g24.r.cl.aug$Q55F==-98),]
g24.r.cl.aug=g24.r.cl.aug[-which(g24.r.cl.aug$Pol==-98),]

#---Full population analysis---#
cubvisual(g24.r.cl.aug$Q54,xlim=c(0,1),ylim=c(0,1),cex=0.8,csiplot = TRUE,paiplot = TRUE)
locator()

#---GOF Analysis begins---#

x=g24.r.cl.aug$Q55K
pai.hat=0.6799
csi.hat=0.7469

1-0.5*sum(abs(table(x)/nrow(g24.r.cl.aug) - probcub00(4,pai=pai.hat,csi=csi.hat)))

#---Regression analysis begins----#
#---We demonstrate the CUB analysis through question 54 and "politics"---#
#---Readers are encouraged to loop through other questions and demographic details--#

Y=cbind(g24.r.cl.aug$Pol,g24.r.cl.aug$Gender,g24.r.cl.aug$Age,g24.r.cl.aug$Educ,g24.r.cl.aug$Race)
W=cbind(g24.r.cl.aug$Pol,g24.r.cl.aug$Gender,g24.r.cl.aug$Age,g24.r.cl.aug$Educ,g24.r.cl.aug$Race)

#--Q54---#
model<-GEM(Formula(g24.r.cl.aug$Q54~Y|W|W),family="cub",data=g24.r.cl.aug,maxiter=50,toler=1e-2)
param<-coef(model)
param
#makeplot(model)
summary(model)

bet=param[1:6]
gama=param[7:12]

### make the SPE plot
paivet=logis(Y,bet)
csivet=logis(W,gama)

#--politics---#
caption="Panel a, Q54, Politics"
symb=rep(19,nrow(g24.r.cl.aug)); symb[g24.r.cl.aug$Pol==1]=1; symb[g24.r.cl.aug$Pol==2]=3;symb[g24.r.cl.aug$Pol==3]=5;symb[g24.r.cl.aug$Pol==4]=7;symb[g24.r.cl.aug$Pol==5]=9
colo=rep("red",nrow(g24.r.cl.aug)); colo[g24.r.cl.aug$Pol==1]="red";
colo[g24.r.cl.aug$Pol==2]="pink";colo[g24.r.cl.aug$Pol==3]="gold";
colo[g24.r.cl.aug$Pol==4]="cyan";colo[g24.r.cl.aug$Pol==5]="blue"
plot(paivet,csivet,
     cex=1.5,pch=symb,col=colo,
     xlab=expression(pi),ylab=expression(psi),
     main=caption,font.main=4)
legend("topleft",c("Very con.","Conservative","Moderate","Liberal","Very lib."),lty=c(1,1,1,1,1),lwd=c(2,2,2,2,2),col=c("red","pink","gold","cyan","blue"),bty="n")
