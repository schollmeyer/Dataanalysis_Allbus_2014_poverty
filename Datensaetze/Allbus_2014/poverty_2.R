library(compiler)
enableJIT(3)
source("Dissertation\\r_programme_dissertation\\quantiles.R")
getwd()
library(foreign)
dat=read.spss("ZA5240_v2-1-0.sav",to.data.frame=TRUE)
dat=dat[dat$V86!="NOCH SCHUELER",]
dat=dat[dat$V86!="ANDERER ABSCHLUSS",]
#dat=dat[which(!is.na(dat$V86)),]


dat$schulabschluss=1
dat$schulabschluss[dat$V86=="VOLKS-,HAUPTSCHULE"]=2
dat$schulabschluss[dat$V86=="MITTLERE REIFE"]=3
dat$schulabschluss[dat$V86=="FACHHOCHSCHULREIFE"]=4
dat$schulabschluss[dat$V86=="HOCHSCHULREIFE"]=5

dat$hochschulabschluss=0
dat$hochschulabschluss[dat$V99=="BACHELOR" | dat$V100=="BACHELOR"]=1
dat$hochschulabschluss[dat$V99 %in% c("MASTER DIPLOM","MAGISTER","STAATSEXAMEN") | dat$V100 %in% c("MASTER DIPLOM","MAGISTER","STAATSEXAMEN") ]=2
dat$hochschulabschluss[dat$V99 =="PROMOTION" | dat$V100 =="PROMOTION"]=3
dat$hochschulabschluss[dat$V99 =="SONSTIGES"]=NA

HS=c("-","BACHELOR","MASTER/DIPLOM,...","PROMOTION")




# dat$bildung=0
# dat$bildung[dat$V86=="VOLKS-,HAUPTSCHULE"]=1
# dat$bildung[dat$V86=="MITTLERE REIFE"]=2
# dat$bildung[dat$V86=="HOCHSCHULREIFE"]=3
# 
# dat$bildung[dat$V86=="HOCHSCHULREIFE"]=3
# dat$bildung[dat$V86=="HOCHSCHULREIFE"]=3
# dat$bildung[dat$V86=="HOCHSCHULREIFE"]=3
# 
# 
# 
# 
# 
# 
# dat$hochschulabschluss=0
# 
# 




#%dat$hochschule[dat$V95=="GENANNT"]=1
#%dat$hochschule[dat$V96=="GENANNT"]=2


abschluss=as.numeric(dat$V86)
einkommen=as.numeric(dat$V420)
gesundheit=7-as.numeric(dat$V226)
behinderung=as.numeric(dat$V269)
#bildung=as.numeric(dat$V99)


I=which( dat$V202!="KEINE GEDANK. GEMACHT" &!is.na(dat$V610) & ! is.na(dat$V84) & dat$V84>=40 & !is.na(einkommen) & dat$V5=="SPLIT B: F75B" & gesundheit %in% (1:6) & !is.na(gesundheit) & behinderung %in% (1:2)&!is.na(dat$schulabschluss) & !is.na(dat$hochschulabschluss))
X=cbind(einkommen,gesundheit,behinderung,dat$schulabschluss,dat$hochschulabschluss)
#X=cbind(einkommen,gesundheit,behinderung,dat$schulabschluss,dat$hochschulabschluss,dat$V610,3-as.numeric(dat$V202))
#X=cbind(einkommen,gesundheit,dat$schulabschluss,3-as.numeric(dat$V202))
X=X[I,]
#X=na.omit(X)
#I=which(ost=="NEUE BUNDESLAENDER")
ost=dat$V7[I]





###


A=preprocess(X)
m=dim(X)[1]
l=rep(0,m);l2=l
#library(depth)
for(k in (1:m)){l[k]=LEVEL(A,X[k,])}#;l2[k]=depth(X[k,],X)}

plot(ecdf(l[ost=="ALTE BUNDESLAENDER"]),verticals=TRUE,mar=c(5,5,5),main="level function poverty, Germany (Allbus 2014)",xlab=expression(lambda),ylab=expression(F[n](lambda)))
lines(ecdf(l[ost!="ALTE BUNDESLAENDER"]),verticals=TRUE, lty=2)



plot(ecdf(l),verticals=TRUE,main="level function poverty, Germany",xlab=expression(lambda))




v=sort(unique(l))
abline(v=v[11],lty=2,col="grey"       )
abline(v=v[22],lty=2,col="grey"  )
       
       
LEVEL(A,A$system2[15,]) #0.1990148
 LEVEL(A,A$system2[16,]) # 0.3300493


LEVEL(A,A$system2[25,]) ##0.7399015

x_20=A$system2[15,]
x_33=A$system2[16,]
x_75=A$system2[26,]





# abline(v=LEVEL(A,x_20),lty=2)
#abline(v=LEVEL(A,x_33),lty=2)
#abline(v=LEVEL(A,x_75),lty=2)

##############################################################################
x=x_20

Einkommen=levels(dat$V420)[x[1]]
Gesundheit=levels(dat$V226)[7-x[2]]
Behinderung=levels(dat$V269)[x[3]]
Schulabschluss= levels(dat$V86)[x[4]]
Hochschulabschluss=HS[x[5]+1]



 par(cex=1.2)
 par(mar= c(1,1,1,1)*5)
 plot(ecdf(l),lab=c(10,10,10),xlim=c(0,1),mar=c(5,5,5),verticals=TRUE,main="level function poverty, Germany (Allbus 2014)",xlab=expression(lambda),ylab=expression(F[n](lambda)))

a=LEVEL(A,x)


D=0.01
E=0.04
F=0.2
par(cex=0.8)
abline(v=a,lty=2)
text(a-D-F,0.95,adj=0,"income: ")
text(a-D-F,0.95-E,adj=0,"health: ")
#text(a-D-F,0.95-2*E,adj=0,"disability: ")
text(a-D-F,0.95-2*E,adj=0,"high school diploma: ")
#text(a-D-F,0.95-4*E,adj=0,"university degree: ")
par(cex=1)
text(a-D,0.95,adj=1,Einkommen)
text(a-D,0.95-E,adj=1,Gesundheit)
#text(a-D,0.95-2*E,adj=1,Behinderung)
text(a-D,0.95-3*E,adj=1,Schulabschluss)
#text(a-D,0.95-4*E,adj=1,Hochschulabschluss)
text(a+0.015,-0.020,"0.20")
##########################################

x=x_75

Einkommen=levels(dat$V420)[x[1]]
Gesundheit=levels(dat$V226)[7-x[2]]
Behinderung=levels(dat$V269)[x[3]]
Schulabschluss= levels(dat$V86)[x[4]]
Hochschulabschluss=HS[x[5]+1]

par(cex=1)

a=LEVEL(A,x)


D=0.01
E=0.04
F=0.24
par(cex=0.8)
abline(v=a,lty=2)
text(a-D-F,0.95,adj=0,"income: ")
text(a-D-F,0.95-E,adj=0,"health: ")
#text(a-D-F,0.95-2*E,adj=0,"disability: ")
text(a-D-F,0.95-2*E,adj=0,"high school diploma: ")
#text(a-D-F,0.95-4*E,adj=0,"university degree: ")
par(cex=1)
text(a-D,0.95,adj=1,Einkommen)
text(a-D,0.95-E,adj=1,Gesundheit)
#text(a-D,0.95-2*E,adj=1,Behinderung)
text(a-D,0.95-3*E,adj=1,Schulabschluss)
#text(a-D,0.95-4*E,adj=1,Hochschulabschluss)
text(a+0.015,-0.020,"0.78")


################################################

## Test ost/west



plot(ecdf(l[ost=="ALTE BUNDESLAENDER"]),verticals=TRUE,mar=c(5,5,5),main="level function poverty, Germany",xlab=expression(lambda),ylab=expression(F[n](lambda)))
lines(ecdf(l[ost!="ALTE BUNDESLAENDER"]),verticals=TRUE, lty=2)
legend(0,0.9, legend=c("west","east"),lty=c(1,2))


F_west= ecdf(l[ost=="ALTE BUNDESLAENDER"])
F_east=ecdf(l[ost!="ALTE BUNDESLAENDER"])
T=(1:1015)/1015
eps=0.000001
T=sort(c(T-eps,T+eps))
D=abs(F_east(T)-F_west(T))
for( x in T[which(D==max(D))]){
lines(rbind(c(x,F_west(x)),c(x,F_east(x))),col="grey")
}

lines(ecdf(l[ost!="ALTE BUNDESLAENDER"]),verticals=TRUE, lty=2)
lines(ecdf(l[ost=="ALTE BUNDESLAENDER"]),verticals=TRUE, )

text(0.55,0.56, expression(D[max]==0.227),cex=0.9)
#for(l in (1:5)){
# 
#m=dim(A$system2)[1]
#lambda=rep(0,m);L=lambda
#for(k in (1:m)){
#  C=A$system2[k,l]
#  lambda[k]=LEVEL(A,A$system2[k,])
#  L[k]= mean(X[,l]<=C)
#  }
#  
#lines(lambda,L,lty=l)}
  
  







## maximale Anzahl levels
lmax=length(unique(l))
for( G in (1:1000)){
II=sample((1:1015),size=2385,replace=TRUE);XX=X[II,];AA=preprocess(XX)
 m=dim(XX)[1]
 l=rep(0,m)
for(k in (1:m)){l[k]=LEVEL(AA,XX[k,])}#;l2[k]=depth(X[k,],X)}
#print(length(unique(l)))
lmax=max(lmax,length(unique(l)))
print(lmax)}


## analytisch: 39

# Simulation
a.cont=rep(0,10000)
a.discrete=rep(0,10000)
a.test=rep(0,10000)
for(k in (1:10000)){
x=runif(1015)
y=floor(39*runif(1015))/39
z=floor(40*sqrt(runif(1015)))

J=sample((1:1015),size=657,replace=FALSE)
a.cont[k]=ks.test(x[J],x[-J])$statistic
a.discrete[k]=ks.test(y[J],y[-J])$statistic
a.test[k]=ks.test(z[J],z[-J])$statistic
}


# Simulation  dominanz
a.cont=rep(0,10000)
a.discrete=rep(0,10000)
a.test=rep(0,10000)
for(k in (1:10000)){
x=runif(1015)
y=floor(39*runif(1015))/39
z=floor(40*sqrt(runif(1015)))

J=sample((1:1015),size=657,replace=FALSE)
a.cont[k]=ks.test(x[J],x[-J],alternative ="less")$statistic
a.discrete[k]=ks.test(y[J],y[-J],alternative ="less")$statistic
a.test[k]=ks.test(z[J],z[-J],alternative ="less")$statistic
}

plot(ecdf(a.cont))
lines(ecdf(a.discrete),col="red")
lines(ecdf(a.test),col="blue")  
  


## Permutation test

a.perm=rep(0,10000)
for(k in (1:10000)){
J=sample((1:1015),size=657,replace=FALSE)
a.perm[k]=ks.test(l[J],l[-J])$statistic}



















##plot(ecdf(l[I]))
##lines(ecdf(l[-I]),col="red")
#
#
#F=ecdf(l[I])
#G=ecdf(l[-I])
# plot(T,F(T)-G(T),type="l")
#
####
#
#cor(na.omit(X))
#pairs.panels(X)
#
#F1=ecdf(X[,1])
#F2=ecdf(X[,2])
#F3=ecdf(X[,3])
#F4=ecdf(X[,4])
#Y=cbind(F1(X[,1]),F2(X[,2]),F3(X[,3]),F4(X[,4]) )


#pairs.panles(Y)