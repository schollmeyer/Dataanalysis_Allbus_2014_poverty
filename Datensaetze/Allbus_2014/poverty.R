setwd("C:/Users/georg/Documents/GIT3/diss/Dissertation/Datensaetze/Datensaetze/Allbus_2014")
#source("L:\\Git\\diss\\Dissertation\\r_programme_dissertation\\quantiles.R")
library(foreign)
dat=read.spss("ZA5240_v2-1-0.sav",to.data.frame=TRUE)




X=cbind(dat$V87,dat$V88,dat$V89,dat$V90,dat$V91,dat$V92)

























abschluss=as.numeric(dat$V86)
einkommen=as.numeric(dat$V420)
gesundheit=as.numeric(dat$V226)
behinderung=as.numeric(dat$V269)

I=which(abschluss %in% (1:5) & !is.na(einkommen) & dat$V5=="SPLIT B: F75B" & gesundheit %in% (1:6) & behinderung %in% (1:2))

X=cbind(abschluss,einkommen,7-gesundheit,behinderung)
X=X[I,]
ost=(dat$V7[I]=="NEUE BUNDESLAENDER")


A=preprocess(X)
m=dim(X)[1]
l=rep(0,m);l2=l
library(depth)
for(k in (1:m)){l[k]=LEVEL(A,X[k,]);l2[k]=depth(X[k,],X)}

M1=lm(abschluss[I]~dat$V7[I]+  dat$V81[I]     +dat$V84[I]+ dat$V104[I])
M2=lm(einkommen[I]~dat$V7[I]+  dat$V81[I]     +dat$V84[I]+ dat$V104[I])
M3=lm(gesundheit[I]~dat$V7[I]+  dat$V81[I]     +dat$V84[I]+ dat$V104[I])
M4=lm(behinderung[I]~dat$V7[I]+  dat$V81[I]     +dat$V84[I]+ dat$V104[I])

M5=lm(l~dat$V7[I]+  dat$V81[I]     +dat$V84[I]+ dat$V104[I])
