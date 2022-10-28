library(foreign)
a <- read.spss(file="Z:/Datensaetze/Allbus_1991/ZA1990_v2-0-0.sav",use.value.labels=FALSE)


I= !(a$V21==9 |     a$V22==9 | a$V29==9 |a$V30==9 |a$V26==9 |a$V27==9 ) & a$V432 ==96
#(a$V327 ==1 | a$V327==2)

#b=subset(a,I)

E.G=(a$V21+a$V22)[I] >= (a$V26+ a$V27)[I]
E.R=(a$V21+a$V22)[I] >= (a$V29+ a$V30)[I]
R.G=(a$V29+a$V30)[I] >= (a$V26+ a$V27)[I]

G.E=(a$V21+a$V22)[I] <= (a$V26+ a$V27)[I]
R.E=(a$V21+a$V22)[I] <= (a$V29+ a$V30)[I]
G.R=(a$V29+a$V30)[I] <= (a$V26+ a$V27)[I]



J=(E.R & E.G)

sum(J[R.G==TRUE])
sum(J[G.R==TRUE])


m=(a$V21+a$V22+a$V23+a$V24+a$V25+a$V26+a$V27+a$V28+a$V29+a$V30+a$V31+a$V32+a$V33)/13


E.G=(pmin(a$V21,a$V22)[I])  >= (pmax(a$V26, a$V27))[I]
E.R=(pmin(a$V21,a$V22)[I] )>= (pmax(a$V29, a$V30))[I]
R.G=(pmin(a$V29,a$V30)[I]) >= (pmax(a$V26, a$V27))[I]

G.E=(pmax(a$V21,a$V22)[I])  <= (pmin(a$V26, a$V27))[I]
R.E=(pmax(a$V21,a$V22)[I] )<= (pmin(a$V29, a$V30))[I]
G.R=(pmax(a$V29,a$V30)[I]) <= (pmin(a$V26, a$V27))[I]
J=(E.R & E.G)

sum(J[R.G==TRUE])
sum(J[G.R==TRUE])