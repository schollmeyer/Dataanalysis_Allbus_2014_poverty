load("Sicherung_beipiel_mult_Ungleicheit.RData")    # Lade Workspace, wo Ergebnisse aller Berechnungen schon vorhanden sind



library(foreign)                ## Lade n?tige Libraries
library(spatstat)               ## ben?tigt zur Berchnung empirischer Verteilungsfunktionen mit Gewichten (Funktion ewcdf)
library(lpSolve)                ## sowohl lpSolve als auch lpSolveAPI greifen auf lpp_solve zur?ck
library(lpSolveAPI)              ## (http://lpsolve.sourceforge.net/5.5/)  wobei lpSOlveAPI den zugrif auf mehr Einstellunge                                  ## n erlaubt
                                ## nur eines der Packages muss geladen sein
 
library(plyr)                  ## Wird zur gewichteten Darstellung von Datentabellen ben?tigt

library(Matrix)

library(igraph)          ## benoetigt f?r Berchnung maximales bipartites Matching f?r Berechnung der Wiete einer geordneten Menge (nur am Schluss benoetigt)

dyn.load("main.dll")
source("main_konsolidiert.r")

gurobi=function(model,params){           ##           Anstelle des R packages Gurobi plus installierter Gurobi-Software ( www.gurobi.com ) kann 
                                         ##   auch lpSolve mit Zusatzfunktion gurobi verwendet werden 
 ans=lp(direction = model$modelsense, objective.in=model$obj, const.mat=model$A, const.dir=model$sense, const.rhs=model$rhs,binary.vec=which(model$vtypes=="B")) 

return(list(objval=ans$objval,x=ans$solution,rest=ans))}



gurobi=function(model,params){       #Alternativ: anstelle von lpSolve kann auch lpSOlveAPI verwendet werden. Dies 
                                     # hat den Vorteil, das man auch mit Sparse Matrizen arbeiten kann
 m=dim(model$A)[1]
 n=dim(model$A)[2]
 ans=make.lp(nrow=m,ncol=n)
 set.objfn(ans,model$obj)

 for(k in (1:m)){
   ind=which(model$A[k,]!=0)
   add.constraint(ans,xt=model$A[k,ind],type=model$sense[k],indices=ind,rhs=model$rhs[k])
 }
 
 set.type(ans,which(model$vtypes=="B"),"binary")
 lp.control(ans,sense= paste(model$modelsense,"imize",sep=""))
 solve(ans)
 x=get.primal.solution(ans)
 N=length(x)
return(list(objval=get.objective(ans),x=x[((N-n+1):N)],rest=ans))}





dat=read.spss("ZA5240_v2-1-0.sav",to.data.frame=TRUE)       ## Daten einlesen
                                                            #Vorher: Pfad entsprechend setzen

dat=dat[which(dat$V5=="SPLIT B: F75B"),]## W?hle Split F075 B

#   FRAGEBOGENSPLIT F075
#   Erg?nzender Kurzkommentar zur Variablenbeschreibung:
#   Fragebogensplit Gesundheitszustand
#   Die Frage zum allgemeinen Gesundheitszustand (F075A/F075B) wurde im Erhebungsjahr 2014 in einem
#   Splitverfahren erhoben, um die Auswirkung verschiedener Antwortskalen auf das Antwortverhalten zu testen.
#   Die Befragten wurden sowohl in Split A als auch in Split B gefragt, wie sie ihren Gesundheitszustand im Allgemeinen
#   beschreiben w?rden. Die eine H?lfte der Befragten (Split A) erhielt die Antwortkategorien ?Sehr gut?, ?Gut?,
#   ?Zufriedenstellend?, ?Weniger gut" und ?Schlecht". Bei der anderen H?lfte der Befragten (Split B) wurde? als zus?tzliche
#   Antwortkategorie ?Ausgezeichnet? angeboten.












# Variablenreport zu V102 (Bildung)

# V102 BEFR.: ISCED 2011
# Erg?nzender Kurzkommentar zur Variablenbeschreibung:
#   International Standard Classification of Education (ISCED) 2011
# 1 Level 1 - Primary education
# 2 Level 2 - Lower secondary education
# 3 Level 3 - Upper secondary education
# 4 Level 4 - Post secondary non-tertiary education
# 5 Level 5 - Short-cycle tertiary education
# 6 Level 6 - Bachelor's or equivalent level
# 7 Level 7 - Master's or equivalent level
# 8 Level 8 - Doctoral or equivalent level
# 94 Noch Sch?ler
# 99 Nicht klassifizierbar
# Ableitung der Daten:
#   Diese Variable wurde mit Hilfe der Angaben zum allgemeinbildenden Schulabschluss (V86) und dem
# berufsqualifizierenden Ausbildungsabschluss (V87-V100) gebildet.



# vgl Codebook
#  F075B
#  <Falls Teilnahme an Split B (Code 2 in V5).>
#  (Int.: Liste 75B vorlegen!)
#  Ich m?chte Ihnen nun einige Fragen zu Ihrer Gesundheit stellen. Wie w?rden Sie Ihren Gesundheitszustand im
#  Allgemeinen beschreiben?
#  (Int.: Bitte achten Sie darauf, dass die richtige Liste, 75B, vorliegt!)
#  0 Keine Teilnahme an Split B (Code 1 in V5)
#  1 Ausgezeichnet
#  2 Sehr gut
#  3 Gut
#  4 Zufriedenstellend
#  5 Weniger gut
#  6 Schlecht
#  9 Keine Angabe

##############
#############
###########

##marginale Analyse


pdf("marginal_analysis.pdf")



 par(mfrow=c(2,2),mar=c(4,3,1,1))

C1="black"  ##Farbe  M?nner
C2="grey"   ### Farbe Frauen
T1=1        ## Linientyp M?nner
T2=2        ## Linientyp Frauen



####Income

Z=cbind(dat$V81,dat$V419,dat$V870);Z=na.omit(Z)
i=which(Z[,1]==1)
j=which(Z[,1]==2)
F1=ewcdf(Z[i,2],weights=Z[i,3]/sum(Z[i,3]))
F2=ewcdf(Z[j,2],weights=Z[j,3]/sum(Z[j,3]))
xx=Z[,2]
min(F1(xx)-F2(xx))
max(F1(xx)-F2(xx))

plot(F1,xlim=c(45,60000),log="x",verticals=TRUE,xlab="Income",ylab="",main="",col=C1,lty=T1)
lines(F2,lty=T2,col=C2,verticals=TRUE)




#Bildung

Z=cbind(dat$V81,dat$V102,dat$V870);Z=na.omit(Z)
i=which(Z[,1]==1)
j=which(Z[,1]==2)
F1=ewcdf(Z[i,2],weights=Z[i,3]/sum(Z[i,3]))
F2=ewcdf(Z[j,2],weights=Z[j,3]/sum(Z[j,3]))
xx=Z[,2]
min(F1(xx)-F2(xx))
max(F1(xx)-F2(xx))

#plot(F1,verticals=TRUE,xlab="Education",ylab="",main="Education International Standard Classification of Education (ISCED 2011)",col=C1,lty=T1)
plot(F1,verticals=TRUE,xlab="Education (ISCED 2011)",ylab="",main="",col=C1,lty=T1)
lines(F2,lty=T2,col=C2,verticals=TRUE)


#Gesundheit

Z=cbind(dat$V81,7-as.numeric(dat$V226),dat$V870);Z=na.omit(Z)
i=which(Z[,1]==1)
j=which(Z[,1]==2)
F1=ewcdf(Z[i,2],weights=Z[i,3]/sum(Z[i,3]))
F2=ewcdf(Z[j,2],weights=Z[j,3]/sum(Z[j,3]))
xx=Z[,2]
min(F1(xx)-F2(xx))
max(F1(xx)-F2(xx))

plot(F1,verticals=TRUE,xlab="Health",ylab="",main="",col=C1,lty=T1)
lines(F2,lty=T2,col=C2,verticals=TRUE)









#Beruf

Z=cbind(dat$V81,as.numeric(dat$V107),dat$V870);Z=na.omit(Z)
i=which(Z[,1]==1)
j=which(Z[,1]==2)
F1=ewcdf(Z[i,2],weights=Z[i,3]/sum(Z[i,3]))
F2=ewcdf(Z[j,2],weights=Z[j,3]/sum(Z[j,3]))
xx=Z[,2]
min(F1(xx)-F2(xx))
max(F1(xx)-F2(xx))

#plot(F1,verticals=TRUE,xlab="Occupation",ylab="",main="Occupation: Standard International Occupational Prestige Scale (SIOPS)",col=C1,lty=T1)
plot(F1,verticals=TRUE,xlab="Occupation (SIOPS)",ylab="",main="",col=C1,lty=T1)
lines(F2,lty=T2,col=C2,verticals=TRUE)



 dev.off()







 ##3-dimensionale Analyse 
 
 
 Z=cbind(dat$V81,      dat$V7  ,  dat$V419 ,                            dat$V102 ,                                            7-as.numeric(dat$V226),    dat $V870)
###     Geschlecht    OST     ,  Einkommen (offen + Listen Abfrage)    Bildung gem. ISCED 2011           Beruf               Gesundheit                  Samplegewicht
                                #offen:  dat$V417 ,                                                      # dat$V107 ,


                                education.names=c("Primary education","Lower secondary education","Upper secondary education","Post secondary non-tertiary education","Short-cycle tertiary education","Bachelor's or equivalent level", "Master's or equivalent level","Doctoral or equivalent level")






Z=na.omit(Z)
X=Z[,c(3,4,5)]  #betrachte 3 Dimensionen Einkommen, Bildung und Gesundheit


gesundheit.names=c("Schlecht","Weniger gut", "Zufriedenstellend", "Gut", "Sehr gut", "Ausgezeichnet")
gesundheit.names.eng=c("bad","suboptimal", "satisfactory", "good", "very good", "excellent")      # Achtung: eigene ?bersetzung, checken!!!

 wx=(Z[,1]==1)*Z[,6]     ##Gewichtsvektor f?r Subpopulation der M?nner
 wy=(Z[,1]==2)*Z[,6]     ## Gewichtsvektor f?r Subpopulation der Frauen

 v=wx/sum(wx)-wy/sum(wy)    ## Differenzvektor beschreibt Differenz der Anteile der M?nner und der Frauen in Oberhalbmenge


XX=weighted.repr(X,v)    ### gweichtete repr?sentation vereinfacht berechnung (Personen mit gleicher Auspr?gung werden zu einem Datenpunkt mit entsprechendem Gewicht


I=incidence(XX$Xw)       #### Berechnung <= Relation I mit Interpretation x <= y iff x in allen Dimensionen kleinergleich y
model=sd1.lp(I)     #### erstellt lineares Program (in einer Liste) f?r Checken von stochastischer Dominanz ?ber Oberhalbmengenansatz
                         ###  Y <=_SD Y iff P(X in U ) <= P(Y in U) f?r jede Oberhalbmenge U
                         ### also betrachte max P(x in U) -P( y in U)
                         ### bzw. min  P(x in U) -P( y in U)  (x entpsicht M?nner, y entspricht Frauen

model$A=Matrix(model$A,sparse=TRUE,ncol=dim(model$A)[1],nrow=dim(model$A[2]))  ## macht Contsraintmatrix sparse, spart Speicerplatz,   geht jedoch nur bei Verwendung von lpSolveAPI, ansonsten auskommentieren



model$obj=XX$yw            ### ?bergabe der Zielfunktion (Vektor v in gewichteter Darstellung an Liste, die das Lineare     Programm enth?lt


model$modelsense="max"     ### maximire!



bmax=gurobi(model)         ### Hier: Durchf?hrung der eigentlichen Optimierung


bmax$objval                ### Maximum
### bmax$objval=  0.364798


bound
model$modelsense="min"     ## Minimierung


bmin=gurobi(model)
bmin$objval

### bmin$objval =  -0.01212844


## Frauen fast stochastisch dominiert von M?nnern

## Wo wird Maximum angenommen?

i.max=minimal.elements(t(I),bmax$x>=0.5)

i.max
#  i.max= 92 187 406 421 472 479 506 508 537
# Maximum wird auf Oberhalbmenge, die von 9 Elementen erzeugt wird, angenommen


# Darstellung ueber Unterhalbmengen
j.max=maximal.elements(t(I),bmax$x==0)

j.max
#  j.max= 9  65 104 138 349 382 386 402 420 468 473 474 624 673
# Maximum wird auf Unterhalbmenge, die von 14 Elementen erzeugt wird angenommen



i.min=minimal.elements(t(I),bmin$x>=0.5)

i.min
#  i.min =  9  13 169 822

# Minimum wird auf Oberhalbmenge, die von 4 Elementen erzeugt wird angenommen


# Darstellung ueber Unterhalbmengen
j.min=maximal.elements(I,bmin$x==0)

j.min
#  j.min= 38 694
# Minimum wird auf Unterhalbmenge, die von 2 Elementen erzeugt wird angenommen

X.out=XX$Xw
X.out[,3]=gesundheit.names.eng[as.integer(X.out[,3])]
X.out[,2]=education.names[as.integer(X.out[,2])]

colnames(X.out)=c("Income (Euro)","Education (ISCED 2011)","Health (self-reported)")

XXX=weighted.repr(X,Z[,6])

Q.out=XXX$Xw
weights=XXX$yw
weights=weights/sum(weights)

#F1=ewcdf(-Q.out[,1],weights)
#F2=ewcdf(-Q.out[,2],weights)
#F3=ewcdf(-Q.out[,3],weights)
#Q.out[,1]=F1(-Q.out[,1])
#Q.out[,2]=F2(-Q.out[,2])          # Berechne U[0,1]-Transformation der Raender 
#Q.out[,3]=F3(-Q.out[,3])


F1=ewcdf(Q.out[,1],weights)           # F?r downsetanalyse
F2=ewcdf(Q.out[,2],weights)
F3=ewcdf(Q.out[,3],weights)
Q.out[,1]=F1(Q.out[,1])
Q.out[,2]=F2(Q.out[,2])          # Berechne U[0,1]-Transformation der Raender 
Q.out[,3]=F3(Q.out[,3])



Q.out=round(Q.out,2)

X.out[i.max,]        ## Wie sieht der ''Rand'# der Oberhalbmenge, wo bmax angenommen wird, aus?



XX2=weighted.repr(X,wx+wy)
T=array(0,c(9,5))
RAND=rep(0,9)
DIFF=rep(0,9)
for(k in (1:9)){for(l in (1:3)){
 RAND[k]=sum(XX2$yw[which((t(I))[i.max[k],]==1)])/length(wx)
 DIFF[k]=sum(XX$yw[which((t(I))[i.max[k],]==1)])
 T[k,l]=paste(X.out[i.max[k],l]," (",Q.out[i.max[k],l],") ",sep="")}}
  T[,5]=round(RAND,2)
  T[,4]=round(DIFF,2)
 colnames(T)=c("Income (Euro)","Education (ISCED 2011)","Health (self-reported)","difference","above")
 
 xtable(T)
 
 RAND=rep(0,4)
 DIFF=rep(0,4)
 T=array(0,c(6,5))
for(k in (1:6)){for(l in (1:3)){
RAND[k]=sum(XX2$yw[which((t(I))[i.min[k],]==1)])/length(wx)

 DIFF[k]=sum(XX$yw[which((t(I))[i.min[k],]==1)])
 T[k,l]=paste(X.out[i.min[k],l]," (",Q.out[i.min[k],l],") ",sep="")}}
  T[,5]=round(RAND,2)
  T[,4]=round(DIFF,4)
colnames(T)=c("Income (Euro)","Education (ISCED 2011)","Health (self-reported)","difference","above")
 
 xtable(T)
 
 
 

X.out[i.min,]       ## Wie sieht der ''Rand'# der Oberhalbmenge, wo bmin angenommen wird, aus?

### Inferenz via Resampling:







 


##########################



#########  Skalierung der Weite (f?r sp?ter)   Dauert sehr lange!!

if(FALSE){

I10=width.scaling(XX$Xw,10) ## width=21
I15=width.scaling(XX$Xw,15) ## width=26
I20=width.scaling(XX$Xw,20) ## width=29
I25=width.scaling(XX$Xw,25) ## width=32
I30=width.scaling(XX$Xw,30) ## width=33
I1=width.scaling(XX$Xw,1)  ## width=4
I2=width.scaling(XX$Xw,2)   ## Width=7
I3=width.scaling(XX$Xw,3)   ## width=9
I4=width.scaling(XX$Xw,4)   ## width=10
I5=width.scaling(XX$Xw,5)   ## width=13
I6=width.scaling(XX$Xw,6)   ### width=16
I7=width.scaling(XX$Xw,7)   ## width=17
I8=width.scaling(XX$Xw,8)   ## width=17
I9=width.scaling(XX$Xw,9)   ## width=18


}
VCD=c(4,7,9,10,13,16,16,17,17,18,21,26,29,32,33)


indexs=list()
indexs[[1]]=I1
indexs[[2]]=I2
indexs[[3]]=I3
indexs[[4]]=I4
indexs[[5]]=I5
indexs[[6]]=I6
indexs[[7]]=I7
indexs[[8]]=I8
indexs[[9]]=I9
indexs[[10]]=I10
indexs[[11]]=I15
indexs[[12]]=I20
indexs[[13]]=I25
indexs[[14]]=I30



########## Inferenz ohne Tamming   Dauert lange

if(FALSE){

nrep=10000
Max.statistic=rep(0,nrep);Min.statistic=Max.statistic

set.seed(1234567)

  
  model=sd1.lp(I)     

model$A=Matrix(model$A,sparse=TRUE,ncol=dim(model$A)[1],nrow=dim(model$A[2]))  
for(k in (1:nrep)){
  
  i=sample((1:(dim(Z)[1])), size=length(which(Z[,1]==1)),replace=FALSE)
  z=rep(2,dim(Z)[1])
  z[i]=1
  
  wx=(z==1)*Z[,6]     ##Gewichtsvektor f?r Subpopulation der M?nner
  wy=(z==2)*Z[,6]     ## Gewichtsvektor f?r Subpopulation der Frauen
  
  v=wx/sum(wx)-wy/sum(wy)    ## Differenzvektor beschreibt Differenz der Anteile der M?nner und der Frauen in Oberhalbmenge
  
  
  XX=weighted.repr(X,v)  
  
  
  
  #I=incidence(XX$Xw)       
  #model=sd1.lp(I)     
  
  
  #model$A=Matrix(model$A,sparse=TRUE,ncol=dim(model$A)[1],nrow=dim(model$A[2]))  
  
  
  model$obj=XX$yw            
  
  
  model$modelsense="max"     ### maximire!
  
  
  
  bmax=gurobi(model,params=list(OutputFlag=0))   
  Max.statistic[k]=bmax$objval
  print(c(k,"max:",quantile(Max.statistic[(1:k)],0.95)))
  
  model$modelsense="min"     ### minimiere!
  
  
  
  bmin=gurobi(model,params=list(OutputFlag=0))   
  Min.statistic[k]=bmin$objval
  print(c(k,"min:",quantile(Min.statistic[(1:k)],0.95)))
  
}

 


}





###### Tamming#####     Dauert lange

if(FALSE){

nrep=10000
max.statistic=array(0,c(nrep,14));min.statistic=max.statistic

set.seed(1234567)
for(l in (1:14)){
  
  model=sd1.lp(I,gen.index=indexs[[l]])     

model$A=Matrix(model$A,sparse=TRUE,ncol=dim(model$A)[1],nrow=dim(model$A[2]))  
for(k in (1:nrep)){
  
  i=sample((1:(dim(Z)[1])), size=length(which(Z[,1]==1)),replace=FALSE)
  z=rep(2,dim(Z)[1])
  z[i]=1
  
  wx=(z==1)*Z[,6]     ##Gewichtsvektor f?r Subpopulation der M?nner
  wy=(z==2)*Z[,6]     ## Gewichtsvektor f?r Subpopulation der Frauen
  
  v=wx/sum(wx)-wy/sum(wy)    ## Differenzvektor beschreibt Differenz der Anteile der M?nner und der Frauen in Oberhalbmenge
  
  
  XX=weighted.repr(X,v)  
  
  
  
  #I=incidence(XX$Xw)       
  #model=sd1.lp(I)     
  
  
  #model$A=Matrix(model$A,sparse=TRUE,ncol=dim(model$A)[1],nrow=dim(model$A[2]))  
  
  
  model$obj=XX$yw            
  
  
  model$modelsense="max"     ### maximire!
  
  
  
  bmax=gurobi(model,params=list(OutputFlag=0))   
  max.statistic[k,l]=bmax$objval
  print(c(k,"max:",quantile(max.statistic[(1:k)],0.95)))
  
  model$modelsense="min"     ### minimiere!
  
  
  
  bmin=gurobi(model,params=list(OutputFlag=0))   
  min.statistic[k,l]=bmin$objval
  print(c(k,"min:",quantile(min.statistic[(1:k)],0.95)))
  
}

 }


 }
##### Graphiken

par(mar=c(4,5,4,4))
 plot(ecdf(max.statistic[,1]),xlim=c(0,0.17),lty=1,main=expression(Empirical~distribution~of~D[xy]^{"+"}),xlab=expression(D[xy]^{"+"}), ylab=expression(F[D[xy]^{"+"}]))

for(k in (2:14)){
lines( ecdf(max.statistic[,k]),lty=k,col=k)
                 
}

legend(0.13,0.9,legend=c("VC=4","VC=7","VC=9","VC=10","VC=13","VC=16","VC=16","VC=17","VC=17","VC=18","VC=21","VC=26","VC=29","VC=32","VC=33"),lty=(1:14),col=(1:14))

w=max.statistic[,1];w=(w-mean(w))/sd(w)
plot(ecdf(w))
for(k in (2:14)){
w=max.statistic[,k];w=(w-mean(w))/sd(w)
lines( ecdf(w),lty=k,col=k)


}


