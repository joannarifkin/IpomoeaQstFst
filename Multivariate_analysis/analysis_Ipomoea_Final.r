#Analysis of Ipomoea data based on Chapuis et al. 2008
#Using transformed data from TransformIpomoea.R 
#what we do here is
#(1) estimate the mean squares and Sum of squares by manova
#(2) do the test based on these covariance matrix estimates
#I adapted the "package neutrality test.r" slightly into "package neutrality test_rounding.r" to compensate for "matrix not symmetrical" errors due to loss of significance
setwd("C:/.../codes_Qst_Fst/codes Qst Fst")
#install.packages("corpcor")
#source("package neutrality test.r")
source("package neutrality test_rounding.r")
library(MASS);library(car); library(KernSmooth);library(matrixcalc);library(dplyr)
#the data in data.frame format
#data=datag2;
#install.packages("matrixcalc")
#mydata=read.csv("TransformedIpomoeaData.csv")
mydata=read.csv("TransformedIpomoeaDataWithAnthersBelow.csv")


#library(hierfstat)
head(mydata)
colnames(mydata)


# =============  / test species effect with no population term ==========================
mydata=read.csv("TransformedIpomoeaDataWithAnthersBelow.csv")
n=dim(mydata)[2]-4;nn=table(mydata$Numeric_Species);nbpop=length(nn);
traits=names(mydata[-c(1:5)]);
effect=paste(traits,collapse=",")
form=as.formula(paste("cbind(",effect,")~factor(Numeric_Species)",sep=""))
manov=summary(manova(form,data=mydata)) 
SSh=manov$SS$"factor(Numeric_Species)";
SSw=manov$SS$Residuals;
dfbh=manov$stats[1,1];
dfw=manov$stats[2,1]; #df between habitat, between pops within habitats, within pop
nf=mean(nn)-1/nbpop*((mean(nn^2)-mean(nn)^2)/mean(nn));#compute the equivalent df with unbalanced designs
MSw=SSw/dfw; MSbh=SSh/dfbh;
Gw=MSw;
Gbh=(MSbh-MSw)/nf; #between species
write.csv(Gw, "between_accession_gmatrix_AnthersBelow_.csv")
write.csv(Gbh, "between_species_gmatrixAnthersBelow_accessions.csv")


#------- Test the population effect after correcting for habitat ---------------

DF=c(dfw,dfbh);G=list(Gw,Gbh);MS=list(MSw, MSbh);    #create the inputs for k.prop()


#DF=c(dfw,dfb);G=list(Gw,GBtransform);MS=list(MSw,MSb);    #create the inputs for k.prop()


is.symmetric.matrix(Gw);is.symmetric.matrix(Gb)
is.symmetric.matrix(MSw);is.symmetric.matrix(MSb)
is.symmetric.matrix(Gbh);is.symmetric.matrix(MSbh)
is.positive.definite(Gw);is.positive.definite(Gb)
is.positive.definite(MSw);is.positive.definite(MSb)

#-------- Retrieve the CI for rho and p-value for the proportionality test -----
#rho_st from test between G matrices
testG=k.prop(DF,G,prints = T)
signif(testG$rho[[2]],4)
paste("rho_P (population) =",signif(testG$rho[[2]],2))

print("95% CI for rho_P :");signif(testG$CI[[2]],2) #Not relevant if df = 2





#------------------Resampling the data from accessions reassigned to different species---------------
View(mydata)

#to values of the statistic d obtained after randomizing accessions between the two habitats (keeping the same number of accessions per habitat).
library(dplyr)
library(gdata)


################# Randomly reassign accessions between species
ResampleList <- list()
OrderList<-list()
resamplelist<-list()
colnames(mydata)
for(i in 1:1000){
  mydata=read.csv("TransformedIpomoeaDataWithAnthersBelow.csv")
  n=dim(mydata)[2]-4;nn=table(mydata$Numeric_Species);nbpop=length(nn);
  traits=names(mydata[-c(1:5)]);
  effect=paste(traits,collapse=",")
  form=as.formula(paste("cbind(",effect,")~factor(Numeric_Species)",sep=""))
  ToResample<-data.frame(mydata$Numeric_Species,mydata$Accession.Dam)
  colnames(ToResample)<-c("Numeric_Species","Accession.Dam")
  ToResample<-unique(ToResample)
  ToResample$Numeric_Species<-sample(ToResample$Numeric_Species)
  resample<-left_join(ToResample, mydata[,3:14], by="Accession.Dam")
  resample$Numeric_Species
  manov=summary(manova(form,data=resample)) 
  SSh=manov$SS$"factor(Numeric_Species)";
  #SSb=manov$SS$"factor(Numeric_Species):factor(Accession.Dam)";
  SSw=manov$SS$Residuals;
  dfbh=manov$stats[1,1];
  #dfb=manov$stats[2,1];
  dfw=manov$stats[2,1]; #df between habitat, between pops within habitats, within pop
  nf=mean(nn)-1/nbpop*((mean(nn^2)-mean(nn)^2)/mean(nn));
  #nf=mean(nn)-1/nbpop*((mean(nn^2)-mean(nn)^2)/mean(nn)); #compute the equivalent df with unbalanced designs
  MSw=SSw/dfw; MSbh=SSh/dfbh;
  Gw=MSw;#within and between population nested within in habitats
  Gbh=(MSbh-MSw)/nf; #between habitats
  DF=c(dfw,dfbh);G=list(Gw,Gbh);MS=list(MSw, MSbh); 
  testG=k.prop(DF,G, prints=T)
  paste(signif(testG$rho)[[2]])
  #x[[i]]<-paste("rho_P  =",signif(testG$rho[[2]],2))
  ResampleList[[i]]<-paste(signif(testG$rho[[2]]))
  OrderList[[i]]<-ToResample$Numeric_Species
  resamplelist[[i]]<-resample
  #keep(c("ResampleList", "OrderList","resamplelist"))
  #rm(list = setdiff(ls(), lsf.str()))
  rm(testG)
  rm(ToResample);  rm(mydata);  rm(resample); rm(DF);  rm(G); rm(Gb); rm(Gbh); rm(manov);rm(MS);  rm(MSb);rm(MSbh);rm(MSw);rm(SSh);rm(SSb);rm(SSw)
}

quantile(as.numeric(ResampleList),probs=c(.0,.95))


#View(G[[2]])
warnings()


