#this code is adapted from the code for the preliminary transformation for the phenotypic data from (Chapuis et al. 2008) on Galba truncatula
#It has to be run before doing the multivariate Qst analysis
#The output is one dataframe of transformed family mean trait values
#the raw data must be in the same working directory as the code
#before analyzing (see methods of Chapuis et al. 2008)

#set the working directory to where the r codes and touthab.txt are
setwd("C:/.../codes_Qst_Fst/codes Qst Fst")



library(MASS)

#Visual inspection of data and creation of family means
#Accession.Dam = population / family index

repl=read.csv("FocalTraitsOnlyFormattedForChapuisTransformation.csv",header=TRUE, na.strings = "NA")#data non scalées
head(repl) #Inspect data - optional 
#hist(repl$Anthersbelow) #Inspect data - optional
#plot(repl$Anthersbelow, repl$MaxAnther.stigmadistancetoptotop...stigmabelow) #Inspect data - optional
#cor(as.numeric(repl$Anthersbelow), as.numeric(repl$MaxAnther.stigmadistancetoptotop...stigmabelow), use="pairwise.complete.obs", method="pearson") #Inspect data - optional
#cor(as.numeric(repl$MINAnther.stigmadistancetoptotop...stigmabelow), as.numeric(repl$MaxAnther.stigmadistancetoptotop...stigmabelow), use="pairwise.complete.obs", method="pearson") #Inspect data - optional


colnames(repl) #List data columns 
#Select focal columns
repl<-repl[,c(1:5,7:9,12, 15:16,19:20)] #Anthers below


#View(repl)

nbfam=length(levels(factor(repl$Accession.Dam)));
nbt=dim(repl)[2]
datans=matrix(0,nrow=nbfam,ncol=nbt)
for(tr in 1:nbt) 
{
  zob=tapply(repl[,tr],repl$Accession.Dam,mean,na.rm=T)
  datans[,tr]=zob}
datans=as.data.frame(datans);
names(datans)=names(repl);
#View(repl)
#View(datans)



#================ TRANSFORM DATA Ipomoea lacunosa ================

data0=datans;
#Transform NAs temorarily into 1000s (they will be removed later)
nana=is.na(data0);
nbna=length(data0[nana]);
data0[nana]=rep(1000,nbna); #1000 is used as a placeholder to transform and then remove NAs

nomstr=names(data0[,-c(1:4)]); #List trait columns
nbt=length(nomstr);

#Relevant if I want to group by trait category
#------------------ separation of early & late | elimination of NA's -----------------

colnames(data0)


nb=dim(data0)[1]
nbt=dim(data0)[2]

#transforms values <0 into NA (code = 1000)
for (tr in 5:nbt)  
{data0[,tr]=ifelse(data0[,tr]<=0,1000,data0[,tr])}

#remove families that are NA for all traits - not necessary here
#test=apply(ifelse(early[,c(4:nbte)]==1000,0,1),1,sum)
#early[test==0,]
#early=early[test>0,]
#test=apply(ifelse(late[,c(4:nbtl)]==1000,0,1),1,sum)
#late[test==0,]
#late=late[test>0,]

#proportion of remaining NA
length(data0[data0==1000])/((nbt-5)*nb)



#------------------- Box Cox: transform to gaussian ----------------------------
# transformation: x-> x^lambda  the best lambda value os printed for each trait
#NB: to keep ordering, you need an increasing function: x -> x^l with l>0 or -x^l with l<0
# TRAITS
datag=data0; lambda=numeric(nbt);
for (tr in 5:nbt)  
{
  xx=datag[,tr];x=datag[datag[,tr]!=1000,tr]; #Remove NA and null values
  XX=boxcox(x~1,lambda=seq(-4,4,1/100),plotit=F) #Identify the best transform for vector x
  lb=XX$x[XX$y==max(XX$y)];  #Value of lambda that maximizes log-likelihood (Gaussian) for each trait 
  datag[datag[,tr]!=1000,tr]=x^lb;lambda[tr]=lb;
  print(paste(names(datag)[tr],lb));
}
datag1=datag;datag1[datag1==1000]=NA; #Now the NAs are put back in


#--------------------- scaling by the mean -------------------------------------
datag2=datag1;
for (tr in 5:nbt)
{datag2[,tr]=datag2[,tr]/abs(mean(datag2[,tr],na.rm=TRUE))}


#Flip sign of anthers below, sum internodes, and flowers/day to maintain direction of original trait correlations after transformation. 
datag2$Anthersbelow=-datag2$Anthersbelow
datag2$flowers.day=-datag2$flowers.day
datag2$Sum_internodes=-datag2$Sum_internodes



#------------------ final checks ------------------------------------------------

par(mfrow=c(3,4))
hist(datag2$flowers.day);hist(datag2$Pedunclelengthnodetoflower);hist(datag2$Cor.Length);hist(datag2$Cor.Width);hist(datag2$MaxAnther.stigmadistancetoptotop...stigmabelow);hist(datag2$StigmaLength);hist(datag2$Nectar_volume);hist(datag2$Sugar_conc_average);hist(datag2$PollenPerOvule);hist(datag2$Sum_internodes) #Histograms of transformed traits

#check scaling by the mean (mean (trait2) = 1)
apply(datag2,2,mean,na.rm=T)


# check distribution of transformed traits vs gaussian: Q-Q plot 
#early
par(mfrow=c(2,3));
for (tr in 5:nbt){qqnorm(datag2[is.na(datag2[,tr])==F,tr],main=names(datag2)[tr]);qqline(datag2[,tr])}
#late
par(mfrow=c(2,3));
for (tr in 4:nbtl){qqnorm(late2[is.na(late2[,tr])==F,tr],main=names(late2)[tr]);qqline(late2[,tr])}

#check effect of the transformation on the correlations
par(mfrow=c(1,2));
#early traits
par(mfrow=c(1,1));

#early traits
data0[data0==1000]=NA;
C0=cor(data0[,-c(1:4)],use="pairwise")
hist(C1)
hist(C2)
#colnames(data0)
#colnames(datag1)
#colnames(datag2)
#length(C2)
#length(C1)
#length(C0)
C2=cor(datag2[,-c(1:4)],use="pairwise")
C1=cor(datag1[,-c(1:4)],use="pairwise")
plot(C0[lower.tri(C0)],C2[lower.tri(C2)],main="traits",xlab="original correlations",ylab="correlations after transfo")
plot(C0[lower.tri(C0)],C1[lower.tri(C1)],main="early traits",xlab="original correlations",ylab="correlations after transfo")
xx=seq(-0.1,1,by=0.001);lines(xx,xx)
line(C0[lower.tri(C0)],C2[lower.tri(C2)])
lines(line(c(C0),c(C2)))
View(C0)
View(C2)


write.csv(datag2, "TransformedIpomoeaDataWithAnthersBelow.csv")

##############################################################
