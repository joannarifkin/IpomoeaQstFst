#install.packages("matrixcalc")
#install.packages("hierfstat")
library(adegenet)
library(hierfstat)
library(matrixcalc)
library(dplyr)



#Import data

Accessions<-read.table("C:/.../NumericAccessionSampleConverter.txt", header = T) #import list of samples


snpmatrixAll<-read.table("C:/.../MATRIX", na.strings = "..", stringsAsFactors = F) #Import SNPs in same order (ALL)

allmatrixwAccessions<-left_join(snpmatrixAll, Accessions, by=c("V2"="Sample")) #Join samples to SNP matrix

ncol(allmatrixwAccessions)
allmatrixwAccessions[,27082]
levels<-allmatrixwAccessions[,c(1,27082)]
loci<-allmatrixwAccessions[,-c(1:2,27082)]
varcomp.glob(levels, loci)
boot.vc(levels, loci)

varcomp.glob(data.frame(snpmatrixAll[,1]),snpmatrixAll[,-c(1:2)])$F #Calculate hierarchical F statistics
boot.vc(snpmatrixAll[,1],snpmatrixAll[,-c(1:2)])$ci


#Repeat with shared SNPs
snpmatrixShared<-read.table("C:/.../MATRIX2", na.strings = "..", stringsAsFactors = F) 
head(snpmatrixShared)
sharedmatrixwAccessions<-left_join(snpmatrixShared, Accessions, by=c("V2"="Sample"))
#sharedmatrixwAccessions[,(ncol(sharedmatrixwAccessions))]
#colnames(sharedmatrixwAccessions)
head(sharedmatrixwAccessions[,1:4])
head(sharedmatrixwAccessions[,c(1:4,2355)])

Accessions$Sample
levels<-sharedmatrixwAccessions[,c(1,2355)] #Species and population
#sharedmatrixwAccessions[,c(1,(ncol(sharedmatrixwAccessions)))]
loci<-sharedmatrixwAccessions[,-c(1:2,2355)]
varcomp.glob(levels, loci)
test.within(loci, test="PopNumeric", within="V1", nperm=100)
boot.vc(levels, loci)
wc(loci, sharedmatrixwAccessions$1)
warnings()
snpmatrixShared$V1
wc(snpmatrixShared[,-2])
varcomp.glob(data.frame(snpmatrixShared[,1]),snpmatrixShared[,-c(1:2)])$F
boot.vc(snpmatrixShared[,1],snpmatrixShared[,-c(1:2)])$ci
traits
head(testmatrix)
testmatrixwAccessions<-left_join(testmatrix, Accessions, by=c("V2"="Sample"))
head(testmatrixwAccessions)
varcomp.glob(data.frame(testmatrixwAccessions[,30]),testmatrixwAccessions[,-c(1:2,21:39)])$F
varcomp.glob(data.frame(testmatrixwAccessions[,1]),testmatrixwAccessions[,-c(1:2,21:39)])$F
colnames(testmatrixwAccessions)
levels<-testmatrixwAccessions[,c(1,30)]
loci<-testmatrixwAccessions[,-c(1:2,29:30)]
varcomp.glob(levels, loci)
wc(testmatrixwAccessions[,-2])
