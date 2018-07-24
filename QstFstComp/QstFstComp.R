setwd("C:/Users/joann/Dropbox/Professional/Duke/Rausher_lab/ChapterTwoFstQst/Analysis/QstFstComp")
setwd("E:/Users/Joanna/Dropbox/Professional/Duke/Rausher_lab/ChapterTwoFstQst/Analysis/QstFstComp")


install.packages("devtools")
library(devtools)
install_github("kjgilbert/QstFstComp")

.libPaths()

library(QstFstComp)
?QstFstComp

trace(QstFstComp, edit = T)
#At https://github.com/kjgilbert/QstFstComp/blob/master/R/QstFstComp.R there are several opportunities to "# uncomment the above line if you want to output a vector of the distribution of Q-F values to the console" - edit local copy accordingly.

#Missing data needs to be formatted as "NA" 

snpmatrix<-read.table("MATRIX", na.strings = "..", stringsAsFactors = F) #Imports cleanly if I set correct NA character!

snpmatrix2<-read.table("MATRIX2", na.strings = "..", stringsAsFactors = F) #Imports cleanly if I set correct NA character!

minisnpmatrix<-subset(snpmatrix[,c(1,3:27081)])
#minisnpmatrix<-subset(snpmatrix2[,c(1,3:2354)])


#head(snpmatrix)
#str(snpmatrix)
#str(snpmatrix2)
#summary(snpmatrix)
#class(snpmatrix)
#snpmatrix<-snpmatrix[order(snpmatrix$V2),]



#minisnpmatrix<-subset(snpmatrix[,c(1,3:27081)])
#str(minisnpmatrix)
#class(minisnpmatrix)
#head(minisnpmatrix)
#minisnpmatrix


SampleTraits<-read.csv("SampleTraits3-12-final.csv")


colnames(SampleTraits)
Internode1<-SampleTraits[1:3]
Internode2<-SampleTraits[c(1:2,4)]
Internode3<-SampleTraits[c(1:2,5)]
flowers.day<-SampleTraits[c(1:2,6)]
Pedunclelengthnodetoflower<-SampleTraits[c(1:2,7)]
CorollaLength<-SampleTraits[c(1:2,8)]
Cor.Width<-SampleTraits[c(1:2,9)]
AnthersBelow<-SampleTraits[c(1:2,10)]
Nectar_volume<-SampleTraits[c(1:2,11)]
Sugar_conc_average<-SampleTraits[c(1:2,12)]


str(minisnpmatrix)

sink(file="Internode13_12.txt",append = F)
QstFstComp(minisnpmatrix, Internode1, numpops=2, nsim=1000, #Run QstFstComp
           breeding.design="half.sib.dam", output="full", dam.offspring.relatedness = 1)
sink() #CLOSE THE SINK!!!!!!!!!!!!!!!!!!

#Construct graphs
    Internode1Fst<-read.table("Internode1Sites.txt",skip=20,nrow=112, fill = T)
    #View(Internode1Fst)
    #str(Internode1Fst)
    #class(Internode1Fst)
    Internode1Fst<-Internode1Fst[,2:10]
    Internode1Fst<-unlist(Internode1Fst)
    Internode1Fst<-Internode1Fst[!is.na(Internode1Fst)]
    boxplot(Internode1Fst)#, add = T)
    length(Internode1Fst)
    #Internode1Fst
    #hist(Internode1Fst)
287-144
    Internode1Qst<-read.table("Internode1Sites.txt",skip=144,nrow=143, fill = T)
    #View(Internode1Qst)
    #str(Internode1Fst)
    #class(Internode1Fst)
    Internode1Qst<-Internode1Qst[,2:8]
    Internode1Qst<-unlist(Internode1Qst)
    Internode1Qst<-Internode1Qst[!is.na(Internode1Qst)]
    max(Internode1Qst)
    sort(Internode1Qst)
    boxplot(Internode1Qst)
    length(Internode1Qst)
    #Internode1Qst
    #hist(Internode1Qst, breaks = 50)
    #hist(Internode1Qst)
    
    boxplot(Internode1Fst, Internode1Qst, names=c("Fst", "Qst"))
    
    hist(Internode1Fst, col=rgb(1,0,0,0.5),xlim=c(-.5, 2),ylim=c(0,200),  
      main="Internode 1", xlab="Fst (red) and Qst (blue)")
    hist(Internode1Qst, col=rgb(0,0,1,0.5), add=T, breaks=50)
    plot(Internode1Fst,Internode1Qst)
    
    hist(read.table("Internode1QminusFvalues_2018-02-07_11-21-00.txt")$V1)

    
    
    
    sink(file="Internode2Sites3_12.txt",append = F)
    QstFstComp(minisnpmatrix, Internode2, numpops=2, nsim=1000, 
               breeding.design="half.sib.dam", output="full", dam.offspring.relatedness = 1)
    sink() #CLOSE THE SINK!!!!!!!!!!!!!!!!!!
    1+1
    Internode2Fst<-read.table("Internode2.txt",skip=20,nrow=77, fill = T)
    #View(Internode2Fst)
    #str(Internode2Fst)
    #class(Internode2Fst)
    Internode2Fst<-Internode2Fst[,2:14]
    Internode2Fst<-unlist(Internode2Fst)
    Internode2Fst<-Internode2Fst[!is.na(Internode2Fst)]
    boxplot(Internode2Fst)#, add = T)
    length(Internode2Fst)
    #Internode2Fst
    #hist(Internode2Fst)
    
    Internode2Qst<-read.table("Internode2.txt",skip=109,nrow=112, fill = T)
    #View(Internode2Qst)
    #str(Internode2Fst)
    #class(Internode2Fst)
    Internode2Qst<-Internode2Qst[,2:10]
    Internode2Qst<-unlist(Internode2Qst)
    Internode2Qst<-Internode2Qst[!is.na(Internode2Qst)]
    max(Internode2Qst)
    min(Internode2Qst)
    sort(Internode2Qst)
    boxplot(Internode2Qst)
    length(Internode2Qst)
    #Internode2Qst
    
    hist(Internode2Qst, breaks = 200)
    #hist(Internode2Qst)
    
    boxplot(Internode2Fst, Internode2Qst, names=c("Fst", "Qst"))
    (-.5, 2),ylim=c(0,200)
    hist(Internode2Fst, col=rgb(1,0,0,0.5),xlim=c(-10, 10),ylim=c(0,800),  
         main="Internode 2", xlab="Fst (red) and Qst (blue)")
    hist(Internode2Qst, col=rgb(0,0,1,0.5), add=T, breaks=100)
    
    hist(Internode2Fst, col=rgb(1,0,0,0.5),xlim=c(-.5, 2),ylim=c(0,200), border=NULL,  
         main="Internode 2 closeup", xlab="Fst (red) and Qst (blue)")
    hist(Internode2Qst, col=rgb(0,0,1,0.5), add=T, breaks=50)
    
    
    plot(Internode2Fst,Internode2Qst)
    
    hist(read.table("Internode2QminusFvalues_2018-02-07_11-21-00.txt")$V1)
    
       
    
    
    
    sink(file="Internode33_12.txt",append = F)
    QstFstComp(minisnpmatrix, Internode3, numpops=2, nsim=1000, 
               breeding.design="half.sib.dam", output="full", dam.offspring.relatedness = 1)
    sink() #CLOSE THE SINK!!!!!!!!!!!!!!!!!!
    1+1
    Internode3Fst<-read.table("Internode3.txt",skip=20,nrow=77, fill = T)
    #View(Internode3Fst)
    #str(Internode3Fst)
    #class(Internode3Fst)
    Internode3Fst<-Internode3Fst[,2:14]
    Internode3Fst<-unlist(Internode3Fst)
    Internode3Fst<-Internode3Fst[!is.na(Internode3Fst)]
    boxplot(Internode3Fst)#, add = T)
    length(Internode3Fst)
    #Internode3Fst
    #hist(Internode3Fst)
    
    Internode3Qst<-read.table("Internode3.txt",skip=109,nrow=112, fill = T)
    #View(Internode3Qst)
    #str(Internode3Fst)
    #class(Internode3Fst)
    Internode3Qst<-Internode3Qst[,2:10]
    Internode3Qst<-unlist(Internode3Qst)
    Internode3Qst<-Internode3Qst[!is.na(Internode3Qst)]
    max(Internode3Qst)
    min(Internode3Qst)
    #sort(Internode3Qst)
    boxplot(Internode3Qst)
    length(Internode3Qst)
    #Internode3Qst
    #hist(Internode3Qst, breaks = 50)
    hist(Internode3Qst)
    
    boxplot(Internode3Fst, Internode3Qst, names=c("Fst", "Qst"))
    
    hist(Internode3Fst, col=rgb(1,0,0,0.5),xlim=c(-10, 10),ylim=c(0,800),  
         main="Internode 3", xlab="Fst (red) and Qst (blue)")
    hist(Internode3Qst, col=rgb(0,0,1,0.5), add=T, breaks=50)
    
    hist(Internode3Fst, col=rgb(1,0,0,0.5),xlim=c(-.5, 2),ylim=c(0,200), border=NULL,  
         main="Internode 3 closeup", xlab="Fst (red) and Qst (blue)")
    hist(Internode3Qst, col=rgb(0,0,1,0.5), add=T, breaks=50)
    
    
    
    
    sink(file="FlowersPerDay312.txt",append = F)
    QstFstComp(minisnpmatrix, flowers.day, numpops=2, nsim=1000, 
               breeding.design="half.sib.dam", output="full", dam.offspring.relatedness = 1)
    sink() #CLOSE THE SINK!!!!!!!!!!!!!!!!!!
    1+1
    FlowersPerDayFst<-read.table("FlowersPerDay.txt",skip=20,nrow=77, fill = T)
    #View(FlowersPerDayFst)
    #str(FlowersPerDayFst)
    #class(FlowersPerDayFst)
    FlowersPerDayFst<-FlowersPerDayFst[,2:14]
    FlowersPerDayFst<-unlist(FlowersPerDayFst)
    FlowersPerDayFst<-FlowersPerDayFst[!is.na(FlowersPerDayFst)]
    boxplot(FlowersPerDayFst)#, add = T)
    length(FlowersPerDayFst)
    #FlowersPerDayFst
    #hist(FlowersPerDayFst)
    
    FlowersPerDayQst<-read.table("FlowersPerDay.txt",skip=109,nrow=112, fill = T)
    #View(FlowersPerDayQst)
    #str(FlowersPerDayFst)
    #class(FlowersPerDayFst)
    FlowersPerDayQst<-FlowersPerDayQst[,2:10]
    FlowersPerDayQst<-unlist(FlowersPerDayQst)
    FlowersPerDayQst<-FlowersPerDayQst[!is.na(FlowersPerDayQst)]
    max(FlowersPerDayQst)
    min(FlowersPerDayQst)
    #sort(FlowersPerDayQst)
    boxplot(FlowersPerDayQst)
    length(FlowersPerDayQst)
    #FlowersPerDayQst
    #hist(FlowersPerDayQst, breaks = 50)
    hist(FlowersPerDayQst)
    
    boxplot(FlowersPerDayFst, FlowersPerDayQst, names=c("Fst", "Qst"))
    
    hist(FlowersPerDayFst, col=rgb(1,0,0,0.5),xlim=c(-2, 3),ylim=c(0,250),  
         main="Flowers per Day", xlab="Fst (red) and Qst (blue)")
    hist(FlowersPerDayQst, col=rgb(0,0,1,0.5), add=T, breaks=50)
    
    hist(FlowersPerDayFst, col=rgb(1,0,0,0.5),xlim=c(-.5, 2),ylim=c(0,200), border=NULL,  
         main="Flowers per Day", xlab="Fst (red) and Qst (blue)")
    hist(FlowersPerDayQst, col=rgb(0,0,1,0.5), add=T, breaks=50)
    
    
    
    
    sink(file="InflorescenceLengthSites312.txt",append = F)
    QstFstComp(minisnpmatrix, Pedunclelengthnodetoflower, numpops=2, nsim=1000, 
               breeding.design="half.sib.dam", output="full", dam.offspring.relatedness = 1)
    sink() #CLOSE THE SINK!!!!!!!!!!!!!!!!!!
    1+1
    InflorescenceLengthFst<-read.table("InflorescenceLength.txt",skip=20,nrow=77, fill = T)
    #View(InflorescenceLengthFst)
    #str(InflorescenceLengthFst)
    #class(InflorescenceLengthFst)
    InflorescenceLengthFst<-InflorescenceLengthFst[,2:14]
    InflorescenceLengthFst<-unlist(InflorescenceLengthFst)
    InflorescenceLengthFst<-InflorescenceLengthFst[!is.na(InflorescenceLengthFst)]
    boxplot(InflorescenceLengthFst)#, add = T)
    length(InflorescenceLengthFst)
    #InflorescenceLengthFst
    #hist(InflorescenceLengthFst)
    
    InflorescenceLengthQst<-read.table("InflorescenceLength.txt",skip=109,nrow=112, fill = T)
    #View(InflorescenceLengthQst)
    #str(InflorescenceLengthFst)
    #class(InflorescenceLengthFst)
    InflorescenceLengthQst<-InflorescenceLengthQst[,2:10]
    InflorescenceLengthQst<-unlist(InflorescenceLengthQst)
    InflorescenceLengthQst<-InflorescenceLengthQst[!is.na(InflorescenceLengthQst)]
    max(InflorescenceLengthQst)
    min(InflorescenceLengthQst)
    #sort(InflorescenceLengthQst)
    boxplot(InflorescenceLengthQst)
    length(InflorescenceLengthQst)
    #InflorescenceLengthQst
    #hist(InflorescenceLengthQst, breaks = 50)
    hist(InflorescenceLengthQst)
    
    boxplot(InflorescenceLengthFst, InflorescenceLengthQst, names=c("Fst", "Qst"))
    
    hist(InflorescenceLengthFst, col=rgb(1,0,0,0.5),xlim=c(-0.5, 2),ylim=c(0,200),  
         main="Inflorescence Length", xlab="Fst (red) and Qst (blue)")
    hist(InflorescenceLengthQst, col=rgb(0,0,1,0.5), add=T, breaks=50)
    
    hist(InflorescenceLengthFst, col=rgb(1,0,0,0.5),xlim=c(-1, 2),ylim=c(0,200), border=NULL,  
         main="Inflorescence Length closeup", xlab="Fst (red) and Qst (blue)")
    hist(InflorescenceLengthQst, col=rgb(0,0,1,0.5), add=T, breaks=50)
    
    
    
    
    
    sink(file="CorollaLengthSites312.txt",append = F)
    QstFstComp(minisnpmatrix, CorollaLength, numpops=2, nsim=1000, 
               breeding.design="half.sib.dam", output="full", dam.offspring.relatedness = 1)
    sink() #CLOSE THE SINK!!!!!!!!!!!!!!!!!!
    1+1
    CorollaLengthFst<-read.table("CorollaLengthSites.txt",skip=20,nrow=112, fill = T)
    #View(CorollaLengthFst)
    #str(CorollaLengthFst)
    #class(CorollaLengthFst)
    CorollaLengthFst<-CorollaLengthFst[,2:10]
    CorollaLengthFst<-unlist(CorollaLengthFst)
    CorollaLengthFst<-CorollaLengthFst[!is.na(CorollaLengthFst)]
    boxplot(CorollaLengthFst)#, add = T)
    length(CorollaLengthFst)
    #CorollaLengthFst
    #hist(CorollaLengthFst)
    
    CorollaLengthQst<-read.table("CorollaLengthSites.txt",skip=144,nrow=143, fill = T)
    #View(CorollaLengthQst)
    #str(CorollaLengthFst)
    #class(CorollaLengthFst)
    CorollaLengthQst<-CorollaLengthQst[,2:8]
    CorollaLengthQst<-unlist(CorollaLengthQst)
    CorollaLengthQst<-CorollaLengthQst[!is.na(CorollaLengthQst)]
    max(CorollaLengthQst)
    min(CorollaLengthQst)
    #sort(CorollaLengthQst)
        boxplot(CorollaLengthQst)
    length(CorollaLengthQst)
    #CorollaLengthQst
    #hist(CorollaLengthQst, breaks = 50)
    hist(CorollaLengthQst)
    
    boxplot(CorollaLengthFst, CorollaLengthQst, names=c("Fst", "Qst"))
    
    hist(CorollaLengthFst, col=rgb(1,0,0,0.5),xlim=c(-0.5, 1),ylim=c(0,200),  
         main="Corolla Length", xlab="Fst (red) and Qst (blue)")
    hist(CorollaLengthQst, col=rgb(0,0,1,0.5), add=T, breaks=50)
    
    hist(CorollaLengthFst, col=rgb(1,0,0,0.5),xlim=c(-1, 2),ylim=c(0,200), border=NULL,  
         main="Corolla Length closeup", xlab="Fst (red) and Qst (blue)")
    hist(CorollaLengthQst, col=rgb(0,0,1,0.5), add=T, breaks=50)
    
    QFVal<-read.table("QminusFvalues_2018-02-28_11-32-09.txt")
    hist(QFVal$V1)
    head(QFVal)    
     
    
    
    sink(file="CorollaWidthSites312.txt",append = F)
    QstFstComp(minisnpmatrix, Cor.Width, numpops=2, nsim=1000, 
               breeding.design="half.sib.dam", output="full", dam.offspring.relatedness = 1)
    sink() #CLOSE THE SINK!!!!!!!!!!!!!!!!!!
    1+1
    CorollaWidthFst<-read.table("CorollaWidth.txt",skip=20,nrow=77, fill = T)
    #View(CorollaWidthFst)
    #str(CorollaWidthFst)
    #class(CorollaWidthFst)
    CorollaWidthFst<-CorollaWidthFst[,2:14]
    CorollaWidthFst<-unlist(CorollaWidthFst)
    CorollaWidthFst<-CorollaWidthFst[!is.na(CorollaWidthFst)]
    boxplot(CorollaWidthFst)#, add = T)
    length(CorollaWidthFst)
    #CorollaWidthFst
    #hist(CorollaWidthFst)
    
    CorollaWidthQst<-read.table("CorollaWidth.txt",skip=109,nrow=112, fill = T)
    #View(CorollaWidthQst)
    #str(CorollaWidthFst)
    #class(CorollaWidthFst)
    CorollaWidthQst<-CorollaWidthQst[,2:10]
    CorollaWidthQst<-unlist(CorollaWidthQst)
    CorollaWidthQst<-CorollaWidthQst[!is.na(CorollaWidthQst)]
    max(CorollaWidthQst)
    min(CorollaWidthQst)
    #sort(CorollaWidthQst)
    boxplot(CorollaWidthQst)
    length(CorollaWidthQst)
    #CorollaWidthQst
    #hist(CorollaWidthQst, breaks = 50)
    hist(CorollaWidthQst)
    
    boxplot(CorollaWidthFst, CorollaWidthQst, names=c("Fst", "Qst"))
    
    hist(CorollaWidthFst, col=rgb(1,0,0,0.5),xlim=c(-0.5, 2),ylim=c(0,200),  
         main="Corolla Width", xlab="Fst (red) and Qst (blue)")
    hist(CorollaWidthQst, col=rgb(0,0,1,0.5), add=T, breaks=50)
    
    hist(CorollaWidthFst, col=rgb(1,0,0,0.5),xlim=c(-1, 2),ylim=c(0,200), border=NULL,  
         main="Corolla Width closeup", xlab="Fst (red) and Qst (blue)")
    hist(CorollaWidthQst, col=rgb(0,0,1,0.5), add=T, breaks=50)
    
    
    
    
    
    
    sink(file="NectarVolume312.txt",append = F)
    QstFstComp(minisnpmatrix, Nectar_volume, numpops=2, nsim=1000, 
               breeding.design="half.sib.dam", output="full", dam.offspring.relatedness = 1)
    sink() #CLOSE THE SINK!!!!!!!!!!!!!!!!!!
    1+1
    NectarVolumeFst<-read.table("NectarVolume.txt",skip=20,nrow=77, fill = T)
    #View(NectarVolumeFst)
    #str(NectarVolumeFst)
    #class(NectarVolumeFst)
    NectarVolumeFst<-NectarVolumeFst[,2:14]
    NectarVolumeFst<-unlist(NectarVolumeFst)
    NectarVolumeFst<-NectarVolumeFst[!is.na(NectarVolumeFst)]
    boxplot(NectarVolumeFst)#, add = T)
    length(NectarVolumeFst)
    #NectarVolumeFst
    #hist(NectarVolumeFst)
    
    NectarVolumeQst<-read.table("NectarVolume.txt",skip=109,nrow=112, fill = T)
    #View(NectarVolumeQst)
    #str(NectarVolumeFst)
    #class(NectarVolumeFst)
    NectarVolumeQst<-NectarVolumeQst[,2:10]
    NectarVolumeQst<-unlist(NectarVolumeQst)
    NectarVolumeQst<-NectarVolumeQst[!is.na(NectarVolumeQst)]
    max(NectarVolumeQst)
    min(NectarVolumeQst)
    #sort(NectarVolumeQst)
    boxplot(NectarVolumeQst)
    length(NectarVolumeQst)
    #NectarVolumeQst
    #hist(NectarVolumeQst, breaks = 50)
    hist(NectarVolumeQst)
    
    boxplot(NectarVolumeFst, NectarVolumeQst, names=c("Fst", "Qst"))
    
    hist(NectarVolumeFst, col=rgb(1,0,0,0.5),xlim=c(-0.5, 2),ylim=c(0,200),  
         main="Nectar Volume", xlab="Fst (red) and Qst (blue)")
    hist(NectarVolumeQst, col=rgb(0,0,1,0.5), add=T, breaks=50)
    
    hist(NectarVolumeFst, col=rgb(1,0,0,0.5),xlim=c(-1, 2),ylim=c(0,200), border=NULL,  
         main="Nectar Volume", xlab="Fst (red) and Qst (blue)")
    hist(NectarVolumeQst, col=rgb(0,0,1,0.5), add=T, breaks=50)
    
    
    
    
    sink(file="NectarSugar312.txt",append = F)
    QstFstComp(minisnpmatrix, Sugar_conc_average, numpops=2, nsim=1000, 
               breeding.design="half.sib.dam", output="full", dam.offspring.relatedness = 1)
    sink() #CLOSE THE SINK!!!!!!!!!!!!!!!!!!
    1+1
    NectarSugarFst<-read.table("NectarSugar.txt",skip=20,nrow=77, fill = T)
    #View(NectarSugarFst)
    #str(NectarSugarFst)
    #class(NectarSugarFst)
    NectarSugarFst<-NectarSugarFst[,2:14]
    NectarSugarFst<-unlist(NectarSugarFst)
    NectarSugarFst<-NectarSugarFst[!is.na(NectarSugarFst)]
    boxplot(NectarSugarFst)#, add = T)
    length(NectarSugarFst)
    #NectarSugarFst
    #hist(NectarSugarFst)
    
    NectarSugarQst<-read.table("NectarSugar.txt",skip=109,nrow=112, fill = T)
    #View(NectarSugarQst)
    #str(NectarSugarFst)
    #class(NectarSugarFst)
    NectarSugarQst<-NectarSugarQst[,2:10]
    NectarSugarQst<-unlist(NectarSugarQst)
    NectarSugarQst<-NectarSugarQst[!is.na(NectarSugarQst)]
    max(NectarSugarQst)
    min(NectarSugarQst)
    #sort(NectarSugarQst)
    boxplot(NectarSugarQst)
    length(NectarSugarQst)
    #NectarSugarQst
    #hist(NectarSugarQst, breaks = 50)
    hist(NectarSugarQst)
    
    boxplot(NectarSugarFst, NectarSugarQst, names=c("Fst", "Qst"))
    
    hist(NectarSugarFst, col=rgb(1,0,0,0.5),xlim=c(-0.5, 2),ylim=c(0,200),  
         main="Nectar Sugar Concentration", xlab="Fst (red) and Qst (blue)")
    hist(NectarSugarQst, col=rgb(0,0,1,0.5), add=T, breaks=50)
    
    hist(NectarSugarFst, col=rgb(1,0,0,0.5),xlim=c(-1, 2),ylim=c(0,200), border=NULL,  
         main="Nectar Volume", xlab="Fst (red) and Qst (blue)")
    hist(NectarSugarQst, col=rgb(0,0,1,0.5), add=T, breaks=50)
    
    
    sink(file="PollenNumberShared.txt",append = F)
    QstFstComp(sharedminisnpmatrix, PollenPerOvule, numpops=2, nsim=1000, 
               breeding.design="half.sib.dam", output="full", dam.offspring.relatedness = 1)
    sink() #CLOSE THE SINK!!!!!!!!!!!!!!!!!!
    
    
    sink(file="PollenNumber312.txt",append = F)
    QstFstComp(minisnpmatrix, PollenPerOvule, numpops=2, nsim=1000, 
               breeding.design="half.sib.dam", output="full", dam.offspring.relatedness = 1)
    sink() #CLOSE THE SINK!!!!!!!!!!!!!!!!!!
    1+1
    PollenNumberFst<-read.table("PollenNumber.txt",skip=20,nrow=78, fill = T)
    #View(PollenNumberFst)
    #str(PollenNumberFst)
    #class(PollenNumberFst)
    PollenNumberFst<-PollenNumberFst[,2:14]
    PollenNumberFst<-unlist(PollenNumberFst)
    PollenNumberFst<-PollenNumberFst[!is.na(PollenNumberFst)]
    boxplot(PollenNumberFst)#, add = T)
    length(PollenNumberFst)
    #PollenNumberFst
    #hist(PollenNumberFst)
    
    PollenNumberQst<-read.table("PollenNumber.txt",skip=109,nrow=100, fill = T)
    View(PollenNumberQst)
    #str(PollenNumberFst)
    #class(PollenNumberFst)
    PollenNumberQst<-PollenNumberQst[,2:11]
    PollenNumberQst<-unlist(PollenNumberQst)
    PollenNumberQst<-PollenNumberQst[!is.na(PollenNumberQst)]
    max(PollenNumberQst)
    min(PollenNumberQst)
    #sort(PollenNumberQst)
    boxplot(PollenNumberQst)
    length(PollenNumberQst)
    #PollenNumberQst
    #hist(PollenNumberQst, breaks = 50)
    hist(PollenNumberQst)
    
    boxplot(PollenNumberFst, PollenNumberQst, names=c("Fst", "Qst"))
    
    hist(PollenNumberFst, col=rgb(1,0,0,0.5),xlim=c(-0.5, 2),ylim=c(0,200),  
         main="Pollen Number", xlab="Fst (red) and Qst (blue)")
    hist(PollenNumberQst, col=rgb(0,0,1,0.5), add=T, breaks=50)
    
    hist(PollenNumberFst, col=rgb(1,0,0,0.5),xlim=c(-1, 2),ylim=c(0,200), border=NULL,  
         main="Nectar Volume", xlab="Fst (red) and Qst (blue)")
    hist(PollenNumberQst, col=rgb(0,0,1,0.5), add=T, breaks=50)
    
    
    sink(file="AnthersBelow312.txt",append = F)
    QstFstComp(minisnpmatrix, AnthersBelow, numpops=2, nsim=1000, 
               breeding.design="half.sib.dam", output="full", dam.offspring.relatedness = 1)
    sink() #CLOSE THE SINK!!!!!!!!!!!!!!!!!!
    
    
    
    sink(file="AnthersBelowSitesShared.txt",append = F)
    QstFstComp(sharedminisnpmatrix, AnthersBelow, numpops=2, nsim=1000, 
               breeding.design="half.sib.dam", output="full", dam.offspring.relatedness = 1)
    sink() #CLOSE THE SINK!!!!!!!!!!!!!!!!!!
    
    
    
    
    
    
    
    sink(file="LeafDissection.txt",append = F)
    QstFstComp(minisnpmatrix, Leafdissection, numpops=2, nsim=1000, 
               breeding.design="half.sib.dam", output="full", dam.offspring.relatedness = 1)
    sink() #CLOSE THE SINK!!!!!!!!!!!!!!!!!!
    1+1
    LeafDissectionFst<-read.table("LeafDissection.txt",skip=20,nrow=78, fill = T)
    #View(LeafDissectionFst)
    #str(LeafDissectionFst)
    #class(LeafDissectionFst)
    LeafDissectionFst<-LeafDissectionFst[,2:14]
    LeafDissectionFst<-unlist(LeafDissectionFst)
    LeafDissectionFst<-LeafDissectionFst[!is.na(LeafDissectionFst)]
    boxplot(LeafDissectionFst)#, add = T)
    length(LeafDissectionFst)
    #LeafDissectionFst
    #hist(LeafDissectionFst)
    
    LeafDissectionQst<-read.table("LeafDissection.txt",skip=109,nrow=112, fill = T)
    #View(LeafDissectionQst)
    #str(LeafDissectionFst)
    #class(LeafDissectionFst)
    LeafDissectionQst<-LeafDissectionQst[,2:10]
    LeafDissectionQst<-unlist(LeafDissectionQst)
    LeafDissectionQst<-LeafDissectionQst[!is.na(LeafDissectionQst)]
    max(LeafDissectionQst)
    min(LeafDissectionQst)
    #sort(LeafDissectionQst)
    boxplot(LeafDissectionQst)
    length(LeafDissectionQst)
    #LeafDissectionQst
    #hist(LeafDissectionQst, breaks = 50)
    hist(LeafDissectionQst)
    
    boxplot(LeafDissectionFst, LeafDissectionQst, names=c("Fst", "Qst"))
    
    hist(LeafDissectionFst, col=rgb(1,0,0,0.5),xlim=c(-0.5, 2),ylim=c(0,200),  
         main="Leaf dissection", xlab="Fst (red) and Qst (blue)")
    hist(LeafDissectionQst, col=rgb(0,0,1,0.5), add=T, breaks=50)
    
    hist(LeafDissectionFst, col=rgb(1,0,0,0.5),xlim=c(-1, 2),ylim=c(0,200), border=NULL,  
         main="Nectar Volume", xlab="Fst (red) and Qst (blue)")
    hist(LeafDissectionQst, col=rgb(0,0,1,0.5), add=T, breaks=50)
    
    
    

