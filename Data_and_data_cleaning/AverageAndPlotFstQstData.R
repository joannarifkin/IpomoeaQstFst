setwd("E:/Users/Joanna/Dropbox/Professional/Duke/Rausher_lab/ChapterTwoFstQst/Analysis")
setwd("C:/Users/joann/Dropbox/Professional/Duke/Rausher_lab/ChapterTwoFstQst/Analysis")

internodes<-read.csv("C:/Users/Joanna/Dropbox/Professional/Duke/Rausher_lab/ChapterTwoFstQst/Data/InternodesCLOnly.csv",header = TRUE)
internodes<-read.csv("C:/Users/joann/Dropbox/Professional/Duke/Rausher_lab/ChapterTwoFstQst/Data/InternodesCLOnly.csv",header = TRUE)
internodes

alltraits<-read.csv("IndividualMeans5-3PopCodesNectarFirstFlower.csv")


library(ggplot2)

head(internodes)

attach(internodes)

hist(Internode1)
hist(Internode2)
hist(Internode3)
class(SpeciesNum)

Internode1
ggplot(internodes,aes(x=as.numeric(Internode1),fill=Species))+geom_histogram(binwidth=1, alpha=0.5, position="identity")

ggplot(internodes,aes(x=as.numeric(Internode2),fill=Species))+geom_histogram(binwidth=2,alpha=0.5, position="identity")

ggplot(internodes,aes(x=as.numeric(Internode3),fill=Species))+geom_histogram(binwidth=3, alpha=0.5, position="identity")

is.na(Internode1)
 
internode1hist

attach(alltraits)

ggplot(alltraits,aes(x=as.numeric(Internode3),fill=Species))+geom_histogram(binwidth=3, alpha=0.5, position="identity")


ggplot(alltraits,aes(x=as.numeric(Cor..Length),fill=Species))+
  geom_histogram(binwidth=1, alpha=0.5, position="identity")+
  xlab("Corolla Length")+ ylim(0,20)


ggplot(alltraits,aes(x=as.numeric(Cor..Width),fill=Species))+
  geom_histogram(binwidth=1, alpha=0.5, position="identity")+
  xlab("Corolla Width")+ ylim(0,20)


Cor..Length
ggplot(alltraits,aes(x=as.numeric(Peduncle.length.node.to.flower),
                     fill=Species))+geom_histogram(binwidth=1, alpha=0.5, position="identity")+
                                                   xlab("Cyme Length")+
        ylim(0,20)











#1,2,3,4, Cdt, Lac, Leu,Aust

ggplot(allmeans,aes(x=Nectar.volume.uL,fill=(Species)))+geom_histogram(binwidth=.1,alpha=.5,position="identity")

ggplot(data=phenology_data, aes(x=Species, y=Leaf.1.open))->leafPlot
ggplot(phenology_data,aes(x=Height_mm_day_21,fill=Species))+geom_histogram(binwidth=25,alpha=.5,position="identity")
ggplot(phenology_data,aes(x=Internode_1_day_21,fill=Species))+geom_histogram(binwidth=2,alpha=.5,position="identity")
ggplot(phenology_data,aes(x=L1_germ,fill=Species))+geom_histogram(binwidth=1,alpha=.5,position="identity")
ggplot(phenology_data,aes(x=Number_leaves_day_21,fill=Species))+geom_histogram(binwidth=1,alpha=.5,position="identity")


Cor._Width_histogram<-(ggplot(F2means,aes(x=Cor._Width))+geom_histogram(binwidth=1,colour="#000099",fill="#66CC99")+
                         geom_vline(xintercept=allmeans$Cor._Width[c(358)],colour="#CC79A7",size=1)+
                         geom_vline(xintercept=allmeans$Cor._Width[c(359)],colour="#E69F00",size=1)+
                         geom_vline(xintercept=allmeans$Cor._Width[c(360)],colour="#F0E442",size=1)+
                         geom_vline(xintercept=c(mean(F2means$Cor._Width)),linetype="solid",size=1)+
                         xlab("Corolla Width")  )




is.numeric(morphology$Cor..Length)
is.numeric(morphology$Cor..Width)
length(morphology$Cor..Width)
hist(morphology$Cor..Width)








morphology<-read.csv("FloralMorph4-28.csv", fileEncoding="UTF-8-BOM")
morphology<-read.csv("FloralMorph7-6.csv")#, fileEncoding="UTF-8-BOM")
morphology<-read.csv("FloralMorph7-6.csv")#, fileEncoding="UTF-8-BOM")
morphology<-read.csv("C:/Users/joann/Dropbox/Professional/Duke/Rausher_lab/ChapterTwoFstQst/Data/Floral_traits_final_with_Irene.csv")#, fileEncoding="UTF-8-BOM")



head(morphology)
str(morphology)
morphology

morphmeans=aggregate(morphology, by=list(morphology$Individual),mean, na.rm=TRUE)
head(morphmeans)
morphmeans=aggregate(morphology[,2:24], by=list(morphology$Individual),mean, na.rm=TRUE)
#This tells me that the warnings are only coming from the "Date" column

length(morphology$Individual)

warnings()
head(morphmeans)

is.na(morphology$Cor..Width)
length(morphology$Color.pattern)
is.na(morphology$pedicel.flower.only)

individual_means<-write.csv(morphmeans,"IndividualMeans4-28.csv")

individual_means<-write.csv(morphmeans,"IndividualMeans7-6.csv")

individual_means<-write.csv(morphmeans,"IndividualMeans1-5-2018.csv")

allmeans <- read.csv("IndividualMeans5-3PopCodesNectarFirstFlower.csv")

head(allmeans)
attach(allmeans)
cdt<-subset(allmeans, Species=="Cordatotriloba")
head(cdt)
lac<-subset(allmeans, Species=="Lacunosa")


nectar<-read.csv("NectarData_170428.csv")
nectar<-read.csv("C:/Users/joann/Dropbox/Professional/Duke/Rausher_lab/ChapterTwoFstQst/Data/All_nectar_data_1-2-18_no_damage_if_3.csv")
head(nectar)
nectaraverages<-aggregate(nectar, by=list(nectar$Individual), mean, na.rm=T)
head(nectaraverages)
write.csv(nectaraverages,"nectarmeans01-5-18.csv")
warnings()

pollen<-read.csv("Pollen7-6.csv")
pollen<-read.csv("PollenCounts2-27All.csv")
pollenaverages<-aggregate(pollen, by=list(pollen$Individual), mean, na.rm=T)
write.csv(pollenaverages, "pollen_means_7-6.csv")
write.csv(pollenaverages, "pollen_means_2-27.csv")
ID_count<-table(pollen$Individual)
ID_count
ID_freq<-as.data.frame(ID_count)
ID_freq
#pollenaverageswithN<-
hist(pollenaverages$PollenPerOvule)
hist(log(pollenaverages$PollenPerOvule))


###########################