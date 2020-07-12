library(maps)
library(osrm)

# acessa diretorio arquivos testes SP
setwd("/Users/fabiosilva/Documents/MECAI/_revbibmecai/cluster")
dataset <- read.csv("cidades.csv", header=T,sep=";", stringsAsFactors=F)

#conversão de dados no dataset para manter no mesmo formato do shp original
dataset$CD_GEOCODI <- as.character(dataset$CD_GEOCODI)
dataset$cod_rgi <- as.character(dataset$cod_rgi)
dataset$cod_rgint <- as.character(dataset$cod_rgint)
dataset$lat <- gsub(",",".", dataset$lat)
dataset$lon <- gsub(",",".", dataset$lon)
dataset$lat <- as.double(dataset$lat)
dataset$lon <- as.double(dataset$lon)

#ordena da maior para a menor demanda em m3
capm3 <- dataset[order(dataset$CapEstNem3, decreasing=TRUE),]

# mostra os 100 maiores municípios em demanda do estado de SP em m3
demanda100 <- data.frame(capm3$localizacao, capm3$lon, capm3$lat, stringsAsFactors=FALSE)
colnames(demanda100) <- c("localizacao", "lon", "lat")
#calcula a matriz de distancias dirigidas
#informa qual o servidor OSRM
options(osrm.server = "http://192.168.56.101:5000/")

locations = data.frame(comm_id = demanda100[,1], lon = demanda100[,2], lat = demanda100[,3], stringsAsFactors=FALSE)

#calculando as distancias dirigidas
start.time <- Sys.time()
ddm100 <- osrmTable(loc = locations, measure="distance")
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#ajuste da matriz de distancias para substituir valores NA por 9 000 000 (9 mil Km)
#valor de 9000 Km foi escolhido devido a max(dmm) == 7590
dmm <- ddm100$distances[1:5570,1:5570]
dmm[is.na(dmm)] <- 9000000

start.time <- Sys.time()
perfil = kmeans(dmm, centers=50, iter.max=100, algorithm = "Lloyd")
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

library(dplyr)
#trabalhando o cluster1
cluster1=cbind(perfil$cluster[perfil$cluster==1])
cluster1<-cbind(dimnames(cluster1)[[1]])
cluster1<-data.frame(cluster1, stringsAsFactors=FALSE)
colnames(cluster1)<-c("localizacao")
#une lon lat
cluster1lonlat <- left_join(cluster1, capm3[c(9,18,19,20)], by = c("localizacao" = "localizacao"))

#trabalhando o cluster2
cluster2=cbind(perfil$cluster[perfil$cluster==2])
cluster2<-cbind(dimnames(cluster2)[[1]])
cluster2<-data.frame(cluster2, stringsAsFactors=FALSE)
colnames(cluster2)<-c("localizacao")
#une lon lat
cluster2lonlat <- left_join(cluster2, capm3[c(9,18,19,20)], by = c("localizacao" = "localizacao"))
#trabalhando o cluster3
cluster3=cbind(perfil$cluster[perfil$cluster==3])
cluster3<-cbind(dimnames(cluster3)[[1]])
cluster3<-data.frame(cluster3, stringsAsFactors=FALSE)
colnames(cluster3)<-c("localizacao")
#une lon lat
cluster3lonlat <- left_join(cluster3, capm3[c(9,18,19,20)], by = c("localizacao" = "localizacao"))

#trabalhando o cluster4
cluster4=cbind(perfil$cluster[perfil$cluster==4])
cluster4<-cbind(dimnames(cluster4)[[1]])
cluster4<-data.frame(cluster4, stringsAsFactors=FALSE)
colnames(cluster4)<-c("localizacao")
#une lon lat
cluster4lonlat <- left_join(cluster4, capm3[c(9,18,19,20)], by = c("localizacao" = "localizacao"))

#trabalhando o cluster5
cluster5=cbind(perfil$cluster[perfil$cluster==5])
cluster5<-cbind(dimnames(cluster5)[[1]])
cluster5<-data.frame(cluster5, stringsAsFactors=FALSE)
colnames(cluster5)<-c("localizacao")
#une lon lat
cluster5lonlat <- left_join(cluster5, capm3[c(9,18,19,20)], by = c("localizacao" = "localizacao"))

#trabalhando o cluster6
cluster6=cbind(perfil$cluster[perfil$cluster==6])
cluster6<-cbind(dimnames(cluster6)[[1]])
cluster6<-data.frame(cluster6, stringsAsFactors=FALSE)
colnames(cluster6)<-c("localizacao")
#une lon lat
cluster6lonlat <- left_join(cluster6, capm3[c(9,18,19,20)], by = c("localizacao" = "localizacao"))

#do cluster 7 ao 12
#trabalhando o cluster7
cluster7=cbind(perfil$cluster[perfil$cluster==7])
cluster7<-cbind(dimnames(cluster7)[[1]])
cluster7<-data.frame(cluster7, stringsAsFactors=FALSE)
colnames(cluster7)<-c("localizacao")
#une lon lat
cluster7lonlat <- left_join(cluster7, capm3[c(9,18,19,20)], by = c("localizacao" = "localizacao"))

#trabalhando o cluster8
cluster8=cbind(perfil$cluster[perfil$cluster==8])
cluster8<-cbind(dimnames(cluster8)[[1]])
cluster8<-data.frame(cluster8, stringsAsFactors=FALSE)
colnames(cluster8)<-c("localizacao")
#une lon lat
cluster8lonlat <- left_join(cluster8, capm3[c(9,18,19,20)], by = c("localizacao" = "localizacao"))
#trabalhando o cluster9
cluster9=cbind(perfil$cluster[perfil$cluster==9])
cluster9<-cbind(dimnames(cluster9)[[1]])
cluster9<-data.frame(cluster9, stringsAsFactors=FALSE)
colnames(cluster9)<-c("localizacao")
#une lon lat
cluster9lonlat <- left_join(cluster9, capm3[c(9,18,19,20)], by = c("localizacao" = "localizacao"))

#trabalhando o cluster10
cluster10=cbind(perfil$cluster[perfil$cluster==10])
cluster10<-cbind(dimnames(cluster10)[[1]])
cluster10<-data.frame(cluster10, stringsAsFactors=FALSE)
colnames(cluster10)<-c("localizacao")
#une lon lat
cluster10lonlat <- left_join(cluster10, capm3[c(9,18,19,20)], by = c("localizacao" = "localizacao"))

#trabalhando o cluster11
cluster11=cbind(perfil$cluster[perfil$cluster==11])
cluster11<-cbind(dimnames(cluster11)[[1]])
cluster11<-data.frame(cluster11, stringsAsFactors=FALSE)
colnames(cluster11)<-c("localizacao")
#une lon lat
cluster11lonlat <- left_join(cluster11, capm3[c(9,18,19,20)], by = c("localizacao" = "localizacao"))

#trabalhando o cluster12
cluster12=cbind(perfil$cluster[perfil$cluster==12])
cluster12<-cbind(dimnames(cluster12)[[1]])
cluster12<-data.frame(cluster12, stringsAsFactors=FALSE)
colnames(cluster12)<-c("localizacao")
#une lon lat
cluster12lonlat <- left_join(cluster12, capm3[c(9,18,19,20)], by = c("localizacao" = "localizacao"))

#cluster 13 ao 18
#trabalhando o cluster13
cluster13=cbind(perfil$cluster[perfil$cluster==13])
cluster13<-cbind(dimnames(cluster13)[[1]])
cluster13<-data.frame(cluster13, stringsAsFactors=FALSE)
colnames(cluster13)<-c("localizacao")
#une lon lat
cluster13lonlat <- left_join(cluster13, capm3[c(9,18,19,20)], by = c("localizacao" = "localizacao"))

#trabalhando o cluster14
cluster14=cbind(perfil$cluster[perfil$cluster==14])
cluster14 <-cbind(dimnames(cluster14)[[1]])
cluster14 <-data.frame(cluster14, stringsAsFactors=FALSE)
colnames(cluster14)<-c("localizacao")
#une lon lat
cluster14lonlat <- left_join(cluster14, capm3[c(9,18,19,20)], by = c("localizacao" = "localizacao"))
#trabalhando o cluster15
cluster15=cbind(perfil$cluster[perfil$cluster==15])
cluster15 <- cbind(dimnames(cluster15)[[1]])
cluster15 <- data.frame(cluster15, stringsAsFactors=FALSE)
colnames(cluster15)<-c("localizacao")
#une lon lat
cluster15lonlat <- left_join(cluster15, capm3[c(9,18,19,20)], by = c("localizacao" = "localizacao"))

#trabalhando o cluster16
cluster16=cbind(perfil$cluster[perfil$cluster==16])
cluster16 <- cbind(dimnames(cluster16)[[1]])
cluster16 <- data.frame(cluster16, stringsAsFactors=FALSE)
colnames(cluster16)<-c("localizacao")
#une lon lat
cluster16lonlat <- left_join(cluster16, capm3[c(9,18,19,20)], by = c("localizacao" = "localizacao"))

#trabalhando o cluster17
cluster17=cbind(perfil$cluster[perfil$cluster==17])
cluster17 <- cbind(dimnames(cluster17)[[1]])
cluster17 <- data.frame(cluster17, stringsAsFactors=FALSE)
colnames(cluster17)<-c("localizacao")
#une lon lat
cluster17lonlat <- left_join(cluster17, capm3[c(9,18,19,20)], by = c("localizacao" = "localizacao"))

#trabalhando o cluster18
cluster18=cbind(perfil$cluster[perfil$cluster==18])
cluster18 <- cbind(dimnames(cluster18)[[1]])
cluster18 <- data.frame(cluster18, stringsAsFactors=FALSE)
colnames(cluster18)<-c("localizacao")
#une lon lat
cluster18lonlat <- left_join(cluster18, capm3[c(9,18,19,20)], by = c("localizacao" = "localizacao"))

#do cluster 19 ao 24
#trabalhando o cluster19
cluster19=cbind(perfil$cluster[perfil$cluster==19])
cluster19 <- cbind(dimnames(cluster19)[[1]])
cluster19 <- data.frame(cluster19, stringsAsFactors=FALSE)
colnames(cluster19)<-c("localizacao")
#une lon lat
cluster19lonlat <- left_join(cluster19, capm3[c(9,18,19,20)], by = c("localizacao" = "localizacao"))

#trabalhando o cluster20
cluster20=cbind(perfil$cluster[perfil$cluster==20])
cluster20 <-cbind(dimnames(cluster20)[[1]])
cluster20 <-data.frame(cluster20, stringsAsFactors=FALSE)
colnames(cluster20)<-c("localizacao")
#une lon lat
cluster20lonlat <- left_join(cluster20, capm3[c(9,18,19,20)], by = c("localizacao" = "localizacao"))
#trabalhando o cluster21
cluster21=cbind(perfil$cluster[perfil$cluster==21])
cluster21<-cbind(dimnames(cluster21)[[1]])
cluster21<-data.frame(cluster21, stringsAsFactors=FALSE)
colnames(cluster21)<-c("localizacao")
#une lon lat
cluster21lonlat <- left_join(cluster21, capm3[c(9,18,19,20)], by = c("localizacao" = "localizacao"))

#trabalhando o cluster22
cluster22=cbind(perfil$cluster[perfil$cluster==22])
cluster22<-cbind(dimnames(cluster22)[[1]])
cluster22<-data.frame(cluster22, stringsAsFactors=FALSE)
colnames(cluster22)<-c("localizacao")
#une lon lat
cluster22lonlat <- left_join(cluster22, capm3[c(9,18,19,20)], by = c("localizacao" = "localizacao"))

#trabalhando o cluster23
cluster23=cbind(perfil$cluster[perfil$cluster==23])
cluster23<-cbind(dimnames(cluster23)[[1]])
cluster23<-data.frame(cluster23, stringsAsFactors=FALSE)
colnames(cluster23)<-c("localizacao")
#une lon lat
cluster23lonlat <- left_join(cluster23, capm3[c(9,18,19,20)], by = c("localizacao" = "localizacao"))

#trabalhando o cluster24
cluster24=cbind(perfil$cluster[perfil$cluster==24])
cluster24<-cbind(dimnames(cluster24)[[1]])
cluster24<-data.frame(cluster24, stringsAsFactors=FALSE)
colnames(cluster24)<-c("localizacao")
#une lon lat
cluster24lonlat <- left_join(cluster24, capm3[c(9,18,19,20)], by = c("localizacao" = "localizacao"))
#-------------fim 19 ao 24

#---------inicio 25 ao 30
#trabalhando o cluster25
cluster25=cbind(perfil$cluster[perfil$cluster==25])
cluster25 <- cbind(dimnames(cluster25)[[1]])
cluster25 <- data.frame(cluster25, stringsAsFactors=FALSE)
colnames(cluster25)<-c("localizacao")
#une lon lat
cluster25lonlat <- left_join(cluster25, capm3[c(9,18,19,20)], by = c("localizacao" = "localizacao"))

#trabalhando o cluster26
cluster26=cbind(perfil$cluster[perfil$cluster==26])
cluster26 <- cbind(dimnames(cluster26)[[1]])
cluster26 <- data.frame(cluster26, stringsAsFactors=FALSE)
colnames(cluster26)<-c("localizacao")
#une lon lat
cluster26lonlat <- left_join(cluster26, capm3[c(9,18,19,20)], by = c("localizacao" = "localizacao"))

#trabalhando o cluster27
cluster27=cbind(perfil$cluster[perfil$cluster==27])
cluster27 <-cbind(dimnames(cluster27)[[1]])
cluster27 <-data.frame(cluster27, stringsAsFactors=FALSE)
colnames(cluster27)<-c("localizacao")
#une lon lat
cluster27lonlat <- left_join(cluster27, capm3[c(9,18,19,20)], by = c("localizacao" = "localizacao"))
#trabalhando o cluster28
cluster28=cbind(perfil$cluster[perfil$cluster==28])
cluster28<-cbind(dimnames(cluster28)[[1]])
cluster28<-data.frame(cluster28, stringsAsFactors=FALSE)
colnames(cluster28)<-c("localizacao")
#une lon lat
cluster28lonlat <- left_join(cluster28, capm3[c(9,18,19,20)], by = c("localizacao" = "localizacao"))

#trabalhando o cluster29
cluster29=cbind(perfil$cluster[perfil$cluster==29])
cluster29<-cbind(dimnames(cluster29)[[1]])
cluster29<-data.frame(cluster29, stringsAsFactors=FALSE)
colnames(cluster29)<-c("localizacao")
#une lon lat
cluster29lonlat <- left_join(cluster29, capm3[c(9,18,19,20)], by = c("localizacao" = "localizacao"))

#trabalhando o cluster30
cluster30=cbind(perfil$cluster[perfil$cluster==30])
cluster30<-cbind(dimnames(cluster30)[[1]])
cluster30<-data.frame(cluster30, stringsAsFactors=FALSE)
colnames(cluster30)<-c("localizacao")
#une lon lat
cluster30lonlat <- left_join(cluster30, capm3[c(9,18,19,20)], by = c("localizacao" = "localizacao"))
#---------fim 25 ao 30
#---------inicio 31 ao 35
#trabalhando o cluster31
cluster31=cbind(perfil$cluster[perfil$cluster==31])
cluster31 <- cbind(dimnames(cluster31)[[1]])
cluster31 <- data.frame(cluster31, stringsAsFactors=FALSE)
colnames(cluster31)<-c("localizacao")
#une lon lat
cluster31lonlat <- left_join(cluster31, capm3[c(9,18,19,20)], by = c("localizacao" = "localizacao"))

#trabalhando o cluster32
cluster32=cbind(perfil$cluster[perfil$cluster==32])
cluster32 <- cbind(dimnames(cluster32)[[1]])
cluster32 <- data.frame(cluster32, stringsAsFactors=FALSE)
colnames(cluster32)<-c("localizacao")
#une lon lat
cluster32lonlat <- left_join(cluster32, capm3[c(9,18,19,20)], by = c("localizacao" = "localizacao"))

#trabalhando o cluster33
cluster33=cbind(perfil$cluster[perfil$cluster==33])
cluster33 <-cbind(dimnames(cluster33)[[1]])
cluster33 <-data.frame(cluster33, stringsAsFactors=FALSE)
colnames(cluster33)<-c("localizacao")
#une lon lat
cluster33lonlat <- left_join(cluster33, capm3[c(9,18,19,20)], by = c("localizacao" = "localizacao"))

#trabalhando o cluster34
cluster34=cbind(perfil$cluster[perfil$cluster==34])
cluster34<-cbind(dimnames(cluster34)[[1]])
cluster34<-data.frame(cluster34, stringsAsFactors=FALSE)
colnames(cluster34)<-c("localizacao")
#une lon lat
cluster34lonlat <- left_join(cluster34, capm3[c(9,18,19,20)], by = c("localizacao" = "localizacao"))

#trabalhando o cluster35
cluster35=cbind(perfil$cluster[perfil$cluster==35])
cluster35<-cbind(dimnames(cluster35)[[1]])
cluster35<-data.frame(cluster35, stringsAsFactors=FALSE)
colnames(cluster35)<-c("localizacao")
#une lon lat
cluster35lonlat <- left_join(cluster35, capm3[c(9,18,19,20)], by = c("localizacao" = "localizacao"))

#trabalhando o cluster36
cluster36=cbind(perfil$cluster[perfil$cluster==36])
cluster36<-cbind(dimnames(cluster36)[[1]])
cluster36<-data.frame(cluster36, stringsAsFactors=FALSE)
colnames(cluster36)<-c("localizacao")
#une lon lat
cluster36lonlat <- left_join(cluster36, capm3[c(9,18,19,20)], by = c("localizacao" = "localizacao"))
#---------fim 31 ao 36
#---------inicio 37 ao 42
#trabalhando o cluster37
cluster37=cbind(perfil$cluster[perfil$cluster==37])
cluster37<-cbind(dimnames(cluster37)[[1]])
cluster37<-data.frame(cluster37, stringsAsFactors=FALSE)
colnames(cluster37)<-c("localizacao")
#une lon lat
cluster37lonlat <- left_join(cluster37, capm3[c(9,18,19,20)], by = c("localizacao" = "localizacao"))

#trabalhando o cluster38
cluster38=cbind(perfil$cluster[perfil$cluster==38])
cluster38<-cbind(dimnames(cluster38)[[1]])
cluster38<-data.frame(cluster38, stringsAsFactors=FALSE)
colnames(cluster38)<-c("localizacao")
#une lon lat
cluster38lonlat <- left_join(cluster38, capm3[c(9,18,19,20)], by = c("localizacao" = "localizacao"))

#trabalhando o cluster39
cluster39=cbind(perfil$cluster[perfil$cluster==39])
cluster39<-cbind(dimnames(cluster39)[[1]])
cluster39<-data.frame(cluster39, stringsAsFactors=FALSE)
colnames(cluster39)<-c("localizacao")
#une lon lat
cluster39lonlat <- left_join(cluster39, capm3[c(9,18,19,20)], by = c("localizacao" = "localizacao"))

#trabalhando o cluster40
cluster40=cbind(perfil$cluster[perfil$cluster==40])
cluster40<-cbind(dimnames(cluster40)[[1]])
cluster40<-data.frame(cluster40, stringsAsFactors=FALSE)
colnames(cluster40)<-c("localizacao")
#une lon lat
cluster40lonlat <- left_join(cluster40, capm3[c(9,18,19,20)], by = c("localizacao" = "localizacao"))

#trabalhando o cluster41
cluster41=cbind(perfil$cluster[perfil$cluster==41])
cluster41<-cbind(dimnames(cluster41)[[1]])
cluster41<-data.frame(cluster41, stringsAsFactors=FALSE)
colnames(cluster41)<-c("localizacao")
#une lon lat
cluster41lonlat <- left_join(cluster41, capm3[c(9,18,19,20)], by = c("localizacao" = "localizacao"))

#trabalhando o cluster42
cluster42=cbind(perfil$cluster[perfil$cluster==42])
cluster42<-cbind(dimnames(cluster42)[[1]])
cluster42<-data.frame(cluster42, stringsAsFactors=FALSE)
colnames(cluster42)<-c("localizacao")
#une lon lat
cluster42lonlat <- left_join(cluster42, capm3[c(9,18,19,20)], by = c("localizacao" = "localizacao"))
#---------fim 37 ao 42

#---------inicio 43 ao 50
#trabalhando o cluster43
cluster43=cbind(perfil$cluster[perfil$cluster==43])
cluster43<-cbind(dimnames(cluster43)[[1]])
cluster43<-data.frame(cluster43, stringsAsFactors=FALSE)
colnames(cluster43)<-c("localizacao")
#une lon lat
cluster43lonlat <- left_join(cluster43, capm3[c(9,18,19,20)], by = c("localizacao" = "localizacao"))

#trabalhando o cluster44
cluster44=cbind(perfil$cluster[perfil$cluster==44])
cluster44<-cbind(dimnames(cluster44)[[1]])
cluster44<-data.frame(cluster44, stringsAsFactors=FALSE)
colnames(cluster44)<-c("localizacao")
#une lon lat
cluster44lonlat <- left_join(cluster44, capm3[c(9,18,19,20)], by = c("localizacao" = "localizacao"))

#trabalhando o cluster45
cluster45=cbind(perfil$cluster[perfil$cluster==45])
cluster45<-cbind(dimnames(cluster45)[[1]])
cluster45<-data.frame(cluster45, stringsAsFactors=FALSE)
colnames(cluster45)<-c("localizacao")
#une lon lat
cluster45lonlat <- left_join(cluster45, capm3[c(9,18,19,20)], by = c("localizacao" = "localizacao"))

#trabalhando o cluster46
cluster46=cbind(perfil$cluster[perfil$cluster==46])
cluster46<-cbind(dimnames(cluster46)[[1]])
cluster46<-data.frame(cluster46, stringsAsFactors=FALSE)
colnames(cluster46)<-c("localizacao")
#une lon lat
cluster46lonlat <- left_join(cluster46, capm3[c(9,18,19,20)], by = c("localizacao" = "localizacao"))

#trabalhando o cluster47
cluster47=cbind(perfil$cluster[perfil$cluster==47])
cluster47<-cbind(dimnames(cluster47)[[1]])
cluster47<-data.frame(cluster47, stringsAsFactors=FALSE)
colnames(cluster47)<-c("localizacao")
#une lon lat
cluster47lonlat <- left_join(cluster47, capm3[c(9,18,19,20)], by = c("localizacao" = "localizacao"))

#trabalhando o cluster48
cluster48=cbind(perfil$cluster[perfil$cluster==48])
cluster48<-cbind(dimnames(cluster48)[[1]])
cluster48<-data.frame(cluster48, stringsAsFactors=FALSE)
colnames(cluster48)<-c("localizacao")
#une lon lat
cluster48lonlat <- left_join(cluster48, capm3[c(9,18,19,20)], by = c("localizacao" = "localizacao"))

#trabalhando o cluster49
cluster49=cbind(perfil$cluster[perfil$cluster==49])
cluster49<-cbind(dimnames(cluster49)[[1]])
cluster49<-data.frame(cluster49, stringsAsFactors=FALSE)
colnames(cluster49)<-c("localizacao")
#une lon lat
cluster49lonlat <- left_join(cluster49, capm3[c(9,18,19,20)], by = c("localizacao" = "localizacao"))

#trabalhando o cluster50
cluster50=cbind(perfil$cluster[perfil$cluster==50])
cluster50<-cbind(dimnames(cluster50)[[1]])
cluster50<-data.frame(cluster50, stringsAsFactors=FALSE)
colnames(cluster50)<-c("localizacao")
#une lon lat
cluster50lonlat <- left_join(cluster50, capm3[c(9,18,19,20)], by = c("localizacao" = "localizacao"))

#---------fim 43 ao 50

#calculando o ponto médio
source("geographic_midpoint.R")
mdpcluster1 <- geographic_midpoint(cluster1lonlat$lon, cluster1lonlat$lat, cluster1lonlat$CapEstNem3)
mdpcluster2 <- geographic_midpoint(cluster2lonlat$lon, cluster2lonlat$lat, cluster2lonlat$CapEstNem3)
mdpcluster3 <- geographic_midpoint(cluster3lonlat$lon, cluster3lonlat$lat, cluster3lonlat$CapEstNem3)
mdpcluster4 <- geographic_midpoint(cluster4lonlat$lon, cluster4lonlat$lat, cluster4lonlat$CapEstNem3)
mdpcluster5 <- geographic_midpoint(cluster5lonlat$lon, cluster5lonlat$lat, cluster5lonlat$CapEstNem3)
mdpcluster6 <- geographic_midpoint(cluster6lonlat$lon, cluster6lonlat$lat, cluster6lonlat$CapEstNem3)

#do cluster 7 ao 12
mdpcluster7 <- geographic_midpoint(cluster7lonlat$lon, cluster7lonlat$lat, cluster7lonlat$CapEstNem3)
mdpcluster8 <- geographic_midpoint(cluster8lonlat$lon, cluster8lonlat$lat, cluster8lonlat$CapEstNem3)
mdpcluster9 <- geographic_midpoint(cluster9lonlat$lon, cluster9lonlat$lat, cluster9lonlat$CapEstNem3)
mdpcluster10 <- geographic_midpoint(cluster10lonlat$lon, cluster10lonlat$lat, cluster10lonlat$CapEstNem3)
mdpcluster11 <- geographic_midpoint(cluster11lonlat$lon, cluster11lonlat$lat, cluster11lonlat$CapEstNem3)
mdpcluster12 <- geographic_midpoint(cluster12lonlat$lon, cluster12lonlat$lat, cluster12lonlat$CapEstNem3)

#do cluster 13 ao 18
mdpcluster13 <- geographic_midpoint(cluster13lonlat$lon, cluster13lonlat$lat, cluster13lonlat$CapEstNem3)
mdpcluster14 <- geographic_midpoint(cluster14lonlat$lon, cluster14lonlat$lat, cluster14lonlat$CapEstNem3)
mdpcluster15 <- geographic_midpoint(cluster15lonlat$lon, cluster15lonlat$lat, cluster15lonlat$CapEstNem3)
mdpcluster16 <- geographic_midpoint(cluster16lonlat$lon, cluster16lonlat$lat, cluster16lonlat$CapEstNem3)
mdpcluster17 <- geographic_midpoint(cluster17lonlat$lon, cluster17lonlat$lat, cluster17lonlat$CapEstNem3)
mdpcluster18 <- geographic_midpoint(cluster18lonlat$lon, cluster18lonlat$lat, cluster18lonlat$CapEstNem3)

#do cluster 19 ao 24 
mdpcluster19 <- geographic_midpoint(cluster19lonlat$lon, cluster19lonlat$lat, cluster19lonlat$CapEstNem3)
mdpcluster20 <- geographic_midpoint(cluster20lonlat$lon, cluster20lonlat$lat, cluster20lonlat$CapEstNem3)
mdpcluster21 <- geographic_midpoint(cluster21lonlat$lon, cluster21lonlat$lat, cluster21lonlat$CapEstNem3)
mdpcluster22 <- geographic_midpoint(cluster22lonlat$lon, cluster22lonlat$lat, cluster22lonlat$CapEstNem3)
mdpcluster23 <- geographic_midpoint(cluster23lonlat$lon, cluster23lonlat$lat, cluster23lonlat$CapEstNem3)
mdpcluster24 <- geographic_midpoint(cluster24lonlat$lon, cluster24lonlat$lat, cluster24lonlat$CapEstNem3)

#do cluster 25 ao 30 
mdpcluster25 <- geographic_midpoint(cluster25lonlat$lon, cluster25lonlat$lat, cluster25lonlat$CapEstNem3)
mdpcluster26 <- geographic_midpoint(cluster26lonlat$lon, cluster26lonlat$lat, cluster26lonlat$CapEstNem3)
mdpcluster27 <- geographic_midpoint(cluster27lonlat$lon, cluster27lonlat$lat, cluster27lonlat$CapEstNem3)
mdpcluster28 <- geographic_midpoint(cluster28lonlat$lon, cluster28lonlat$lat, cluster28lonlat$CapEstNem3)
mdpcluster29 <- geographic_midpoint(cluster29lonlat$lon, cluster29lonlat$lat, cluster29lonlat$CapEstNem3)
mdpcluster30 <- geographic_midpoint(cluster30lonlat$lon, cluster30lonlat$lat, cluster30lonlat$CapEstNem3)

#do cluster 31 ao 36 
mdpcluster31 <- geographic_midpoint(cluster31lonlat$lon, cluster31lonlat$lat, cluster31lonlat$CapEstNem3)
mdpcluster32 <- geographic_midpoint(cluster32lonlat$lon, cluster32lonlat$lat, cluster32lonlat$CapEstNem3)
mdpcluster33 <- geographic_midpoint(cluster33lonlat$lon, cluster33lonlat$lat, cluster33lonlat$CapEstNem3)
mdpcluster34 <- geographic_midpoint(cluster34lonlat$lon, cluster34lonlat$lat, cluster34lonlat$CapEstNem3)
mdpcluster35 <- geographic_midpoint(cluster35lonlat$lon, cluster35lonlat$lat, cluster35lonlat$CapEstNem3)
mdpcluster36 <- geographic_midpoint(cluster36lonlat$lon, cluster36lonlat$lat, cluster36lonlat$CapEstNem3)

#do cluster 37 ao 42 
mdpcluster37 <- geographic_midpoint(cluster37lonlat$lon, cluster37lonlat$lat, cluster37lonlat$CapEstNem3)
mdpcluster38 <- geographic_midpoint(cluster38lonlat$lon, cluster38lonlat$lat, cluster38lonlat$CapEstNem3)
mdpcluster39 <- geographic_midpoint(cluster39lonlat$lon, cluster39lonlat$lat, cluster39lonlat$CapEstNem3)
mdpcluster40 <- geographic_midpoint(cluster40lonlat$lon, cluster40lonlat$lat, cluster40lonlat$CapEstNem3)
mdpcluster41 <- geographic_midpoint(cluster41lonlat$lon, cluster41lonlat$lat, cluster41lonlat$CapEstNem3)
mdpcluster42 <- geographic_midpoint(cluster42lonlat$lon, cluster42lonlat$lat, cluster42lonlat$CapEstNem3)

#do cluster 43 ao 50 
mdpcluster43 <- geographic_midpoint(cluster43lonlat$lon, cluster43lonlat$lat, cluster43lonlat$CapEstNem3)
mdpcluster44 <- geographic_midpoint(cluster44lonlat$lon, cluster44lonlat$lat, cluster44lonlat$CapEstNem3)
mdpcluster45 <- geographic_midpoint(cluster45lonlat$lon, cluster45lonlat$lat, cluster45lonlat$CapEstNem3)
mdpcluster46 <- geographic_midpoint(cluster46lonlat$lon, cluster46lonlat$lat, cluster46lonlat$CapEstNem3)
mdpcluster47 <- geographic_midpoint(cluster47lonlat$lon, cluster47lonlat$lat, cluster47lonlat$CapEstNem3)
mdpcluster48 <- geographic_midpoint(cluster48lonlat$lon, cluster48lonlat$lat, cluster48lonlat$CapEstNem3)
mdpcluster49 <- geographic_midpoint(cluster49lonlat$lon, cluster49lonlat$lat, cluster49lonlat$CapEstNem3)
mdpcluster50 <- geographic_midpoint(cluster50lonlat$lon, cluster50lonlat$lat, cluster50lonlat$CapEstNem3)


#desenha os agrupamentos
png(file="BR50Agrupamentos_posdefesa01.png")
map("world","Brazil")

#do cluster 1 ao 6
text(cluster1lonlat$lon, cluster1lonlat$lat, pch=19,cex=0.5, col="#cea632", labels="1")
text(cluster2lonlat$lon, cluster2lonlat$lat, pch=19,cex=0.5, col="#6677db", labels="2")
text(cluster3lonlat$lon, cluster3lonlat$lat, pch=19,cex=0.5, col="#acb839", labels="3")
text(cluster4lonlat$lon, cluster4lonlat$lat, pch=19,cex=0.5, col="#553687", labels="4")
text(cluster5lonlat$lon, cluster5lonlat$lat, pch=19,cex=0.5, col="#5cb455", labels="5")
text(cluster6lonlat$lon, cluster6lonlat$lat, pch=19,cex=0.5, col="#cc78d5", labels="6")
#do cluster 7 ao 12
text(cluster7lonlat$lon, cluster7lonlat$lat, pch=19,cex=0.5, col="#86af42", labels="7")
text(cluster8lonlat$lon, cluster8lonlat$lat, pch=19,cex=0.5, col="#b855a2", labels="8")
text(cluster9lonlat$lon, cluster9lonlat$lat, pch=19,cex=0.5, col="#64c77a", labels="9")
text(cluster10lonlat$lon, cluster10lonlat$lat, pch=19,cex=0.5, col="#c9407d", labels="10")
text(cluster11lonlat$lon, cluster11lonlat$lat, pch=19,cex=0.5, col="#48bf8f", labels="11")
text(cluster12lonlat$lon, cluster12lonlat$lat, pch=19,cex=0.5, col="#d54b5e", labels="12")
#pontos 13 ao 18
text(cluster13lonlat$lon, cluster13lonlat$lat, pch=19,cex=0.5, col="#38dbda", labels="13")
text(cluster14lonlat$lon, cluster14lonlat$lat, pch=19,cex=0.5, col="#da504c", labels="14")
text(cluster15lonlat$lon, cluster15lonlat$lat, pch=19,cex=0.5, col="#397836", labels="15")
text(cluster16lonlat$lon, cluster16lonlat$lat, pch=19,cex=0.5, col="#7e2458", labels="16")
text(cluster17lonlat$lon, cluster17lonlat$lat, pch=19,cex=0.5, col="#beb35a", labels="17")
text(cluster18lonlat$lon, cluster18lonlat$lat, pch=19,cex=0.5, col="#8687d0", labels="18")
#do cluster 19 ao 24
text(cluster19lonlat$lon, cluster19lonlat$lat, pch=19,cex=0.5, col="#d38725", labels="19")
text(cluster20lonlat$lon, cluster20lonlat$lat, pch=19,cex=0.5, col="#d8739f", labels="20")
text(cluster21lonlat$lon, cluster21lonlat$lat, pch=19,cex=0.5, col="#6f8835", labels="21")
text(cluster22lonlat$lon, cluster22lonlat$lat, pch=19,cex=0.5, col="#962945", labels="22")
text(cluster23lonlat$lon, cluster23lonlat$lat, pch=19,cex=0.5, col="#d58e48", labels="23")
text(cluster24lonlat$lon, cluster24lonlat$lat, pch=19,cex=0.5, col="#db657c", labels="24")
#do cluster 25 ao 30
text(cluster25lonlat$lon, cluster25lonlat$lat, pch=19,cex=0.5, col="#a27836", labels="25")
text(cluster26lonlat$lon, cluster26lonlat$lat, pch=19,cex=0.5, col="#952d33", labels="26")
text(cluster27lonlat$lon, cluster27lonlat$lat, pch=19,cex=0.5, col="#e88373", labels="27")
text(cluster28lonlat$lon, cluster28lonlat$lat, pch=19,cex=0.5, col="#7f2b10", labels="28")
text(cluster29lonlat$lon, cluster29lonlat$lat, pch=19,cex=0.5, col="#c25a2e", labels="29")
text(cluster30lonlat$lon, cluster30lonlat$lat, pch=19,cex=0.5, col="#b05740", labels="30")

#do cluster 31 ao 36
text(cluster31lonlat$lon, cluster31lonlat$lat, pch=19,cex=0.5, col="#edf8fb", labels="31")
text(cluster32lonlat$lon, cluster32lonlat$lat, pch=19,cex=0.5, col="#b2e2e2", labels="32")
text(cluster33lonlat$lon, cluster33lonlat$lat, pch=19,cex=0.5, col="#66c2a4", labels="33")
text(cluster34lonlat$lon, cluster34lonlat$lat, pch=19,cex=0.5, col="#238b45", labels="34")
text(cluster35lonlat$lon, cluster35lonlat$lat, pch=19,cex=0.5, col="#edf8fb", labels="35")
text(cluster36lonlat$lon, cluster36lonlat$lat, pch=19,cex=0.5, col="#b3cde3", labels="36")

#do cluster 37 ao 42
text(cluster37lonlat$lon, cluster37lonlat$lat, pch=19,cex=0.5, col="#8c96c6", labels="37")
text(cluster38lonlat$lon, cluster38lonlat$lat, pch=19,cex=0.5, col="#88419d", labels="38")
text(cluster38lonlat$lon, cluster38lonlat$lat, pch=19,cex=0.5, col="#fef0d9", labels="39")
text(cluster40lonlat$lon, cluster40lonlat$lat, pch=19,cex=0.5, col="#fdcc8a", labels="40")
text(cluster41lonlat$lon, cluster41lonlat$lat, pch=19,cex=0.5, col="#fc8d59", labels="41")
text(cluster42lonlat$lon, cluster42lonlat$lat, pch=19,cex=0.5, col="#d7301f", labels="42")

#do cluster 43 ao 50
text(cluster43lonlat$lon, cluster43lonlat$lat, pch=19,cex=0.5, col="#f1eef6", labels="43")
text(cluster44lonlat$lon, cluster44lonlat$lat, pch=19,cex=0.5, col="#bdc9e1", labels="44")
text(cluster45lonlat$lon, cluster45lonlat$lat, pch=19,cex=0.5, col="#74a9cf", labels="45")
text(cluster46lonlat$lon, cluster46lonlat$lat, pch=19,cex=0.5, col="#0570b0", labels="46")
text(cluster47lonlat$lon, cluster47lonlat$lat, pch=19,cex=0.5, col="#ffffd4", labels="47")
text(cluster48lonlat$lon, cluster48lonlat$lat, pch=19,cex=0.5, col="#fed98e", labels="48")
text(cluster49lonlat$lon, cluster49lonlat$lat, pch=19,cex=0.5, col="#fe9929", labels="49")
text(cluster50lonlat$lon, cluster50lonlat$lat, pch=19,cex=0.5, col="#cc4c02", labels="50")


#desenha CoG e numera de acordo com cada agrupamento
points(mdpcluster1$lon,mdpcluster1$lat, pch=8,cex=1,col="#404040")
text(mdpcluster1$lon,mdpcluster1$lat, pch=8,cex=1,col="#404040", labels="1" ,pos=4)

points(mdpcluster2$lon,mdpcluster2$lat, pch=8,cex=1,col="#404040")
text(mdpcluster2$lon,mdpcluster2$lat, pch=8,cex=1,col="#404040", labels="2" ,pos=4)

points(mdpcluster3$lon,mdpcluster3$lat, pch=8,cex=1,col="#404040")
text(mdpcluster3$lon,mdpcluster3$lat, pch=8,cex=1,col="#404040", labels="3" ,pos=4)

points(mdpcluster4$lon,mdpcluster4$lat, pch=8,cex=1,col="#404040")
text(mdpcluster4$lon,mdpcluster4$lat, pch=8,cex=1,col="#404040", labels="4" ,pos=4)

points(mdpcluster5$lon,mdpcluster5$lat, pch=8,cex=1,col="#404040")
text(mdpcluster5$lon,mdpcluster5$lat, pch=8,cex=1,col="#404040", labels="5" ,pos=4)

points(mdpcluster6$lon,mdpcluster6$lat, pch=8,cex=1,col="#404040")
text(mdpcluster6$lon,mdpcluster6$lat, pch=8,cex=1,col="#404040", labels="6" ,pos=4)

points(mdpcluster7$lon,mdpcluster7$lat, pch=8,cex=1,col="#404040")
text(mdpcluster7$lon,mdpcluster7$lat, pch=8,cex=1,col="#404040", labels="7" ,pos=4)

points(mdpcluster8$lon,mdpcluster8$lat, pch=8,cex=1,col="#404040")
text(mdpcluster8$lon,mdpcluster8$lat, pch=8,cex=1,col="#404040", labels="8", pos=4)

points(mdpcluster9$lon,mdpcluster9$lat, pch=8,cex=1,col="#404040")
text(mdpcluster9$lon,mdpcluster9$lat, pch=8,cex=1,col="#404040", labels="9", pos=4)

points(mdpcluster10$lon,mdpcluster10$lat, pch=8,cex=1,col="#404040")
text(mdpcluster10$lon,mdpcluster10$lat, pch=8,cex=1,col="#404040", labels="10", pos=4)

points(mdpcluster11$lon,mdpcluster11$lat, pch=8,cex=1,col="#404040")
text(mdpcluster11$lon,mdpcluster11$lat, pch=8,cex=1,col="#404040", labels="11", pos=4)

points(mdpcluster12$lon,mdpcluster12$lat, pch=8,cex=1,col="#404040")
text(mdpcluster12$lon,mdpcluster12$lat, pch=8,cex=1,col="#404040", labels="12", pos=4)

points(mdpcluster13$lon,mdpcluster13$lat, pch=8,cex=1,col="#404040")
text(mdpcluster13$lon,mdpcluster13$lat, pch=8,cex=1,col="#404040", labels="13", pos=4)

points(mdpcluster14$lon,mdpcluster14$lat, pch=8,cex=1,col="#404040")
text(mdpcluster14$lon,mdpcluster14$lat, pch=8,cex=1,col="#404040", labels="14", pos=4)

points(mdpcluster15$lon,mdpcluster15$lat, pch=8,cex=1,col="#404040")
text(mdpcluster15$lon,mdpcluster15$lat, pch=8,cex=1,col="#404040", labels="15", pos=4)

points(mdpcluster16$lon,mdpcluster16$lat, pch=8,cex=1,col="#404040")
text(mdpcluster16$lon,mdpcluster16$lat, pch=8,cex=1,col="#404040", labels="16", pos=4)

points(mdpcluster17$lon,mdpcluster17$lat, pch=8,cex=1,col="#404040")
text(mdpcluster17$lon,mdpcluster17$lat, pch=8,cex=1,col="#404040", labels="17", pos=4)

points(mdpcluster18$lon,mdpcluster18$lat, pch=8,cex=1,col="#404040")
text(mdpcluster18$lon,mdpcluster18$lat, pch=8,cex=1,col="#404040", labels="18", pos=4)

points(mdpcluster19$lon,mdpcluster19$lat, pch=8,cex=1,col="#404040")
text(mdpcluster19$lon,mdpcluster19$lat, pch=8,cex=1,col="#404040",labels="19", pos=4)

points(mdpcluster20$lon,mdpcluster20$lat, pch=8,cex=1,col="#404040")
text(mdpcluster20$lon,mdpcluster20$lat, pch=8,cex=1,col="#404040", labels="20", pos=4)

points(mdpcluster21$lon,mdpcluster21$lat, pch=8,cex=1,col="#404040")
text(mdpcluster21$lon,mdpcluster21$lat, pch=8,cex=1,col="#404040", labels="21", pos=4)

points(mdpcluster22$lon,mdpcluster22$lat, pch=8,cex=1,col="#404040")
text(mdpcluster22$lon,mdpcluster22$lat, pch=8,cex=1,col="#404040", labels="22", pos=4)

points(mdpcluster23$lon,mdpcluster23$lat, pch=8,cex=1,col="#404040")
text(mdpcluster23$lon,mdpcluster23$lat, pch=8,cex=1,col="#404040",labels="23", pos=4)

points(mdpcluster24$lon,mdpcluster24$lat, pch=8,cex=1,col="#404040")
text(mdpcluster24$lon,mdpcluster24$lat, pch=8,cex=1,col="#404040", labels="24", pos=4)

points(mdpcluster25$lon,mdpcluster25$lat, pch=8,cex=1,col="#404040")
text(mdpcluster25$lon,mdpcluster25$lat, pch=8,cex=1,col="#404040", labels="25", pos=4)

points(mdpcluster26$lon,mdpcluster26$lat, pch=8,cex=1,col="#404040")
text(mdpcluster26$lon,mdpcluster26$lat, pch=8,cex=1,col="#404040", labels="26", pos=4)

points(mdpcluster27$lon,mdpcluster27$lat, pch=8,cex=1,col="#404040")
text(mdpcluster27$lon,mdpcluster27$lat, pch=8,cex=1,col="#404040", labels="27", pos=4)

points(mdpcluster28$lon,mdpcluster28$lat, pch=8,cex=1,col="#404040")
text(mdpcluster28$lon,mdpcluster28$lat, pch=8,cex=1,col="#404040", labels="28", pos=4)

points(mdpcluster29$lon,mdpcluster29$lat, pch=8,cex=1,col="#404040")
text(mdpcluster29$lon,mdpcluster29$lat, pch=8,cex=1,col="#404040", labels="29", pos=4)

points(mdpcluster30$lon,mdpcluster30$lat, pch=8,cex=1,col="#404040")
text(mdpcluster30$lon,mdpcluster30$lat, pch=8,cex=1,col="#404040", labels="30", pos=4)

points(mdpcluster31$lon,mdpcluster31$lat, pch=8,cex=1,col="#404040")
text(mdpcluster31$lon,mdpcluster31$lat, pch=8,cex=1,col="#404040", labels="31", pos=4)

points(mdpcluster32$lon,mdpcluster32$lat, pch=8,cex=1,col="#404040")
text(mdpcluster32$lon,mdpcluster32$lat, pch=8,cex=1,col="#404040", labels="32", pos=4)

points(mdpcluster33$lon,mdpcluster33$lat, pch=8,cex=1,col="#404040")
text(mdpcluster33$lon,mdpcluster33$lat, pch=8,cex=1,col="#404040", labels="33", pos=4)

points(mdpcluster34$lon,mdpcluster34$lat, pch=8,cex=1,col="#404040")
text(mdpcluster34$lon,mdpcluster34$lat, pch=8,cex=1,col="#404040", labels="34", pos=4)

points(mdpcluster35$lon,mdpcluster35$lat, pch=8,cex=1,col="#404040")
text(mdpcluster35$lon,mdpcluster35$lat, pch=8,cex=1,col="#404040", labels="35", pos=4)

points(mdpcluster36$lon,mdpcluster36$lat, pch=8,cex=1,col="#404040")
text(mdpcluster36$lon,mdpcluster36$lat, pch=8,cex=1,col="#404040", labels="36", pos=4)

points(mdpcluster37$lon,mdpcluster37$lat, pch=8,cex=1,col="#404040")
text(mdpcluster37$lon,mdpcluster37$lat, pch=8,cex=1,col="#404040", labels="37", pos=4)

points(mdpcluster38$lon,mdpcluster38$lat, pch=8,cex=1,col="#404040")
text(mdpcluster38$lon,mdpcluster38$lat, pch=8,cex=1,col="#404040", labels="38", pos=4)

points(mdpcluster39$lon,mdpcluster39$lat, pch=8,cex=1,col="#404040")
text(mdpcluster39$lon,mdpcluster39$lat, pch=8,cex=1,col="#404040", labels="39", pos=4)

points(mdpcluster40$lon,mdpcluster40$lat, pch=8,cex=1,col="#404040")
text(mdpcluster40$lon,mdpcluster40$lat, pch=8,cex=1,col="#404040", labels="40", pos=4)

points(mdpcluster41$lon,mdpcluster41$lat, pch=8,cex=1,col="#404040")
text(mdpcluster41$lon,mdpcluster41$lat, pch=8,cex=1,col="#404040", labels="41", pos=4)

points(mdpcluster42$lon,mdpcluster42$lat, pch=8,cex=1,col="#404040")
text(mdpcluster42$lon,mdpcluster42$lat, pch=8,cex=1,col="#404040", labels="42", pos=4)

points(mdpcluster43$lon,mdpcluster43$lat, pch=8,cex=1,col="#404040")
text(mdpcluster43$lon,mdpcluster43$lat, pch=8,cex=1,col="#404040", labels="43", pos=4)

points(mdpcluster44$lon,mdpcluster44$lat, pch=8,cex=1,col="#404040")
text(mdpcluster44$lon,mdpcluster44$lat, pch=8,cex=1,col="#404040", labels="44", pos=4)

points(mdpcluster45$lon,mdpcluster45$lat, pch=8,cex=1,col="#404040")
text(mdpcluster45$lon,mdpcluster45$lat, pch=8,cex=1,col="#404040", labels="45", pos=4)

points(mdpcluster46$lon,mdpcluster46$lat, pch=8,cex=1,col="#404040")
text(mdpcluster46$lon,mdpcluster46$lat, pch=8,cex=1,col="#404040", labels="46", pos=4)

points(mdpcluster47$lon,mdpcluster47$lat, pch=8,cex=1,col="#404040")
text(mdpcluster47$lon,mdpcluster47$lat, pch=8,cex=1,col="#404040", labels="47", pos=4)

points(mdpcluster48$lon,mdpcluster48$lat, pch=8,cex=1,col="#404040")
text(mdpcluster48$lon,mdpcluster48$lat, pch=8,cex=1,col="#404040", labels="48", pos=4)

points(mdpcluster49$lon,mdpcluster49$lat, pch=8,cex=1,col="#404040")
text(mdpcluster49$lon,mdpcluster49$lat, pch=8,cex=1,col="#404040", labels="49", pos=4)

points(mdpcluster50$lon,mdpcluster50$lat, pch=8,cex=1,col="#404040")
text(mdpcluster50$lon,mdpcluster50$lat, pch=8,cex=1,col="#404040", labels="50", pos=4)

dev.off()

#prepara os dados para geolocalização reversa
wlmdp <- rbind(mdpcluster1, mdpcluster2, mdpcluster3, mdpcluster4, mdpcluster5, mdpcluster6, mdpcluster7, mdpcluster8, mdpcluster9, mdpcluster10, mdpcluster11, mdpcluster12, mdpcluster13, mdpcluster14, mdpcluster15, mdpcluster16, mdpcluster17, mdpcluster18, mdpcluster19, mdpcluster20, mdpcluster21, mdpcluster22, mdpcluster23, mdpcluster24, mdpcluster25, mdpcluster26, mdpcluster27, mdpcluster28, mdpcluster29, mdpcluster30,
mdpcluster31, mdpcluster32, mdpcluster33, mdpcluster34, mdpcluster35, mdpcluster36, mdpcluster37, mdpcluster38, mdpcluster39, mdpcluster40,
mdpcluster41, mdpcluster42, mdpcluster43, mdpcluster44, mdpcluster45, mdpcluster46, mdpcluster47, mdpcluster48, mdpcluster49, mdpcluster50)

library(tmaptools)
start.time = Sys.time()
wl <- rev_geocode_OSM(wlmdp$lon, wlmdp$lat, as.data.frame=TRUE)
end.time = Sys.time()
time.taken = end.time - start.time
time.taken
#demora em média < 2 minutos

# calcula as distancias a partir de wl
osrmTable(src=cluster1lonlat[c(1,4,3)], dst=as.data.frame(cbind(wl$town[1], wl$lon[1], wl$lat[1]), stringsAsFactors=FALSE), measure="distance")


#trabalhando os agrupamentos encontrados para apresentar no mapa
write.table(file="agrupamentos.csv",cbind(wl$city, wl$town, wl$village, wl$state,  wl$x, wl$y, wl$lon, wl$lat), sep=';')
#necessario realizar alguns ajustes no excel

#carrega os agrupamentos trabalhados
agrupamentos <- read.csv(file="agrupamentosKDD.csv", stringsAsFactors=FALSE, header=TRUE, sep=";")
agrupamentos$lon <- gsub(",",".",agrupamentos$lon)
agrupamentos$lon <- as.double(agrupamentos$lon)
agrupamentos$lat <- gsub(",",".",agrupamentos$lat)
agrupamentos$lat <- as.double(agrupamentos$lat)

#apresentando o potencial em m3 de cada cluster

#cluster1 reverso
revcluster1 <- osrmTable(src=cluster1lonlat[c("localizacao","lon","lat")], dst=as.data.frame(cbind(agrupamentos$localizacao[1], agrupamentos$lon[1], agrupamentos$lat[1]), stringsAsFactors=FALSE), measure="distance")
#une dados de capm3, lon, lat
rc1dd <- data.frame(localizacao=cbind(rownames(revcluster1$distances)), distancia=matrix(revcluster1$distances), stringsAsFactors=FALSE)
rc1dd <- left_join(rc1dd, cluster1lonlat, by = c("localizacao" = "localizacao"))
#ordena pela distancia
rc1dd<-rc1dd[order(rc1dd$distancia),]
rc1pot <- sum(rc1dd$CapEstNem3[rc1dd$distancia < 150000])
write.table(file="rc1dd.csv", rc1dd, sep=";")

#cluster2 reverso
revcluster2 <- osrmTable(src=cluster2lonlat[c("localizacao","lon","lat")], dst=as.data.frame(cbind(agrupamentos$localizacao[2], agrupamentos$lon[2], agrupamentos$lat[2]), stringsAsFactors=FALSE), measure="distance")
#une dados de capm3, lon, lat
rc2dd <- data.frame(localizacao=cbind(rownames(revcluster2$distances)), distancia=matrix(revcluster2$distances), stringsAsFactors=FALSE)
rc2dd <- left_join(rc2dd, cluster2lonlat, by = c("localizacao" = "localizacao"))
#ordena pela distancia
rc2dd<-rc2dd[order(rc2dd$distancia),]
rc2pot <- sum(rc2dd$CapEstNem3[rc2dd$distancia < 150000])
write.table(file="rc2dd.csv", rc2dd, sep=";")

#cluster3 reverso
revcluster3 <- osrmTable(src=cluster3lonlat[c("localizacao","lon","lat")], dst=as.data.frame(cbind(agrupamentos$localizacao[3], agrupamentos$lon[3], agrupamentos$lat[3]), stringsAsFactors=FALSE), measure="distance")
#une dados de capm3, lon, lat
rc3dd <- data.frame(localizacao=cbind(rownames(revcluster3$distances)), distancia=matrix(revcluster3$distances), stringsAsFactors=FALSE)
rc3dd <- left_join(rc3dd, cluster3lonlat, by = c("localizacao" = "localizacao"))
#ordena pela distancia
rc3dd<-rc3dd[order(rc3dd$distancia),]
rc3pot <- sum(rc3dd$CapEstNem3[rc3dd$distancia < 150000])
write.table(file="rc3dd.csv", rc3dd, sep=";")

#cluster4 reverso
revcluster4 <- osrmTable(src=cluster4lonlat[c("localizacao","lon","lat")], dst=as.data.frame(cbind(agrupamentos$localizacao[4], agrupamentos$lon[4], agrupamentos$lat[4]), stringsAsFactors=FALSE), measure="distance")
#une dados de capm3, lon, lat
rc4dd <- data.frame(localizacao=cbind(rownames(revcluster4$distances)), distancia=matrix(revcluster4$distances), stringsAsFactors=FALSE)
rc4dd <- left_join(rc4dd, cluster4lonlat, by = c("localizacao" = "localizacao"))
#ordena pela distancia
rc4dd<-rc4dd[order(rc4dd$distancia),]
rc4pot <- sum(rc4dd$CapEstNem3[rc4dd$distancia < 150000])
write.table(file="rc4dd.csv", rc4dd, sep=";")

#cluster5 reverso
revcluster5 <- osrmTable(src=cluster5lonlat[c("localizacao","lon","lat")], dst=as.data.frame(cbind(agrupamentos$localizacao[5], agrupamentos$lon[5], agrupamentos$lat[5]), stringsAsFactors=FALSE), measure="distance")
#une dados de capm3, lon, lat
rc5dd <- data.frame(localizacao=cbind(rownames(revcluster5$distances)), distancia=matrix(revcluster5$distances), stringsAsFactors=FALSE)
rc5dd <- left_join(rc5dd, cluster5lonlat, by = c("localizacao" = "localizacao"))
#ordena pela distancia
rc5dd<-rc5dd[order(rc5dd$distancia),]
rc5pot <- sum(rc5dd$CapEstNem3[rc5dd$distancia < 150000])
write.table(file="rc5dd.csv", rc5dd, sep=";")

#cluster6 reverso
revcluster6 <- osrmTable(src=cluster6lonlat[c("localizacao","lon","lat")], dst=as.data.frame(cbind(agrupamentos$localizacao[6], agrupamentos$lon[6], agrupamentos$lat[6]), stringsAsFactors=FALSE), measure="distance")
#une dados de capm3, lon, lat
rc6dd <- data.frame(localizacao=cbind(rownames(revcluster6$distances)), distancia=matrix(revcluster6$distances), stringsAsFactors=FALSE)
rc6dd <- left_join(rc6dd, cluster6lonlat, by = c("localizacao" = "localizacao"))
#ordena pela distancia
rc6dd<-rc6dd[order(rc6dd$distancia),]
rc6pot <- sum(rc6dd$CapEstNem3[rc6dd$distancia < 150000])
write.table(file="rc6dd.csv", rc6dd, sep=";")

#cluster7 reverso
revcluster7 <- osrmTable(src=cluster7lonlat[c("localizacao","lon","lat")], dst=as.data.frame(cbind(agrupamentos$localizacao[7], agrupamentos$lon[7], agrupamentos$lat[7]), stringsAsFactors=FALSE), measure="distance")
#une dados de capm3, lon, lat
rc7dd <- data.frame(localizacao=cbind(rownames(revcluster7$distances)), distancia=matrix(revcluster7$distances), stringsAsFactors=FALSE)
rc7dd <- left_join(rc7dd, cluster7lonlat, by = c("localizacao" = "localizacao"))
#ordena pela distancia
rc7dd<-rc7dd[order(rc7dd$distancia),]
rc7pot <- sum(rc7dd$CapEstNem3[rc7dd$distancia < 150000])
write.table(file="rc7dd.csv", rc7dd, sep=";")

#cluster8 reverso
revcluster8 <- osrmTable(src=cluster8lonlat[c("localizacao","lon","lat")], dst=as.data.frame(cbind(agrupamentos$localizacao[8], agrupamentos$lon[8], agrupamentos$lat[8]), stringsAsFactors=FALSE), measure="distance")
#une dados de capm3, lon, lat
rc8dd <- data.frame(localizacao=cbind(rownames(revcluster8$distances)), distancia=matrix(revcluster8$distances), stringsAsFactors=FALSE)
rc8dd <- left_join(rc8dd, cluster8lonlat, by = c("localizacao" = "localizacao"))
#ordena pela distancia
rc8dd<-rc8dd[order(rc8dd$distancia),]
rc8pot <- sum(rc8dd$CapEstNem3[rc8dd$distancia < 150000])
write.table(file="rc8dd.csv", rc8dd, sep=";")

#cluster9 reverso
revcluster9 <- osrmTable(src=cluster9lonlat[c("localizacao","lon","lat")], dst=as.data.frame(cbind(agrupamentos$localizacao[9], agrupamentos$lon[9], agrupamentos$lat[9]), stringsAsFactors=FALSE), measure="distance")
#une dados de capm3, lon, lat
rc9dd <- data.frame(localizacao=cbind(rownames(revcluster9$distances)), distancia=matrix(revcluster9$distances), stringsAsFactors=FALSE)
rc9dd <- left_join(rc9dd, cluster9lonlat, by = c("localizacao" = "localizacao"))
#ordena pela distancia
rc9dd<-rc9dd[order(rc9dd$distancia),]
rc9pot <- sum(rc9dd$CapEstNem3[rc9dd$distancia < 150000])
write.table(file="rc9dd.csv", rc9dd, sep=";")

#cluster10 reverso
revcluster10 <- osrmTable(src=cluster10lonlat[c("localizacao","lon","lat")], dst=as.data.frame(cbind(agrupamentos$localizacao[10], agrupamentos$lon[10], agrupamentos$lat[10]), stringsAsFactors=FALSE), measure="distance")
#une dados de capm3, lon, lat
rc10dd <- data.frame(localizacao=cbind(rownames(revcluster10$distances)), distancia=matrix(revcluster10$distances), stringsAsFactors=FALSE)
rc10dd <- left_join(rc10dd, cluster10lonlat, by = c("localizacao" = "localizacao"))
#ordena pela distancia
rc10dd<-rc10dd[order(rc10dd$distancia),]
rc10pot <- sum(rc10dd$CapEstNem3[rc10dd$distancia < 150000])
write.table(file="rc10dd.csv", rc10dd, sep=";")

#cluster11 reverso
revcluster11 <- osrmTable(src=cluster11lonlat[c("localizacao","lon","lat")], dst=as.data.frame(cbind(agrupamentos$localizacao[11], agrupamentos$lon[11], agrupamentos$lat[11]), stringsAsFactors=FALSE), measure="distance")
#une dados de capm3, lon, lat
rc11dd <- data.frame(localizacao=cbind(rownames(revcluster11$distances)), distancia=matrix(revcluster11$distances), stringsAsFactors=FALSE)
rc11dd <- left_join(rc11dd, cluster11lonlat, by = c("localizacao" = "localizacao"))
#ordena pela distancia
rc11dd<-rc11dd[order(rc11dd$distancia),]
rc11pot <- sum(rc11dd$CapEstNem3[rc11dd$distancia < 150000])
write.table(file="rc11dd.csv", rc11dd, sep=";")

#cluster12 reverso
revcluster12 <- osrmTable(src=cluster12lonlat[c("localizacao","lon","lat")], dst=as.data.frame(cbind(agrupamentos$localizacao[12], agrupamentos$lon[12], agrupamentos$lat[12]), stringsAsFactors=FALSE), measure="distance")
#une dados de capm3, lon, lat
rc12dd <- data.frame(localizacao=cbind(rownames(revcluster12$distances)), distancia=matrix(revcluster12$distances), stringsAsFactors=FALSE)
rc12dd <- left_join(rc12dd, cluster12lonlat, by = c("localizacao" = "localizacao"))
#ordena pela distancia
rc12dd<-rc12dd[order(rc12dd$distancia),]
rc12pot <- sum(rc12dd$CapEstNem3[rc12dd$distancia < 150000])
write.table(file="rc12dd.csv", rc12dd, sep=";")

#cluster13 reverso
revcluster13 <- osrmTable(src=cluster13lonlat[c("localizacao","lon","lat")], dst=as.data.frame(cbind(agrupamentos$localizacao[13], agrupamentos$lon[13], agrupamentos$lat[13]), stringsAsFactors=FALSE), measure="distance")
#une dados de capm3, lon, lat
rc13dd <- data.frame(localizacao=cbind(rownames(revcluster13$distances)), distancia=matrix(revcluster13$distances), stringsAsFactors=FALSE)
rc13dd <- left_join(rc13dd, cluster13lonlat, by = c("localizacao" = "localizacao"))
#ordena pela distancia
rc13dd<-rc13dd[order(rc13dd$distancia),]
rc13pot <- sum(rc13dd$CapEstNem3[rc13dd$distancia < 150000])
write.table(file="rc13dd.csv", rc13dd, sep=";")

#cluster14 reverso
revcluster14 <- osrmTable(src=cluster14lonlat[c("localizacao","lon","lat")], dst=as.data.frame(cbind(agrupamentos$localizacao[14], agrupamentos$lon[14], agrupamentos$lat[14]), stringsAsFactors=FALSE), measure="distance")
#une dados de capm3, lon, lat
rc14dd <- data.frame(localizacao=cbind(rownames(revcluster14$distances)), distancia=matrix(revcluster14$distances), stringsAsFactors=FALSE)
rc14dd <- left_join(rc14dd, cluster14lonlat, by = c("localizacao" = "localizacao"))
#ordena pela distancia
rc14dd<-rc14dd[order(rc14dd$distancia),]
rc14pot <- sum(rc14dd$CapEstNem3[rc14dd$distancia < 150000])
write.table(file="rc14dd.csv", rc14dd, sep=";")

#cluster15 reverso
revcluster15 <- osrmTable(src=cluster15lonlat[c("localizacao","lon","lat")], dst=as.data.frame(cbind(agrupamentos$localizacao[15], agrupamentos$lon[15], agrupamentos$lat[15]), stringsAsFactors=FALSE), measure="distance")
#une dados de capm3, lon, lat
rc15dd <- data.frame(localizacao=cbind(rownames(revcluster15$distances)), distancia=matrix(revcluster15$distances), stringsAsFactors=FALSE)
rc15dd <- left_join(rc15dd, cluster15lonlat, by = c("localizacao" = "localizacao"))
#ordena pela distancia
rc15dd<-rc15dd[order(rc15dd$distancia),]
rc15pot <- sum(rc15dd$CapEstNem3[rc15dd$distancia < 150000])
write.table(file="rc15dd.csv", rc15dd, sep=";")

#cluster16 reverso
revcluster16 <- osrmTable(src=cluster16lonlat[c("localizacao","lon","lat")], dst=as.data.frame(cbind(agrupamentos$localizacao[16], agrupamentos$lon[16], agrupamentos$lat[16]), stringsAsFactors=FALSE), measure="distance")
#une dados de capm3, lon, lat
rc16dd <- data.frame(localizacao=cbind(rownames(revcluster16$distances)), distancia=matrix(revcluster16$distances), stringsAsFactors=FALSE)
rc16dd <- left_join(rc16dd, cluster16lonlat, by = c("localizacao" = "localizacao"))
#ordena pela distancia
rc16dd<-rc16dd[order(rc16dd$distancia),]
rc16pot <- sum(rc16dd$CapEstNem3[rc16dd$distancia < 150000])
write.table(file="rc16dd.csv", rc16dd, sep=";")

#cluster17 reverso
revcluster17 <- osrmTable(src=cluster17lonlat[c("localizacao","lon","lat")], dst=as.data.frame(cbind(agrupamentos$localizacao[17], agrupamentos$lon[17], agrupamentos$lat[17]), stringsAsFactors=FALSE), measure="distance")
#une dados de capm3, lon, lat
rc17dd <- data.frame(localizacao=cbind(rownames(revcluster17$distances)), distancia=matrix(revcluster17$distances), stringsAsFactors=FALSE)
rc17dd <- left_join(rc17dd, cluster17lonlat, by = c("localizacao" = "localizacao"))
#ordena pela distancia
rc17dd<-rc17dd[order(rc17dd$distancia),]
rc17pot <- sum(rc17dd$CapEstNem3[rc17dd$distancia < 150000])
write.table(file="rc17dd.csv", rc17dd, sep=";")

#cluster18 reverso
revcluster18 <- osrmTable(src=cluster18lonlat[c("localizacao","lon","lat")], dst=as.data.frame(cbind(agrupamentos$localizacao[18], agrupamentos$lon[18], agrupamentos$lat[18]), stringsAsFactors=FALSE), measure="distance")
#une dados de capm3, lon, lat
rc18dd <- data.frame(localizacao=cbind(rownames(revcluster18$distances)), distancia=matrix(revcluster18$distances), stringsAsFactors=FALSE)
rc18dd <- left_join(rc18dd, cluster18lonlat, by = c("localizacao" = "localizacao"))
#ordena pela distancia
rc18dd<-rc18dd[order(rc18dd$distancia),]
rc18pot <- sum(rc18dd$CapEstNem3[rc18dd$distancia < 150000])
write.table(file="rc18dd.csv", rc18dd, sep=";")

#cluster19 reverso
revcluster19 <- osrmTable(src=cluster19lonlat[c("localizacao","lon","lat")], dst=as.data.frame(cbind(agrupamentos$localizacao[19], agrupamentos$lon[19], agrupamentos$lat[19]), stringsAsFactors=FALSE), measure="distance")
#une dados de capm3, lon, lat
rc19dd <- data.frame(localizacao=cbind(rownames(revcluster19$distances)), distancia=matrix(revcluster19$distances), stringsAsFactors=FALSE)
rc19dd <- left_join(rc19dd, cluster19lonlat, by = c("localizacao" = "localizacao"))
#ordena pela distancia
rc19dd<-rc19dd[order(rc19dd$distancia),]
rc19pot <- sum(rc19dd$CapEstNem3[rc19dd$distancia < 150000])
write.table(file="rc19dd.csv", rc19dd, sep=";")

#cluster20 reverso
revcluster20 <- osrmTable(src=cluster20lonlat[c("localizacao","lon","lat")], dst=as.data.frame(cbind(agrupamentos$localizacao[20], agrupamentos$lon[20], agrupamentos$lat[20]), stringsAsFactors=FALSE), measure="distance")
#une dados de capm3, lon, lat
rc20dd <- data.frame(localizacao=cbind(rownames(revcluster20$distances)), distancia=matrix(revcluster20$distances), stringsAsFactors=FALSE)
rc20dd <- left_join(rc20dd, cluster20lonlat, by = c("localizacao" = "localizacao"))
#ordena pela distancia
rc20dd<-rc20dd[order(rc20dd$distancia),]
rc20pot <- sum(rc20dd$CapEstNem3[rc20dd$distancia < 150000])
write.table(file="rc20dd.csv", rc20dd, sep=";")

#cluster21 reverso
revcluster21 <- osrmTable(src=cluster21lonlat[c("localizacao","lon","lat")], dst=as.data.frame(cbind(agrupamentos$localizacao[21], agrupamentos$lon[21], agrupamentos$lat[21]), stringsAsFactors=FALSE), measure="distance")
#une dados de capm3, lon, lat
rc21dd <- data.frame(localizacao=cbind(rownames(revcluster21$distances)), distancia=matrix(revcluster21$distances), stringsAsFactors=FALSE)
rc21dd <- left_join(rc21dd, cluster21lonlat, by = c("localizacao" = "localizacao"))
#ordena pela distancia
rc21dd<-rc21dd[order(rc21dd$distancia),]
rc21pot <- sum(rc21dd$CapEstNem3[rc21dd$distancia < 150000])
write.table(file="rc21dd.csv", rc21dd, sep=";")

#cluster22 reverso
revcluster22 <- osrmTable(src=cluster22lonlat[c("localizacao","lon","lat")], dst=as.data.frame(cbind(agrupamentos$localizacao[22], agrupamentos$lon[22], agrupamentos$lat[22]), stringsAsFactors=FALSE), measure="distance")
#une dados de capm3, lon, lat
rc22dd <- data.frame(localizacao=cbind(rownames(revcluster22$distances)), distancia=matrix(revcluster22$distances), stringsAsFactors=FALSE)
rc22dd <- left_join(rc22dd, cluster22lonlat, by = c("localizacao" = "localizacao"))
#ordena pela distancia
rc22dd<-rc22dd[order(rc22dd$distancia),]
rc22pot <- sum(rc22dd$CapEstNem3[rc22dd$distancia < 150000])
write.table(file="rc22dd.csv", rc22dd, sep=";")

#cluster23 reverso
revcluster23 <- osrmTable(src=cluster23lonlat[c("localizacao","lon","lat")], dst=as.data.frame(cbind(agrupamentos$localizacao[23], agrupamentos$lon[23], agrupamentos$lat[23]), stringsAsFactors=FALSE), measure="distance")
#une dados de capm3, lon, lat
rc23dd <- data.frame(localizacao=cbind(rownames(revcluster23$distances)), distancia=matrix(revcluster23$distances), stringsAsFactors=FALSE)
rc23dd <- left_join(rc23dd, cluster23lonlat, by = c("localizacao" = "localizacao"))
#ordena pela distancia
rc23dd<-rc23dd[order(rc23dd$distancia),]
rc23pot <- sum(rc23dd$CapEstNem3[rc23dd$distancia < 150000])
write.table(file="rc23dd.csv", rc23dd, sep=";")

#cluster24 reverso
revcluster24 <- osrmTable(src=cluster24lonlat[c("localizacao","lon","lat")], dst=as.data.frame(cbind(agrupamentos$localizacao[24], agrupamentos$lon[24], agrupamentos$lat[24]), stringsAsFactors=FALSE), measure="distance")
#une dados de capm3, lon, lat
rc24dd <- data.frame(localizacao=cbind(rownames(revcluster24$distances)), distancia=matrix(revcluster24$distances), stringsAsFactors=FALSE)
rc24dd <- left_join(rc24dd, cluster24lonlat, by = c("localizacao" = "localizacao"))
#ordena pela distancia
rc24dd<-rc24dd[order(rc24dd$distancia),]
rc24pot <- sum(rc24dd$CapEstNem3[rc24dd$distancia < 150000])
write.table(file="rc24dd.csv", rc24dd, sep=";")

#cluster25 reverso
revcluster25 <- osrmTable(src=cluster25lonlat[c("localizacao","lon","lat")], dst=as.data.frame(cbind(agrupamentos$localizacao[25], agrupamentos$lon[25], agrupamentos$lat[25]), stringsAsFactors=FALSE), measure="distance")
#une dados de capm3, lon, lat
rc25dd <- data.frame(localizacao=cbind(rownames(revcluster25$distances)), distancia=matrix(revcluster25$distances), stringsAsFactors=FALSE)
rc25dd <- left_join(rc25dd, cluster25lonlat, by = c("localizacao" = "localizacao"))
#ordena pela distancia
rc25dd<-rc25dd[order(rc25dd$distancia),]
rc25pot <- sum(rc25dd$CapEstNem3[rc25dd$distancia < 150000])
write.table(file="rc25dd.csv", rc25dd, sep=";")

#cluster26 reverso
revcluster26 <- osrmTable(src=cluster26lonlat[c("localizacao","lon","lat")], dst=as.data.frame(cbind(agrupamentos$localizacao[26], agrupamentos$lon[26], agrupamentos$lat[26]), stringsAsFactors=FALSE), measure="distance")
#une dados de capm3, lon, lat
rc26dd <- data.frame(localizacao=cbind(rownames(revcluster26$distances)), distancia=matrix(revcluster26$distances), stringsAsFactors=FALSE)
rc26dd <- left_join(rc26dd, cluster26lonlat, by = c("localizacao" = "localizacao"))
#ordena pela distancia
rc26dd<-rc26dd[order(rc26dd$distancia),]
rc26pot <- sum(rc26dd$CapEstNem3[rc26dd$distancia < 150000])
write.table(file="rc26dd.csv", rc26dd, sep=";")

#cluster27 reverso
revcluster27 <- osrmTable(src=cluster27lonlat[c("localizacao","lon","lat")], dst=as.data.frame(cbind(agrupamentos$localizacao[27], agrupamentos$lon[27], agrupamentos$lat[27]), stringsAsFactors=FALSE), measure="distance")
#une dados de capm3, lon, lat
rc27dd <- data.frame(localizacao=cbind(rownames(revcluster27$distances)), distancia=matrix(revcluster27$distances), stringsAsFactors=FALSE)
rc27dd <- left_join(rc27dd, cluster27lonlat, by = c("localizacao" = "localizacao"))
#ordena pela distancia
rc27dd<-rc27dd[order(rc27dd$distancia),]
rc27pot <- sum(rc27dd$CapEstNem3[rc27dd$distancia < 150000])
write.table(file="rc27dd.csv", rc27dd, sep=";")

#cluster28 reverso
revcluster28 <- osrmTable(src=cluster28lonlat[c("localizacao","lon","lat")], dst=as.data.frame(cbind(agrupamentos$localizacao[28], agrupamentos$lon[28], agrupamentos$lat[28]), stringsAsFactors=FALSE), measure="distance")
#une dados de capm3, lon, lat
rc28dd <- data.frame(localizacao=cbind(rownames(revcluster28$distances)), distancia=matrix(revcluster28$distances), stringsAsFactors=FALSE)
rc28dd <- left_join(rc28dd, cluster28lonlat, by = c("localizacao" = "localizacao"))
#ordena pela distancia
rc28dd<-rc28dd[order(rc28dd$distancia),]
rc28pot <- sum(rc28dd$CapEstNem3[rc28dd$distancia < 150000])
write.table(file="rc28dd.csv", rc28dd, sep=";")

#cluster29 reverso
revcluster29 <- osrmTable(src=cluster29lonlat[c("localizacao","lon","lat")], dst=as.data.frame(cbind(agrupamentos$localizacao[29], agrupamentos$lon[29], agrupamentos$lat[29]), stringsAsFactors=FALSE), measure="distance")
#une dados de capm3, lon, lat
rc29dd <- data.frame(localizacao=cbind(rownames(revcluster29$distances)), distancia=matrix(revcluster29$distances), stringsAsFactors=FALSE)
rc29dd <- left_join(rc29dd, cluster29lonlat, by = c("localizacao" = "localizacao"))
#ordena pela distancia
rc29dd<-rc29dd[order(rc29dd$distancia),]
rc29pot <- sum(rc29dd$CapEstNem3[rc29dd$distancia < 150000])
write.table(file="rc29dd.csv", rc29dd, sep=";")

#cluster30 reverso
revcluster30 <- osrmTable(src=cluster30lonlat[c("localizacao","lon","lat")], dst=as.data.frame(cbind(agrupamentos$localizacao[30], agrupamentos$lon[30], agrupamentos$lat[30]), stringsAsFactors=FALSE), measure="distance")
#une dados de capm3, lon, lat
rc30dd <- data.frame(localizacao=cbind(rownames(revcluster30$distances)), distancia=matrix(revcluster30$distances), stringsAsFactors=FALSE)
rc30dd <- left_join(rc30dd, cluster30lonlat, by = c("localizacao" = "localizacao"))
#ordena pela distancia
rc30dd<-rc30dd[order(rc30dd$distancia),]
rc30pot <- sum(rc30dd$CapEstNem3[rc30dd$distancia < 150000])
write.table(file="rc30dd.csv", rc30dd, sep=";")

#cluster31 reverso
revcluster31 <- osrmTable(src=cluster31lonlat[c("localizacao","lon","lat")], dst=as.data.frame(cbind(agrupamentos$localizacao[31], agrupamentos$lon[31], agrupamentos$lat[31]), stringsAsFactors=FALSE), measure="distance")
#une dados de capm3, lon, lat
rc31dd <- data.frame(localizacao=cbind(rownames(revcluster31$distances)), distancia=matrix(revcluster31$distances), stringsAsFactors=FALSE)
rc31dd <- left_join(rc31dd, cluster31lonlat, by = c("localizacao" = "localizacao"))
#ordena pela distancia
rc31dd<-rc31dd[order(rc31dd$distancia),]
rc31pot <- sum(rc31dd$CapEstNem3[rc31dd$distancia < 150000])
write.table(file="rc31dd.csv", rc31dd, sep=";")

#cluster32 reverso
revcluster32 <- osrmTable(src=cluster32lonlat[c("localizacao","lon","lat")], dst=as.data.frame(cbind(agrupamentos$localizacao[32], agrupamentos$lon[32], agrupamentos$lat[32]), stringsAsFactors=FALSE), measure="distance")
#une dados de capm3, lon, lat
rc32dd <- data.frame(localizacao=cbind(rownames(revcluster32$distances)), distancia=matrix(revcluster32$distances), stringsAsFactors=FALSE)
rc32dd <- left_join(rc32dd, cluster32lonlat, by = c("localizacao" = "localizacao"))
#ordena pela distancia
rc32dd<-rc32dd[order(rc32dd$distancia),]
rc32pot <- sum(rc32dd$CapEstNem3[rc32dd$distancia < 150000])
write.table(file="rc32dd.csv", rc32dd, sep=";")

#cluster33 reverso
revcluster33 <- osrmTable(src=cluster33lonlat[c("localizacao","lon","lat")], dst=as.data.frame(cbind(agrupamentos$localizacao[33], agrupamentos$lon[33], agrupamentos$lat[33]), stringsAsFactors=FALSE), measure="distance")
#une dados de capm3, lon, lat
rc33dd <- data.frame(localizacao=cbind(rownames(revcluster33$distances)), distancia=matrix(revcluster33$distances), stringsAsFactors=FALSE)
rc33dd <- left_join(rc33dd, cluster33lonlat, by = c("localizacao" = "localizacao"))
#ordena pela distancia
rc33dd<-rc33dd[order(rc33dd$distancia),]
rc33pot <- sum(rc33dd$CapEstNem3[rc33dd$distancia < 150000])
write.table(file="rc33dd.csv", rc33dd, sep=";")

#cluster34 reverso
revcluster34 <- osrmTable(src=cluster34lonlat[c("localizacao","lon","lat")], dst=as.data.frame(cbind(agrupamentos$localizacao[34], agrupamentos$lon[34], agrupamentos$lat[34]), stringsAsFactors=FALSE), measure="distance")
#une dados de capm3, lon, lat
rc34dd <- data.frame(localizacao=cbind(rownames(revcluster34$distances)), distancia=matrix(revcluster34$distances), stringsAsFactors=FALSE)
rc34dd <- left_join(rc34dd, cluster34lonlat, by = c("localizacao" = "localizacao"))
#ordena pela distancia
rc34dd<-rc34dd[order(rc34dd$distancia),]
rc34pot <- sum(rc34dd$CapEstNem3[rc34dd$distancia < 150000])
write.table(file="rc34dd.csv", rc34dd, sep=";")

#cluster35 reverso
revcluster35 <- osrmTable(src=cluster35lonlat[c("localizacao","lon","lat")], dst=as.data.frame(cbind(agrupamentos$localizacao[35], agrupamentos$lon[35], agrupamentos$lat[35]), stringsAsFactors=FALSE), measure="distance")
#une dados de capm3, lon, lat
rc35dd <- data.frame(localizacao=cbind(rownames(revcluster35$distances)), distancia=matrix(revcluster35$distances), stringsAsFactors=FALSE)
rc35dd <- left_join(rc35dd, cluster35lonlat, by = c("localizacao" = "localizacao"))
#ordena pela distancia
rc35dd<-rc35dd[order(rc35dd$distancia),]
rc35pot <- sum(rc35dd$CapEstNem3[rc35dd$distancia < 150000])
write.table(file="rc35dd.csv", rc35dd, sep=";")

#cluster36 reverso
revcluster36 <- osrmTable(src=cluster36lonlat[c("localizacao","lon","lat")], dst=as.data.frame(cbind(agrupamentos$localizacao[36], agrupamentos$lon[36], agrupamentos$lat[36]), stringsAsFactors=FALSE), measure="distance")
#une dados de capm3, lon, lat
rc36dd <- data.frame(localizacao=cbind(rownames(revcluster36$distances)), distancia=matrix(revcluster36$distances), stringsAsFactors=FALSE)
rc36dd <- left_join(rc36dd, cluster36lonlat, by = c("localizacao" = "localizacao"))
#ordena pela distancia
rc36dd<-rc36dd[order(rc36dd$distancia),]
rc36pot <- sum(rc36dd$CapEstNem3[rc36dd$distancia < 150000])
write.table(file="rc36dd.csv", rc36dd, sep=";")

#cluster37 reverso
revcluster37 <- osrmTable(src=cluster37lonlat[c("localizacao","lon","lat")], dst=as.data.frame(cbind(agrupamentos$localizacao[37], agrupamentos$lon[37], agrupamentos$lat[37]), stringsAsFactors=FALSE), measure="distance")
#une dados de capm3, lon, lat
rc37dd <- data.frame(localizacao=cbind(rownames(revcluster37$distances)), distancia=matrix(revcluster37$distances), stringsAsFactors=FALSE)
rc37dd <- left_join(rc37dd, cluster37lonlat, by = c("localizacao" = "localizacao"))
#ordena pela distancia
rc37dd<-rc37dd[order(rc37dd$distancia),]
rc37pot <- sum(rc37dd$CapEstNem3[rc37dd$distancia < 150000])
write.table(file="rc37dd.csv", rc37dd, sep=";")

#cluster38 reverso
revcluster38 <- osrmTable(src=cluster38lonlat[c("localizacao","lon","lat")], dst=as.data.frame(cbind(agrupamentos$localizacao[38], agrupamentos$lon[38], agrupamentos$lat[38]), stringsAsFactors=FALSE), measure="distance")
#une dados de capm3, lon, lat
rc38dd <- data.frame(localizacao=cbind(rownames(revcluster38$distances)), distancia=matrix(revcluster38$distances), stringsAsFactors=FALSE)
rc38dd <- left_join(rc38dd, cluster38lonlat, by = c("localizacao" = "localizacao"))
#ordena pela distancia
rc38dd<-rc38dd[order(rc38dd$distancia),]
rc38pot <- sum(rc38dd$CapEstNem3[rc38dd$distancia < 150000])
write.table(file="rc38dd.csv", rc38dd, sep=";")

#cluster39 reverso
revcluster39 <- osrmTable(src=cluster39lonlat[c("localizacao","lon","lat")], dst=as.data.frame(cbind(agrupamentos$localizacao[39], agrupamentos$lon[39], agrupamentos$lat[39]), stringsAsFactors=FALSE), measure="distance")
#une dados de capm3, lon, lat
rc39dd <- data.frame(localizacao=cbind(rownames(revcluster39$distances)), distancia=matrix(revcluster39$distances), stringsAsFactors=FALSE)
rc39dd <- left_join(rc39dd, cluster39lonlat, by = c("localizacao" = "localizacao"))
#ordena pela distancia
rc39dd<-rc39dd[order(rc39dd$distancia),]
rc39pot <- sum(rc39dd$CapEstNem3[rc39dd$distancia < 150000])
write.table(file="rc39dd.csv", rc39dd, sep=";")

#cluster40 reverso
revcluster40 <- osrmTable(src=cluster40lonlat[c("localizacao","lon","lat")], dst=as.data.frame(cbind(agrupamentos$localizacao[40], agrupamentos$lon[40], agrupamentos$lat[40]), stringsAsFactors=FALSE), measure="distance")
#une dados de capm3, lon, lat
rc40dd <- data.frame(localizacao=cbind(rownames(revcluster40$distances)), distancia=matrix(revcluster40$distances), stringsAsFactors=FALSE)
rc40dd <- left_join(rc40dd, cluster40lonlat, by = c("localizacao" = "localizacao"))
#ordena pela distancia
rc40dd<-rc40dd[order(rc40dd$distancia),]
rc40pot <- sum(rc40dd$CapEstNem3[rc40dd$distancia < 150000])
write.table(file="rc40dd.csv", rc40dd, sep=";")

#cluster41 reverso
revcluster41 <- osrmTable(src=cluster41lonlat[c("localizacao","lon","lat")], dst=as.data.frame(cbind(agrupamentos$localizacao[41], agrupamentos$lon[41], agrupamentos$lat[41]), stringsAsFactors=FALSE), measure="distance")
#une dados de capm3, lon, lat
rc41dd <- data.frame(localizacao=cbind(rownames(revcluster41$distances)), distancia=matrix(revcluster41$distances), stringsAsFactors=FALSE)
rc41dd <- left_join(rc41dd, cluster41lonlat, by = c("localizacao" = "localizacao"))
#ordena pela distancia
rc41dd<-rc41dd[order(rc41dd$distancia),]
rc41pot <- sum(rc41dd$CapEstNem3[rc41dd$distancia < 150000])
write.table(file="rc41dd.csv", rc41dd, sep=";")

#cluster42 reverso
revcluster42 <- osrmTable(src=cluster42lonlat[c("localizacao","lon","lat")], dst=as.data.frame(cbind(agrupamentos$localizacao[42], agrupamentos$lon[42], agrupamentos$lat[42]), stringsAsFactors=FALSE), measure="distance")
#une dados de capm3, lon, lat
rc42dd <- data.frame(localizacao=cbind(rownames(revcluster42$distances)), distancia=matrix(revcluster42$distances), stringsAsFactors=FALSE)
rc42dd <- left_join(rc42dd, cluster42lonlat, by = c("localizacao" = "localizacao"))
#ordena pela distancia
rc42dd<-rc42dd[order(rc42dd$distancia),]
rc42pot <- sum(rc42dd$CapEstNem3[rc42dd$distancia < 150000])
write.table(file="rc42dd.csv", rc42dd, sep=";")

#cluster43 reverso
revcluster43 <- osrmTable(src=cluster43lonlat[c("localizacao","lon","lat")], dst=as.data.frame(cbind(agrupamentos$localizacao[43], agrupamentos$lon[43], agrupamentos$lat[43]), stringsAsFactors=FALSE), measure="distance")
#une dados de capm3, lon, lat
rc43dd <- data.frame(localizacao=cbind(rownames(revcluster43$distances)), distancia=matrix(revcluster43$distances), stringsAsFactors=FALSE)
rc43dd <- left_join(rc43dd, cluster43lonlat, by = c("localizacao" = "localizacao"))
#ordena pela distancia
rc43dd<-rc43dd[order(rc43dd$distancia),]
rc43pot <- sum(rc43dd$CapEstNem3[rc43dd$distancia < 150000])
write.table(file="rc43dd.csv", rc43dd, sep=";")

#cluster44 reverso
revcluster44 <- osrmTable(src=cluster44lonlat[c("localizacao","lon","lat")], dst=as.data.frame(cbind(agrupamentos$localizacao[44], agrupamentos$lon[44], agrupamentos$lat[44]), stringsAsFactors=FALSE), measure="distance")
#une dados de capm3, lon, lat
rc44dd <- data.frame(localizacao=cbind(rownames(revcluster44$distances)), distancia=matrix(revcluster44$distances), stringsAsFactors=FALSE)
rc44dd <- left_join(rc44dd, cluster44lonlat, by = c("localizacao" = "localizacao"))
#ordena pela distancia
rc44dd<-rc44dd[order(rc44dd$distancia),]
rc44pot <- sum(rc44dd$CapEstNem3[rc44dd$distancia < 150000])
write.table(file="rc44dd.csv", rc44dd, sep=";")

#cluster45 reverso
revcluster45 <- osrmTable(src=cluster45lonlat[c("localizacao","lon","lat")], dst=as.data.frame(cbind(agrupamentos$localizacao[45], agrupamentos$lon[45], agrupamentos$lat[45]), stringsAsFactors=FALSE), measure="distance")
#une dados de capm3, lon, lat
rc45dd <- data.frame(localizacao=cbind(rownames(revcluster45$distances)), distancia=matrix(revcluster45$distances), stringsAsFactors=FALSE)
rc45dd <- left_join(rc45dd, cluster45lonlat, by = c("localizacao" = "localizacao"))
#ordena pela distancia
rc45dd<-rc45dd[order(rc45dd$distancia),]
rc45pot <- sum(rc45dd$CapEstNem3[rc45dd$distancia < 150000])
write.table(file="rc45dd.csv", rc45dd, sep=";")

#cluster46 reverso
revcluster46 <- osrmTable(src=cluster46lonlat[c("localizacao","lon","lat")], dst=as.data.frame(cbind(agrupamentos$localizacao[46], agrupamentos$lon[46], agrupamentos$lat[46]), stringsAsFactors=FALSE), measure="distance")
#une dados de capm3, lon, lat
rc46dd <- data.frame(localizacao=cbind(rownames(revcluster46$distances)), distancia=matrix(revcluster46$distances), stringsAsFactors=FALSE)
rc46dd <- left_join(rc46dd, cluster46lonlat, by = c("localizacao" = "localizacao"))
#ordena pela distancia
rc46dd<-rc46dd[order(rc46dd$distancia),]
rc46pot <- sum(rc46dd$CapEstNem3[rc46dd$distancia < 150000])
write.table(file="rc46dd.csv", rc46dd, sep=";")

#cluster47 reverso
revcluster47 <- osrmTable(src=cluster47lonlat[c("localizacao","lon","lat")], dst=as.data.frame(cbind(agrupamentos$localizacao[47], agrupamentos$lon[47], agrupamentos$lat[47]), stringsAsFactors=FALSE), measure="distance")
#une dados de capm3, lon, lat
rc47dd <- data.frame(localizacao=cbind(rownames(revcluster47$distances)), distancia=matrix(revcluster47$distances), stringsAsFactors=FALSE)
rc47dd <- left_join(rc47dd, cluster47lonlat, by = c("localizacao" = "localizacao"))
#ordena pela distancia
rc47dd<-rc47dd[order(rc47dd$distancia),]
rc47pot <- sum(rc47dd$CapEstNem3[rc47dd$distancia < 150000])
write.table(file="rc47dd.csv", rc47dd, sep=";")

#cluster48 reverso
revcluster48 <- osrmTable(src=cluster48lonlat[c("localizacao","lon","lat")], dst=as.data.frame(cbind(agrupamentos$localizacao[48], agrupamentos$lon[48], agrupamentos$lat[48]), stringsAsFactors=FALSE), measure="distance")
#une dados de capm3, lon, lat
rc48dd <- data.frame(localizacao=cbind(rownames(revcluster48$distances)), distancia=matrix(revcluster48$distances), stringsAsFactors=FALSE)
rc48dd <- left_join(rc48dd, cluster48lonlat, by = c("localizacao" = "localizacao"))
#ordena pela distancia
rc48dd<-rc48dd[order(rc48dd$distancia),]
rc48pot <- sum(rc48dd$CapEstNem3[rc48dd$distancia < 150000])
write.table(file="rc48dd.csv", rc48dd, sep=";")

#cluster49 reverso
revcluster49 <- osrmTable(src=cluster49lonlat[c("localizacao","lon","lat")], dst=as.data.frame(cbind(agrupamentos$localizacao[49], agrupamentos$lon[49], agrupamentos$lat[49]), stringsAsFactors=FALSE), measure="distance")
#une dados de capm3, lon, lat
rc49dd <- data.frame(localizacao=cbind(rownames(revcluster49$distances)), distancia=matrix(revcluster49$distances), stringsAsFactors=FALSE)
rc49dd <- left_join(rc49dd, cluster49lonlat, by = c("localizacao" = "localizacao"))
#ordena pela distancia
rc49dd<-rc49dd[order(rc49dd$distancia),]
rc49pot <- sum(rc49dd$CapEstNem3[rc49dd$distancia < 150000])
write.table(file="rc49dd.csv", rc49dd, sep=";")

#cluster50 reverso
revcluster50 <- osrmTable(src=cluster50lonlat[c("localizacao","lon","lat")], dst=as.data.frame(cbind(agrupamentos$localizacao[50], agrupamentos$lon[50], agrupamentos$lat[50]), stringsAsFactors=FALSE), measure="distance")
#une dados de capm3, lon, lat
rc50dd <- data.frame(localizacao=cbind(rownames(revcluster50$distances)), distancia=matrix(revcluster50$distances), stringsAsFactors=FALSE)
rc50dd <- left_join(rc50dd, cluster50lonlat, by = c("localizacao" = "localizacao"))
#ordena pela distancia
rc50dd<-rc50dd[order(rc50dd$distancia),]
rc50pot <- sum(rc50dd$CapEstNem3[rc50dd$distancia < 150000])
write.table(file="rc50dd.csv", rc50dd, sep=";")

#final apresentando o potencial em m3 de cada cluster

#consolida todos os potencias em m3
rcpot <- rbind(rc1pot, rc2pot, rc3pot, rc4pot, rc5pot, rc6pot, rc7pot, rc8pot, rc9pot, rc10pot, rc11pot, rc12pot, rc13pot, rc14pot, rc15pot, rc16pot, rc17pot, rc18pot, rc19pot, rc20pot, rc21pot, rc22pot, rc23pot, rc24pot, rc25pot, rc26pot, rc27pot, rc28pot, rc29pot, rc30pot, rc31pot, rc32pot, rc33pot, rc34pot, rc35pot, rc36pot, rc37pot, rc38pot, rc39pot, rc40pot, rc41pot, rc42pot, rc43pot, rc44pot, rc45pot, rc46pot, rc47pot, rc48pot, rc49pot, rc50pot)

#apresenta a lista
data.frame(id=seq(length(rcpot)),pot=rcpot)





