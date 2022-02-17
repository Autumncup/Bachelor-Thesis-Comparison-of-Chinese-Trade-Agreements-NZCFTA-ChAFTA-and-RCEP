#NZCFTA
rm(list=ls())
# importing the required library
library(rio)
# reading data from all sheets
nzcfta <- import_list("Universität/Bachelor/output.xlsx", rbind = TRUE)
#Taking out descriptions not needed for analysis
# delete headings
nzcfta_frame=nzcfta[-c(1,2,3,14,15)]
#rename columns for future cleaning
colnames(nzcfta_frame)[2]="test" 
#deleting NA rows
dat = nzcfta_frame[rowSums(is.na(nzcfta_frame)) != ncol(nzcfta_frame), ]
#deleting Baseyear-heading rows
dat1<-subset(dat, test!="2008")
#filling in zeros
dat1[dat1 == "free"] <- 0 
dat1[is.na(dat1)] <- 0
nzcfta_frame = dat1
#Renaming columns
colnames(nzcfta_frame)[1]="Baserate"
colnames(nzcfta_frame)[2]="Y + 1"
colnames(nzcfta_frame)[3]="Y + 2"
colnames(nzcfta_frame)[4]="Y + 3"
colnames(nzcfta_frame)[5]="Y + 4"
colnames(nzcfta_frame)[6]="Y + 5"


#plotting
nzcfta_frame=sapply(nzcfta_frame, as.numeric)
nzcfta_frame=as.data.frame(nzcfta_frame)
NZCFTA <- boxplot(nzcfta_frame$Baserate, nzcfta_frame$`Y + 1`, nzcfta_frame$`Y + 2`, nzcfta_frame$`Y + 3`, nzcfta_frame$`Y + 4`, nzcfta_frame$`Y + 5`,
                  names = c("Baserate", "Y + 1","Y + 2","Y + 3", "Y + 4", "Y + 5"), ylab = "Tariffs in %", xlab = "Tariff development from baseyear until 5th year of implementation",
                  main = "NZCFTA")

# ChAFTA
# importing the required library
library(readxl)
#fta einlesen
chafta_schedule_tariff <- read_excel("~/Universität/Bachelor/chafta-explanatory-schedule-of-chinese-tariff-commitments-non-official(1).xlsx")
chafta_frame <- chafta_schedule_tariff
#prepare data
chafta_frame=chafta_frame[-c(1,2,3),]
chafta_frame=chafta_frame[-c(1,2,4)]
cdata = chafta_frame[rowSums(is.na(chafta_frame)) != ncol(chafta_frame), ]
cdata = as.data.frame(cdata)
cdata[is.na(cdata)] <- 0
chafta_frame =cdata
#rename columns
colnames(chafta_frame)[1]="Baserate"
colnames(chafta_frame)[2]="Y + 1"
colnames(chafta_frame)[3]="Y + 2"
colnames(chafta_frame)[4]="Y + 3"
colnames(chafta_frame)[5]="Y + 4"
colnames(chafta_frame)[6]="Y + 5"
#plotting
chafta_frame=sapply(chafta_frame, as.numeric)
chafta_frame=as.data.frame(chafta_frame)
ChAFTA <- boxplot(chafta_frame$Baserate, chafta_frame$`Y + 1`, chafta_frame$`Y + 2`, chafta_frame$`Y + 3`, chafta_frame$`Y + 4`,chafta_frame$`Y + 5`,
                  names = c("Baserate", "Y + 1","Y + 2","Y + 3", "Y + 4", "Y + 5"), ylab = "Tariffs in %", xlab = "Tariff development from baseyear until 5th year of implementation",
                  main = "ChAFTA")


#RCEP
#importing required library
library(readxl)
# RCEP ASEAN
#fta einlesen
rcep_asean <- read_excel("~/Universität/Bachelor/recp-schedule-of-china-for-asean.xls")
ra_frame <- rcep_asean
#prepare data
ra_frame = ra_frame[-c(1,2,3),]
ra_frame = ra_frame[-c(1,2)]
ra_frame=ra_frame[rowSums(is.na(ra_frame)) != ncol(ra_frame), ] 
#rename columns
colnames(ra_frame)[1]="Baserate"
colnames(ra_frame)[2]="Y + 1"
colnames(ra_frame)[3]="Y + 2"
colnames(ra_frame)[4]="Y + 3"
colnames(ra_frame)[5]="Y + 4"
colnames(ra_frame)[6]="Y + 5"
ra_frame=sapply(ra_frame, as.numeric)
ra_frame=as.data.frame(ra_frame)
#RCEP Australia
#fta einlesen
rcep_australia <- read_excel("~/Universität/Bachelor/recp-schedule-of-china-for-australia.xls", 
                             col_names = FALSE)
raus_frame <- rcep_australia
#prepare data
raus_frame=raus_frame[-c(1,2,3,4),]
raus_frame=raus_frame[-c(1,2)]
raus_frame=raus_frame[rowSums(is.na(raus_frame)) != ncol(raus_frame), ] 
#rename columns
colnames(raus_frame)[1]="Baserate"
colnames(raus_frame)[2]="Y + 1"
colnames(raus_frame)[3]="Y + 2"
colnames(raus_frame)[4]="Y + 3"
colnames(raus_frame)[5]="Y + 4"
colnames(raus_frame)[6]="Y + 5"
raus_frame=sapply(raus_frame, as.numeric)
raus_frame=as.data.frame(raus_frame)

#RCEP Japan
#fta einlesen
rcep_japan <- read_excel("~/Universität/Bachelor/recp-schedule-of-china-for-japan.xls")
rja_frame <- rcep_japan
#prepare data
rja_frame=rja_frame[-c(1,2,3),]
rja_frame=rja_frame[-c(1,2)]
rja_frame=rja_frame[rowSums(is.na(rja_frame)) != ncol(rja_frame), ] 
#rename columns
colnames(rja_frame)[1]="Baserate"
colnames(rja_frame)[2]="Y + 1"
colnames(rja_frame)[3]="Y + 2"
colnames(rja_frame)[4]="Y + 3"
colnames(rja_frame)[5]="Y + 4"
colnames(rja_frame)[6]="Y + 5"
rja_frame=sapply(rja_frame, as.numeric)
rja_frame=as.data.frame(rja_frame)

#RCEP Korea
#fta einlesen
rcep_korea <- read_excel("~/Universität/Bachelor/recp-schedule-of-china-for-korea.xls")
rko_frame <- rcep_korea
#prepare data
rko_frame=rko_frame[-c(1,2,3),]
rko_frame=rko_frame[-c(1,2)]
rko_frame=rko_frame[rowSums(is.na(rko_frame)) != ncol(rko_frame), ] 
#rename columns
colnames(rko_frame)[1]="Baserate"
colnames(rko_frame)[2]="Y + 1"
colnames(rko_frame)[3]="Y + 2"
colnames(rko_frame)[4]="Y + 3"
colnames(rko_frame)[5]="Y + 4"
colnames(rko_frame)[6]="Y + 5"
rko_frame=sapply(rko_frame, as.numeric)
rko_frame=as.data.frame(rko_frame)

#RCEP New Zealand
#fta einlesen
rcep_newzealand <- recp_schedule_of_china_for_nz <- read_excel("~/Universität/Bachelor/recp-schedule-of-china-for-nz.xls")
rnz_frame <- rcep_newzealand

#prepare data
rnz_frame=rnz_frame[-c(1,2,3),]
rnz_frame=rnz_frame[-c(1,2)]
rnz_frame=rnz_frame[rowSums(is.na(rnz_frame)) != ncol(rnz_frame), ] 
#rename columns
colnames(rnz_frame)[1]="Baserate"
colnames(rnz_frame)[2]="Y + 1"
colnames(rnz_frame)[3]="Y + 2"
colnames(rnz_frame)[4]="Y + 3"
colnames(rnz_frame)[5]="Y + 4"
colnames(rnz_frame)[6]="Y + 5"
rnz_frame=sapply(rnz_frame, as.numeric)
rnz_frame=as.data.frame(rnz_frame)

#Boxplots
par(mfrow = c(2,3))
#importing required library
RCEP_ASEAN <- boxplot(ra_frame$Baserate*100, ra_frame$`Y + 1`*100, ra_frame$`Y + 2`*100, ra_frame$`Y + 3`*100, ra_frame$`Y + 4`*100,ra_frame$`Y + 5`*100,
                      names = c("Baserate", "Y + 1","Y + 2","Y + 3", "Y + 4", "Y + 5"), ylab = "Tariffs in %", 
                      main = "RCEP ASEAN")
RCEP_AUSTRALIA <- boxplot(raus_frame$Baserate*100, raus_frame$`Y + 1`*100, raus_frame$`Y + 2`*100, raus_frame$`Y + 3`*100, raus_frame$`Y + 4`*100,raus_frame$`Y + 5`*100,
                          names = c("Baserate", "Y + 1","Y + 2","Y + 3", "Y + 4", "Y + 5"), ylab = "Tariffs in %", 
                          main = "RCEP AUSTRLIA")
RCEP_JAPAN <- boxplot(rja_frame$Baserate* 100 , rja_frame$`Y + 1`* 100 , rja_frame$`Y + 2`* 100 , rja_frame$`Y + 3`* 100 , rja_frame$`Y + 4`* 100 ,rja_frame$`Y + 5`* 100,
                      names = c("Baserate", "Y + 1","Y + 2","Y + 3", "Y + 4", "Y + 5"), ylab = "Tariffs in %", 
                      main = "RCEP JAPAN")
RCEP_KOREA <- boxplot(rko_frame$Baserate* 100 , rko_frame$`Y + 1`* 100 , rko_frame$`Y + 2`* 100 , rko_frame$`Y + 3`* 100 , rko_frame$`Y + 4`* 100 ,rko_frame$`Y + 5`* 100 ,
                      names = c("Baserate", "Y + 1","Y + 2","Y + 3", "Y + 4", "Y + 5"), ylab = "Tariffs in %", 
                      main = "RCEP KOREA")
RCEP_NEWZEALAND <- boxplot(rnz_frame$Baserate* 100 , rnz_frame$`Y + 1`* 100 , rnz_frame$`Y + 2`* 100 , rnz_frame$`Y + 3`* 100 , rnz_frame$`Y + 4`* 100 ,rnz_frame$`Y + 5`* 100 ,
                           names = c("Baserate", "Y + 1","Y + 2","Y + 3", "Y + 4", "Y + 5"), ylab = "Tariffs in %", 
                           main = "RCEP NEW ZEALAND")

#Main Table
#NZCFTA
nz1 = mean(nzcfta_frame[,1],na.rm = TRUE)
nz2 = mean(nzcfta_frame[,2],na.rm = TRUE)
nz3 = mean(nzcfta_frame[,3],na.rm = TRUE)
nz4 = mean(nzcfta_frame[,4],na.rm = TRUE)
nz5 = mean(nzcfta_frame[,5],na.rm = TRUE)
nz6 = mean(nzcfta_frame[,6],na.rm = TRUE)
NROW = rbind(nz1, nz2, nz3, nz4,nz5, nz6)

#ChAFTA
c1 = mean(chafta_frame[,1],na.rm = TRUE)/100
c2 = mean(chafta_frame[,2],na.rm = TRUE)/100
c3 = mean(chafta_frame[,3],na.rm = TRUE)/100
c4 = mean(chafta_frame[,4],na.rm = TRUE)/100
c5 = mean(chafta_frame[,5],na.rm = TRUE)/100
c6 = mean(chafta_frame[,6],na.rm = TRUE)/100
CROW = rbind(c1, c2, c3, c4, c5, c6)


#RCEP
#RCEP ASEAN
as1 = mean(ra_frame[,1],na.rm = TRUE)
as2 = mean(ra_frame[,2],na.rm = TRUE)
as3 = mean(ra_frame[,3],na.rm = TRUE)
as4 = mean(ra_frame[,4],na.rm = TRUE)
as5 = mean(ra_frame[,5],na.rm = TRUE)
as6 = mean(ra_frame[,6],na.rm = TRUE)
ASROW = rbind(as1, as2, as3, as4, as5, as6)
#RCEP AUSTRLIA
au1 = mean(raus_frame[,1],na.rm = TRUE)
au2 = mean(raus_frame[,2],na.rm = TRUE)
au3 = mean(raus_frame[,3],na.rm = TRUE)
au4 = mean(raus_frame[,4],na.rm = TRUE)
au5 = mean(raus_frame[,5],na.rm = TRUE)
au6 = mean(raus_frame[,6],na.rm = TRUE)
AUROW = rbind(au1, au2, au3, au4, au5, au6)
#RCEP JAPAN
ja1 = mean(rja_frame[,1],na.rm = TRUE)
ja2 = mean(rja_frame[,2],na.rm = TRUE)
ja3 = mean(rja_frame[,3],na.rm = TRUE)
ja4 = mean(rja_frame[,4],na.rm = TRUE)
ja5 = mean(rja_frame[,5],na.rm = TRUE)
ja6 = mean(rja_frame[,6],na.rm = TRUE)
JAROW = rbind(ja1, ja2, ja3, ja4, ja5, ja6)
#RCEP KOREA
ko1 = mean(rko_frame[,1],na.rm = TRUE)
ko2 = mean(rko_frame[,2],na.rm = TRUE)
ko3 = mean(rko_frame[,3],na.rm = TRUE)
ko4 = mean(rko_frame[,4],na.rm = TRUE)
ko5 = mean(rko_frame[,5],na.rm = TRUE)
ko6 = mean(rko_frame[,6],na.rm = TRUE)
KOROW = rbind(ko1, ko2, ko3, ko4, ko5, ko6)
#RCEP NEW ZEALAND
n1 = mean(rnz_frame[,1],na.rm = TRUE)
n2 = mean(rnz_frame[,2],na.rm = TRUE)
n3 = mean(rnz_frame[,3],na.rm = TRUE)
n4 = mean(rnz_frame[,4],na.rm = TRUE)
n5 = mean(rnz_frame[,5],na.rm = TRUE)
n6 = mean(rnz_frame[,6],na.rm = TRUE)
NROW = rbind(n1, n2, n3, n4, n5, n6)

#Joining rows to data frame
tabledat = cbind(NROW, CROW, ASROW, AUROW, JAROW, KOROW, NROW)
tabledat = data.frame(tabledat)
tabledat = as.data.frame(t(tabledat))
colnames(tabledat)[1]="Baserate"
colnames(tabledat)[2]="Y + 1"
colnames(tabledat)[3]="Y + 2"
colnames(tabledat)[4]="Y + 3"
colnames(tabledat)[5]="Y + 4"
colnames(tabledat)[6]="Y + 5"
rownames(tabledat)[1]="NZCFTA"
rownames(tabledat)[2]="ChAFTA"
rownames(tabledat)[3]="RCEP ASEAN"
rownames(tabledat)[4]="RCEP AUSTRALIA"
rownames(tabledat)[5]="RCEP JAPAN"
rownames(tabledat)[6]="RCEP KOREA"
rownames(tabledat)[7]="RCEP NEW ZEALAND"


#Compute growth rates
tabledat = as.data.frame(t(tabledat))
#NZCFTA
GrowNZ <- as.data.frame(tabledat[1])
GrowNZ$Growth = with(GrowNZ, ave(tabledat[,1],  
                                 FUN=function(x) c(NA, diff(x)/x[-length(x)]) ))
View(GrowNZ)
AverageNZ = mean(GrowNZ$Growth, na.rm=TRUE)

#ChAFTA
GrowC <- as.data.frame(tabledat[2])
GrowC$Growth[1] = NA
GrowC$Growth[2] = tabledat$ChAFTA[2]/tabledat$ChAFTA[1]-1
GrowC$Growth[3] = tabledat$ChAFTA[3]/tabledat$ChAFTA[2]-1
GrowC$Growth[4] = tabledat$ChAFTA[4]/tabledat$ChAFTA[3]-1
GrowC$Growth[5] = tabledat$ChAFTA[5]/tabledat$ChAFTA[4]-1
GrowC$Growth[6] = tabledat$ChAFTA[6]/tabledat$ChAFTA[5]-1
View(GrowC)
AverageC = mean(GrowC$Growth, na.rm=TRUE)

#RCEP ASEAN
GrowAS <- as.data.frame(tabledat[3])
GrowAS$Growth[1] = NA
GrowAS$Growth[2] = tabledat$`RCEP ASEAN`[2]/tabledat$`RCEP ASEAN`[1]-1
GrowAS$Growth[3] = tabledat$`RCEP ASEAN`[3]/tabledat$`RCEP ASEAN`[2]-1
GrowAS$Growth[4] = tabledat$`RCEP ASEAN`[4]/tabledat$`RCEP ASEAN`[3]-1
GrowAS$Growth[5] = tabledat$`RCEP ASEAN`[5]/tabledat$`RCEP ASEAN`[4]-1
GrowAS$Growth[6] = tabledat$`RCEP ASEAN`[6]/tabledat$`RCEP ASEAN`[5]-1

View(GrowAS)
AverageAS = mean(GrowAS$Growth, na.rm=TRUE)

#RCEP AUSTRALIA
GrowAU <- as.data.frame(tabledat[4])
GrowAU$Growth[1] = NA
GrowAU$Growth[2] = tabledat$`RCEP AUSTRALIA`[2]/tabledat$`RCEP AUSTRALIA`[1]-1
GrowAU$Growth[3] = tabledat$`RCEP AUSTRALIA`[3]/tabledat$`RCEP AUSTRALIA`[2]-1
GrowAU$Growth[4] = tabledat$`RCEP AUSTRALIA`[4]/tabledat$`RCEP AUSTRALIA`[3]-1
GrowAU$Growth[5] = tabledat$`RCEP AUSTRALIA`[5]/tabledat$`RCEP AUSTRALIA`[4]-1
GrowAU$Growth[6] = tabledat$`RCEP AUSTRALIA`[6]/tabledat$`RCEP AUSTRALIA`[5]-1
View(GrowAU)
AverageAU = mean(GrowAU$Growth, na.rm=TRUE)

#RCEP JAPAN
GrowJA <- as.data.frame(tabledat[5])
GrowJA$Growth[1] = NA
GrowJA$Growth[2] = tabledat$`RCEP JAPAN`[2]/tabledat$`RCEP JAPAN`[1]-1
GrowJA$Growth[3] = tabledat$`RCEP JAPAN`[3]/tabledat$`RCEP JAPAN`[2]-1
GrowJA$Growth[4] = tabledat$`RCEP JAPAN`[4]/tabledat$`RCEP JAPAN`[3]-1
GrowJA$Growth[5] = tabledat$`RCEP JAPAN`[5]/tabledat$`RCEP JAPAN`[4]-1
GrowJA$Growth[6] = tabledat$`RCEP JAPAN`[6]/tabledat$`RCEP JAPAN`[5]-1
View(GrowJA)
AverageJA = mean(GrowJA$Growth, na.rm=TRUE)

#RCEP KOREA
GrowKO <- as.data.frame(tabledat[6])
GrowKO$Growth[1] = NA
GrowKO$Growth[2] = tabledat$`RCEP KOREA`[2]/tabledat$`RCEP KOREA`[1]-1
GrowKO$Growth[3] = tabledat$`RCEP KOREA`[3]/tabledat$`RCEP KOREA`[2]-1
GrowKO$Growth[4] = tabledat$`RCEP KOREA`[4]/tabledat$`RCEP KOREA`[3]-1
GrowKO$Growth[5] = tabledat$`RCEP KOREA`[5]/tabledat$`RCEP KOREA`[4]-1
GrowKO$Growth[6] = tabledat$`RCEP KOREA`[6]/tabledat$`RCEP KOREA`[5]-1
View(GrowKO)
AverageKO = mean(GrowKO$Growth, na.rm=TRUE)

#NEW ZEALAND
GrowN <- as.data.frame(tabledat[7])
GrowN$Growth[1] = NA
GrowN$Growth[2] = tabledat$`RCEP NEW ZEALAND`[2]/tabledat$`RCEP NEW ZEALAND`[1]-1
GrowN$Growth[3] = tabledat$`RCEP NEW ZEALAND`[3]/tabledat$`RCEP NEW ZEALAND`[2]-1
GrowN$Growth[4] = tabledat$`RCEP NEW ZEALAND`[4]/tabledat$`RCEP NEW ZEALAND`[3]-1
GrowN$Growth[5] = tabledat$`RCEP NEW ZEALAND`[5]/tabledat$`RCEP NEW ZEALAND`[4]-1
GrowN$Growth[6] = tabledat$`RCEP NEW ZEALAND`[6]/tabledat$`RCEP NEW ZEALAND`[5]-1
View(GrowN)
AverageN = mean(GrowN$Growth, na.rm=TRUE)

tabledat = as.data.frame(t(tabledat))
tabledat$AverageGrowth = c (AverageNZ, AverageC,AverageAS,AverageAU,AverageJA,AverageKO,AverageN)
print(tabledat)

