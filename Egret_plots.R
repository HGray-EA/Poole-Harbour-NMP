
# EGRET plots

library(tidyverse)
library(EGRET)
library(zoo)
library(EGRETci)

shortName="River Stour at Iford Bridge"
paramShortName="Total oxidised nitrogen"
staAbbrev="50370169"
constitAbbrev="TON"
drainSqKm=323.7
param.units="mg/l"

#read water quality data from csv file
wq.data<-read.csv("C:/Users/hg000051/Downloads/IfordBridgeTON.csv")%>%
  mutate(Date=dmy(Date))

plot(wq.data$Date,wq.data$Result)

write.csv(wq.data,"C:/Users/hg000051/Downloads/wq.data.csv",row.names = FALSE)
Sample<-readUserSample("C:/Users/hg000051/Downloads/","wq.data.csv")
Sample<-removeDuplicates(Sample)

#read flow data from csv file
flow.data<-read.csv("C:/Users/hg000051/Downloads/ThroopQ.csv")%>%
  mutate(Date=dmy(Date))%>%
  mutate(Qdaily=na.approx(Qdaily))

plot(flow.data$Date,flow.data$Qdaily)

write.csv(flow.data,"C:/Users/hg000051/Downloads/flow.data.csv",row.names = FALSE)
Daily<-readUserDaily("C:/Users/hg000051/Downloads/","flow.data.csv",qUnit=2)

#make INFO data frame
INFO<-data.frame(param.units,shortName,paramShortName,staAbbrev,constitAbbrev,drainSqKm)

#make eList
eList<-mergeReport(INFO,Daily,Sample)
eList<-setPA(eList,paStart=1,paLong=12) #calendar year

#basic plots (not WRTDS)
plotConcTime(eList)
plotConcQ(eList)
plotFluxQ(eList)
boxConcMonth(eList)
boxQTwice(eList,logScale=TRUE)

#WRTDS
eList<-modelEstimation(eList)
plotConcHist(eList,1970,2025)
returnDF<-tableResults(eList)
plotFluxHist(eList,1965,2025)
plotConcTimeDaily(eList,1975,2025)
plotContours(eList,yearStart=1965,yearEnd=2025)
plotConcQSmooth(eList,"1980-08-01","2000-08-01","2025-01-01",0.5,25,printValues=TRUE,logScale=TRUE,legendLeft=1,legendTop=0.25,printTitle=TRUE)
boxResidMonth(eList)
boxConcThree(eList)
multiPlotDataOverview(eList)

#save output
saveResults("C:/Users/hg000051/Downloads/PH",eList)
write.csv(eList$Daily,"C:/Users/hg000051/Downloads/PH/Daily.csv")
write.csv(eList$Sample,"C:/Users/hg000051/Downloads/PH/Sample.csv")
write.csv(returnDF,"C:/Users/hg000051/Downloads/PH/returnDF_IfordBridgeTON.csv")

#annual confidence intervals ***takes 1hr plus to run***
CIAnnualResults<-ciCalculations(eList)
plotConcHistBoot(eList,CIAnnualResults)
plotFluxHistBoot(eList,CIAnnualResults)

#Interactive function to set up trend analysis
caseSetUp<-trendSetUp(eList)
eBoot<-wBT(eList,caseSetUp,fileName="outputText.txt")
saveEGRETci(eList,eBoot,caseSetUp)

plotHistogramTrend(eList,eBoot,caseSetUp,flux=FALSE)

#WRTDS kalman
load("Hants Avon at Amesbury.Ortho-P.RData")
eList_K<-WRTDSKalman(eList,rho=0.9,niter=200,seed=060570)
AnnualResults<-setupYears(eList_K$Daily)
plotWRTDSKalman(eList_K)
plotTimeSlice(eList_K,start=2000,end=2022,conc=TRUE)
plotConcHist(eList_K,plotAnnual=FALSE,plotGenConc=TRUE)

annual.means<-eList_K$Daily%>%
  select(DecYear,ConcDay,GenConc)%>%
  mutate(DecYear=as.integer(DecYear))%>%
  group_by(DecYear)%>%
  summarise(WRTDS_mean=mean(ConcDay),K_mean=mean(GenConc))

plot(K_mean~DecYear,data=annual.means,type="l")
