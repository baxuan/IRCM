library(plyr)
library(dplyr)
library(e1071)
library(psych)
library(ggplot2)

acm_data <- 
  read.table("ACM_bilateral_all.csv",sep=",",header = T)

mag_data <- 
  read.table("MAG_bilateral_all.csv",sep=" ",header = T)

### ranking analysis

acm_data.c1 <- plyr::count(acm_data,Year ~ Country1)
colnames(acm_data.c1)<-c("year","country","count")
acm_data.c1$year<-as.character(acm_data.c1$year)
acm_data.c1$country<-as.character(acm_data.c1$country)
acm_data.c2 <- plyr::count(acm_data,Year ~ Country2)
colnames(acm_data.c2)<-c("year","country","count")
acm_data.c2$year<-as.character(acm_data.c2$year)
acm_data.c2$country<-as.character(acm_data.c2$country)

acm_data.annual <- aggregate(count ~ year + country,rbind(acm_data.c1,acm_data.c2),sum)

years <- c(2000:2017)

acm_data.annual_ranking <- list()

for(yea in years){
  nex <- acm_data.annual[which(acm_data.annual$year==yea),]
  acm_data.annual_ranking[[as.character(yea)]]<-nex[with(nex,order(-count)),]
}

mag_data.c1 <- plyr::count(mag_data,Year ~ Country1)
colnames(mag_data.c1)<-c("year","country","count")
mag_data.c1$year<-as.character(mag_data.c1$year)
mag_data.c1$country<-as.character(mag_data.c1$country)
mag_data.c2 <- plyr::count(mag_data,Year ~ Country2)
colnames(mag_data.c2)<-c("year","country","count")
mag_data.c2$year<-as.character(mag_data.c2$year)
mag_data.c2$country<-as.character(mag_data.c2$country)

mag_data.annual <- aggregate(count ~ year + country,rbind(mag_data.c1,mag_data.c2),sum)

mag_data.annual_ranking <- list()

for(yea in years){
  nex <- mag_data.annual[which(mag_data.annual$year==yea),]
  mag_data.annual_ranking[[as.character(yea)]]<-nex[with(nex,order(-count)),]
}


### raw distance

distances <- list()

for(i in 1:length(mag_data.annual_ranking)){
  acm_10 <- head(acm_data.annual_ranking[[i]]$country,15)
  mag_10 <- head(mag_data.annual_ranking[[i]]$country,15)
  
  distances[[length(distances)+1]] <- e1071::hamming.distance(acm_10,mag_10)
}

### correlate country rank kendall and spearman


uniqueMagCountries <- unique(mag_data.c1$country)
uniqueAcmCountries <- unique(acm_data.c1$country)

uniqueCountries <- sort(uniqueAcmCountries[which(uniqueAcmCountries %in% uniqueMagCountries)])

countryRanksAcm <- list()

for(i in 1:length(acm_data.annual_ranking)){
  for(j in uniqueCountries){
    if(length(countryRanksAcm[[j]]>0)){
      countryRanksAcm[[j]]<-c(countryRanksAcm[[j]],which(acm_data.annual_ranking[[i]]$country==j))
    } else {
      countryRanksAcm[[j]]<-c(which(acm_data.annual_ranking[[i]]$country==j))
    }
  }
}

countryRanksMag <- list()

for(i in 1:length(mag_data.annual_ranking)){
  for(j in uniqueCountries){
    if(length(countryRanksMag[[j]]>0)){
      countryRanksMag[[j]]<-c(countryRanksMag[[j]],which(mag_data.annual_ranking[[i]]$country==j))
    } else {
      countryRanksMag[[j]]<-c(which(mag_data.annual_ranking[[i]]$country==j))
    }
  }
}

lapply(countryRanksMag,function(x){length(x)})

magCompleteData <- which(unlist(lapply(countryRanksMag,function(x){length(x)}))==18)

acmCompleteData <- which(unlist(lapply(countryRanksAcm,function(x){length(x)}))==18)

finalCountryList <- sort(names(magCompleteData[which(magCompleteData %in% acmCompleteData)]))

correlationsKen <- list()
correlationsSpear <- list()
meanMag <- list()
sdMag <- list()
meanAcm <- list()
sdAcm <- list()
hamDistance <- list()

finalData <- data.frame(country=character(length(finalCountryList)),
                        kendall=numeric(length(finalCountryList)),
                        spearman=numeric(length(finalCountryList)),
                        hammingDistance=numeric(length(finalCountryList)),
                        meanMAG=numeric(length(finalCountryList)),
                        sdMAG=numeric(length(finalCountryList)),
                        meanACM=numeric(length(finalCountryList)),
                        sdACM=numeric(length(finalCountryList)),stringsAsFactors = F)

index<-0
for(k in finalCountryList){
  index<-index+1
  meanMag[[k]] <- mean(countryRanksMag[[k]])
  meanAcm[[k]] <- mean(countryRanksAcm[[k]])
  sdMag[[k]] <- sd(countryRanksMag[[k]])
  sdAcm[[k]] <- sd(countryRanksAcm[[k]])
  hamDistance[[k]] <- e1071::hamming.distance(countryRanksMag[[k]],countryRanksAcm[[k]])
  
  finalData$country[index] <- k
  finalData$kendall[index] <- psych::corr.test(countryRanksMag[[k]],countryRanksAcm[[k]], method = "kendall")$ci$r
  finalData$spearman[index] <- psych::corr.test(countryRanksMag[[k]],countryRanksAcm[[k]], method = "spearman")$ci$r
  finalData$kendall_l[index] <- psych::corr.test(countryRanksMag[[k]],countryRanksAcm[[k]], method = "kendall")$ci$lower
  finalData$kendall_u[index] <- psych::corr.test(countryRanksMag[[k]],countryRanksAcm[[k]], method = "kendall")$ci$upper
  finalData$spearman_l[index] <- psych::corr.test(countryRanksMag[[k]],countryRanksAcm[[k]], method = "spearman")$ci$lower
  finalData$spearman_u[index] <- psych::corr.test(countryRanksMag[[k]],countryRanksAcm[[k]], method = "spearman")$ci$upper
  finalData$hammingDistance[index] <- hamDistance[[k]]
  finalData$meanMAG[index] <- meanMag[[k]]
  finalData$sdMAG[index] <- sdMag[[k]]
  finalData$meanACM[index] <- meanAcm[[k]]
  finalData$sdACM[index] <- sdAcm[[k]]
}

write_csv(finalData,"/Users/mlr/OneDrive - Victoria University of Wellington - STAFF/Data/CountryRankingEvaluation.csv",col_names = T)
write.table(finalData,"CountryRankingEvaluation.csv", sep=',', row.names=FALSE, col.names = TRUE)

plotData <- data.frame(country=character(2*(nrow(finalData)-1)),variable=character(2*(nrow(finalData)-1)),value=numeric(2*(nrow(finalData)-1)),stringsAsFactors = F)

for(i in seq(1,(2*(nrow(finalData)-1))-1,by = 2)){
  plotData[i,1] <- finalData$country[i%/%2 + 1]
  plotData[i,2] <- "kendall"
  plotData[i,3] <- finalData$kendall[i%/%2 + 1]
  plotData[i+1,1] <- finalData$country[i%/%2 + 1]
  plotData[i+1,2] <- "spearman"
  plotData[i+1,3] <- finalData$spearman[i%/%2 + 1]
}

colourCount = nrow(plotData)/2

# Grouped
ggplot(plotData, aes(fill=country, y=value, x=variable)) + 
  geom_bar(position="dodge", stat="identity") +
  facet_wrap(~country) 
#scale_fill_brewer()
#scale_fill_brewer(palette = "Set1")

plotData <- as.data.frame(cbind(finalData$country,finalData$spearman),stringsAsFactors = F)
colnames(plotData)<-c("country","correlation")
plotData$correlation <- as.numeric(plotData$correlation)
plotData <- plotData[-nrow(plotData),]
plotData$correlation <- round(plotData$correlation,3)

ggplot(plotData, aes(y=correlation, x=country)) + 
  geom_bar(position="dodge", stat="identity") +
  theme_classic() + theme(axis.text.x = element_text(angle = 45,hjust = 1))

finalData %>%
ggplot() +
  aes(y=spearman, x=country) + 
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin = spearman_l, ymax = spearman_u)) +
  theme_classic() + theme(axis.text.x = element_text(angle = 45,hjust = 1))

