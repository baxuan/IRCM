library(jsonlite)            	# for opening json files
library(countrycode)        	# for identifying countries' names
library(stringr)            		# for processing strings
library(dplyr)			# for grouping and summarizing

f1_min <- 0
f1_max <- 165
f2_min <- 1
f2_max <- 5

f1 <- f1_min
f2 <- f2_min 

IRC <- read.csv("MAG_bilateral_version4_166_1.csv", col.names = c("Year", "Country1", "Country2", "Article_count"))

for (s in f1_min:f1_max) 
{
  for (t in f2_min:f2_max)
  {
    current_file <-paste("MAG_bilateral_version4_",toString(f1),"_",toString(f2),".csv",sep="")
    
    if (file.exists(current_file))
    {
      message <- paste("File: ", current_file)
      print(message)
      IRC_add <- read.csv(current_file, col.names = c("Year", "Country1", "Country2", "Article_count"))
      #IRC <- merge(IRC,IRC_add)  
      IRC <- rbind(IRC,IRC_add)
    }
    else
    {
      message <- paste("Missing file: ", current_file)
      print(message)
    }
    
    f2 <- f2+1
    if (f2>f2_max) 
    {
      f2 <- 1
      f1 <- f1+1
      if (f1>f1_max) {break}
    }
    if ((f1==166) &(f2==2)) {break}
  }
}

IRC_total <- aggregate(IRC$Article_count, by=list(Year=IRC$Year, Country1=IRC$Country1, Country2=IRC$Country2), FUN=sum)
IRC_final <- IRC_total[order(IRC_total[,1],IRC_total[,2],IRC_total[,3]),]
colnames(IRC_final) <- c("Year", "Country1", "Country2", "Article_count")

IRC_file <- "MAG_bilateral_article_count.csv"
write.table(IRC_final, IRC_file, row.names=FALSE, col.names = TRUE)

IRC_by_year <- aggregate(IRC$Article_count, by=list(Year=IRC$Year), FUN=sum)
IRC_year <- IRC_by_year[order(IRC_by_year[,1]),]
write.table(IRC_year, "MAG_bilateral_by_year.csv", row.names=FALSE, col.names = TRUE, sep=",")