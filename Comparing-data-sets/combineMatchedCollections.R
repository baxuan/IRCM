library(jsonlite)            	# for opening json files
library(countrycode)        	# for identifying countries' names
library(stringr)            		# for processing strings
library(dplyr)			# for grouping and summarizing

matched_all_file <- "matched_collection_all.csv"

m_min <- 0
m_max <- 165
n_min <- 1
n_max <- 5

m <-m_min
n <- n_min 

k<-1
title_list <- list()

for (s in m_min:m_max) {
  for (t in n_min:n_max){
    message=paste('s: ',toString(s),', t: ',toString(t),', m: ',toString(m),', n: ',toString(n))
    print(message)
  
    current_file <-paste("matched_collection_",toString(m),"_",toString(n),".csv",sep="")
    
    print(current_file)
    
    MyData <- read.csv(current_file, header=FALSE, sep=";")
    n_row <- nrow(MyData)
    
    if (n_row>0)
    {
      for (i in 1:n_row)
      {
        title_list[k] <- as.character(MyData[i,2])
        k <-k+1
      }
    }
    n <- n+1
    if (n>n_max) 
    {
      n <- 1
      m <- m+1
      if (m>m_max) {break}
    }
    if ((m==166) &(n==2)) {break}
    
  } #for
}#for

write.csv(t(as.data.frame(title_list)), file = matched_all_file, append = T, row.names = F, col.names = F)

new_data <- t(read.csv(matched_all_file, header=FALSE, sep=","))
