library(jsonlite)            	# for opening json files
library(countrycode)        	# for identifying countries' names
library(stringr)            		# for processing strings
library(dplyr)			# for grouping and summarizing

csv_file <- "fos_MAG_ACM.csv"
new_file <- "fos_MAG_ACM_occurences.csv"
MyData <- read.csv(csv_file, header=TRUE, sep=",")

fos_terms <- list()
fos_occurs <- list()

l=1
fos <- as.character(MyData$MAG.s.fields.of.study[1][1])
fos_list <- str_split(fos, " #, ")
fos_terms[1] <- trimws(fos_list[[1]][1])
fos_occurs[1] <- 0

n_row <- nrow(MyData)

for (i in 1:n_row)
{
  fos <- as.character(MyData$MAG.s.fields.of.study[i][1])
  fos_list <- str_split(fos, " #, ")
  for (j in 1:length(fos_list[[1]]))
  {
    fos_string <- trimws(fos_list[[1]][j])    
      if (fos_string %in% fos_terms)
      {
        position <- match(fos_string,fos_terms)
        value <- fos_occurs[[position]][1]
        fos_occurs[position] <-  value +1
      }
      else
      {
        l <- l+1
        fos_terms[l] <- fos_string
        fos_occurs[l] <- 1
      }
  }
}

fos_df <- data.frame(cbind(fos_terms), cbind(fos_occurs), stringsAsFactors=TRUE)
write.csv(fos_df, new_file)
