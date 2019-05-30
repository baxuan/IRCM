library(jsonlite)            	# for opening json files
library(countrycode)        	# for identifying countries' names
library(stringr)            		# for processing strings
library(dplyr)			# for grouping and summarizing

csv_file <- "Papers3.csv"
MyData <- read.csv(csv_file, header=TRUE, sep=",")

n_row <- nrow(MyData)

countrycode_list <- list()
n_o_Authors <- 0
n_o_Affiliations <- 0

title_ACM <- list()
title_MAG <- list()
title_match <- list() 
concept_ACM <- list()
fos_MAG <- list() 

m_min <- 0
m_max <- 166
n_min <- 1
n_max <- 5

m <-m_min
n <- n_min 

for (s in m_min:m_max) {
  for (t in n_min:n_max){
    message=paste('s: ',toString(s),', t: ',toString(t),', m: ',toString(m),', n: ',toString(n))
    print(message)
    
    current_file <-paste("MAG_filtered_",toString(m),"_",toString(n),".txt",sep="")
    matched_file <-paste("matched_collection_",toString(m),"_",toString(n),".csv",sep="")
    k <- 1 #order of record in fos_file

    con = file(current_file, "r")
    flag_all_NA <- TRUE
    
    while ( TRUE ) {
      line = readLines(con, n = 1)
      if ( length(line) == 0 ) {
        break
      }
      paper <- jsonlite::fromJSON(line)
      ptitle_MAG <- paper$title

      n_o_Authors <- length(paper$authors$name)
      if (n_o_Authors>1)
      {
        n_o_Affiliations <- length(paper$authors$org)
        if (n_o_Affiliations>1)
        {
          for (i in 1:n_o_Affiliations)
          {
            
            my_string <-paper$authors$org[i]
            position <-gregexpr(",",my_string)
            position_value <- position[[1]][length(position[[1]])]
            substrRight <- function(x, n){
              substr(x, nchar(x)-n+1, nchar(x))}
            country_name <- trimws(substrRight(my_string, nchar(my_string)-position_value))
            country_code <- countrycode(country_name, 'country.name', 'iso3c')
            
            if (is.na(country_code))     	#indentifying states in the United States
            {
              if (length(state.abb[which(state.name == country_name)])==1)
              {
                country_name <- 'United States'
                country_code <- countrycode(country_name, 'country.name', 'iso3c')
              }
            }
            if (!is.na(country_code)) { flag_all_NA <- FALSE }
            countrycode_list[i] <- country_code
          }
          
          if (length(unique(countrycode_list[!is.na(countrycode_list)]))>1)
          {
            if(length(which(MyData$paperFullTitle == ptitle_MAG))>0){
              matched_titles <- MyData[which(MyData$paperFullTitle == ptitle_MAG),]
              #print(matched_titles)
              for (l in 1:nrow(matched_titles))
              {
                ptitle_ACM = as.character(matched_titles$paperFullTitle[l])
                pconcept_ACM = as.character(matched_titles$paperConcept[l])
                if (ptitle_MAG == ptitle_ACM)
                { 
                  fos_string <- paper$fos[1]
                  for (q in 2: length(paper$fos))
                  {
                    fos_string <- paste(fos_string, " #, ", paper$fos[s])
                  }
                  #fos_MAG[k] <- fos_string
                  #concept_ACM[k] <- pconcept_ACM
                  write.table(data.frame(fos=fos_string,title=ptitle_MAG,stringsAsFactors = F), file = matched_file, sep=";",append = T,row.names = F, col.names = F)
                  
                  #message = paste("Record: ", k, " with title: ", ptitle_ACM)
                  #print(message)
                  #break
                  
                  k <- k+1
                  
                }
                else
                {
                  if (grepl(ptitle_MAG,ptitle_ACM, fixed=TRUE))
                  { 
                    #title_ACM[k] <- ptitle_ACM
                    #title_MAG[k] <- ptitle_MAG
                    #title_match[k] <- 2 
                    #stop()
                    fos_string <- paper$fos[1]
                    for (q in 2: length(paper$fos))
                    {
                      fos_string <- paste(fos_string, " #, ", paper$fos[s])
                    }
                    #fos_MAG[k] <- fos_string
                    #concept_ACM[k] <- pconcept_ACM
                    write.table(data.frame(fos=fos_string,title=ptitle_MAG,stringsAsFactors = F), file = matched_file, sep=";",append = T,row.names = F, col.names = F)
                    
                    #message = paste("Record: ", k, " with title: ", ptitle_ACM)
                    #print(message)
                    #break
                    
                    k <- k+1
                    
                  }                  
                  else
                  { 
                    if (grepl(ptitle_ACM,ptitle_MAG, fixed=TRUE))
                      fos_string <- paper$fos[1]
                      for (q in 2: length(paper$fos))
                      {
                        fos_string <- paste(fos_string, " #, ", paper$fos[s])
                      }
                      write.table(data.frame(fos=fos_string,title=ptitle_MAG,stringsAsFactors = F), file = matched_file, sep=";",append = T,row.names = F, col.names = F)
                      k <- k+1            
                      
                    }
                  }
                }
              }
            }
          }
        }
      }    
    }
    
    if (k==1)
    {
      write.table(data.frame(fos="",title="",stringsAsFactors = F), file = matched_file, sep=";",append = T,row.names = F, col.names = F)
    }
    close(con)
    
    n <- n+1
    if (n>n_max) 
    {
      n <- 1
      m <- m+1
      if (m>m_max) {break}
    }
    if ((m==166) &(n==2)) {break}
    
  } 
}
