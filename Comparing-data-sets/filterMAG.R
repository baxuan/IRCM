library(jsonlite)            	# for opening json files
library(dplyr)			# for grouping and summarizing
library(readr)

fos_file <- "fos_toplist_occurences.csv"
fos_list <- read.csv(fos_file, header=TRUE, sep=",")

n_fos <- 38

m_min <- 0
m_max <- 165
n_min <- 1
n_max <- 5

m <- m_min
n <- n_min 

for (s in m_min:m_max) 
{
  for (t in n_min:n_max)
  {
    current_file <- paste("mag_papers_simplified_",toString(m),"_",toString(n),".txt",sep="")
    output_file  <- paste("\\MAG\\MAG_filtered_",toString(m),"_",toString(n),".txt",sep="")
    
    print(current_file)
    con = file(current_file, "r")
    while ( TRUE ) 
    {
      line = readLines(con, n = 1)
      if ( length(line) == 0 ) {
        break
      }
      paper <- jsonlite::fromJSON(line)
      pfos <- paper$fos
      ptitle <- paper$title
      
      matched <- FALSE
      for (i in 1:n_fos)
      {
        term <- as.character(fos_list[[1]][i])
        if (term %in% pfos)
        {
          matched <- TRUE
          break
        }
      }
      if (matched == TRUE)
      {
        paper %>% toJSON() %>% write_lines(output_file, append=TRUE)
      }
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