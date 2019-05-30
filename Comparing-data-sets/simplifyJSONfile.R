library(jsonlite)
library(dplyr)

for (i in 0:165)
{  
current_file <-paste("mag_papers_",toString(i),".txt",sep="")
for (j in 1:5){
  starting_j <- 1+(j-1)*10000
  ending_j <- j*10000
  new_file <-paste("mag_papers_simplified_",toString(i),"_",toString(j),".txt",sep="")
  stream_in(textConnection(readLines(current_file)[starting_j:ending_j]),verbose=F) %>% select(id, title, year, authors, fos) %>% stream_out(file(new_file))
}
}
