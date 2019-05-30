library(jsonlite)            	# for opening json files
library(countrycode)        	# for identifying countries' names
library(zipcode)              # for identifying the US's zip codes
library(stringr)            	# for processing strings
library(dplyr)			          # for grouping and summarizing
library(RCurl)			          # for composing general HTTP requests

f1_min <- 0
f1_max <- 165
f2_min <- 1
f2_max <- 5

f1 <- f1_min
f2 <- f2_min 

matched_file <- "matched_collection_all.csv"
matched_data <- read.csv(matched_file, header=TRUE, sep=";")
n_row_matched <- nrow(matched_data)

bilateral_file <- "MAG_bilateral.csv"

p_id <- list()
p_title <- list()
p_year <- numeric() 
p_fos <- list(character())
p_authors_affil <- list()     
p_n_o_Authors <- numeric()  
p_n_o_Affiliations <- numeric() 

flag_all_NA <- logical()

special_string_endings <- c(',','.',' ','(',')',';')

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))}

substrLeft = function(text, num_char) {
  substr(text, 1, num_char)
}

istheUS <- function(name,string){
  data(zipcode)
  if (is.na(name)) {return(FALSE)} else
  {
  if ((length(state.abb[which(state.name == name)])==1)|(name %in% state.abb) | (name=='USA') | (name =='U.S.A') | (name =='U.S.A.') |(substrRight(my_string,13)=='United States'))  
    { 
    return(TRUE)} else
  {
    if (grepl(' ',name))
    {
      last_space_position <- gregexpr(' ',name)
      last_space_position_value <- last_space_position[[1]][length(last_space_position[[1]])]
      z_code <- trimws(substrRight(name, nchar(name)-last_space_position_value))
    }
    else
    {z_code <- name}
    
    zip_code <- clean.zipcodes(z_code)
    if (is.na(match(zip_code,zipcode$zip)))
    {return(FALSE)}
    else
    {
      s_abb <- zipcode$state[match(zip_code,zipcode$zip)]
      city_name <- zipcode$city[match(zip_code,zipcode$zip)]
      state_name <- state.name[grep(s_abb, state.abb)]
      
      if (length(state_name)==0) 
      {(grepl(paste(s_abb,' ',z_code),name)) | grepl(city_name,string)}
      else
      {(grepl(paste(s_abb,' ',z_code),name)) | grepl(city_name,string) | grepl(state_name,string)}
    }
  }
  }
}

istheUK <- function(name){
  UK_list <- c('UK', 'U.K', 'U.K.', 'England', 'ENGLAND', 'Scotland', 'SCOTLAND','Wales', 'WALES', 'Northern Ireland', 'NORTHERN IRELAND')
  if (is.na(name)) {return(FALSE)}
  else {return((name %in% UK_list) | (substrRight(name,15)=='United Kingdoms'))}
}

isChina_Taiwan <- function(name, string){
  if (is.na(string))  { return('None')} 
  else {
    if (((substrRight(name,3)=='ROC') |(substrRight(name,5)=='R.O.C') | (substrRight(name,5)=='R.O.C.')) & (grepl('Taiwan',string))) {return ('TWN')}         
    else {
      if ((substrRight(name,11)=='Taiwan, ROC')|(substrRight(name,13)=='Taiwan, R.O.C') | (substrRight(name,14)=='Taiwan, R.O.C.'))
      { return ('TWN')}
      else { 
        if ((substrRight(name,5)==', PRC') | (substrRight(name,7)==', P.R.C') | (substrRight(name,9)==' PR China')|(substrRight(name,11)==' P.R. China')) {return ('CHN')}
        else {return ('None')} 
      }         
    }      
  }  
}

for (s in f1_min:f1_max) 
{
  for (t in f2_min:f2_max)
  {
    
    n_o_case1 <- 0 #identify countries by countries' name
    n_o_case2 <- 0 #identify the US by state name (US) or state abb (US)
    n_o_case3 <- 0 #identify the UK by component parts' names
    n_o_case4 <- 0 #identify using Wikidata Query Service
    n_o_case5 <- 0 #could not identify
    n_o_case6 <- 0 #errors while in query
    n_o_case7 <- 0 #identify Taiwan
    
    total_papers <- 0
    total_coAuthored_papers <- 0 
    total_coAuthored_papers_International <- 0 
    
    total_coAuthored_Affiliations <- 0 
    total_matched_Affiliations <- 0
    
    Year <- list()
    Country1 <- list()
    Country2 <- list()
    International <- list()
    
    
    current_file <- paste("MAG_filtered_",toString(f1),"_",toString(f2),".txt",sep="")
    new_file <- paste(current_folder,"MAG_bilateral_",toString(f1),"_",toString(f2),".csv",sep="")    
    print(current_file)
    
    j <- 1
    
    con = file(current_file, "r")
    while ( TRUE ) 
    {
      line = readLines(con, n = 1)
      if ( length(line) == 0 ) {
        break
      }
      
      flag_all_NA[j] <- TRUE
      
      paper <- jsonlite::fromJSON(line)
      p_id[j] <- paper$id
      p_title[j] <- paper$title
      p_year[j] <- paper$year
      
      p_fos[j] <- list(paper$fos) 
      
      p_authors_affil[j]<-"" 

      p_n_o_Authors[j] <- length(paper$authors$name)
      
      if (p_n_o_Authors[j]<2) 
      {
        p_n_o_Affiliations[j] <- 0
        p_authors_affil[j] <-"" 
      }
      else
      {
        p_n_o_Affiliations[j] <- length(paper$authors$org)
        
        if (p_n_o_Affiliations[j]==0)
        {
          p_authors_affil[j] <-"" 
        }
        else {
          for (i in 1:p_n_o_Affiliations[j])
          {
            my_string <-paper$authors$org[i]
            
            #some special cases followed
            if (grepl('vcard_country-name=',my_string))
            {
              position <- gregexpr('vcard_country-name=',my_string)
              position_value <- position[[length(position)]][1]
              right_part <- trimws(substrRight(my_string, nchar(my_string)-position_value-18))
              position <- gregexpr(',',right_part)
              position_value <- position[[1]][1]
              country_name <- trimws(substrLeft(right_part, position_value-1))
            }
            else
            {
              if ((grepl('email:',my_string,ignore.case=TRUE)) | (grepl('e-mail:',my_string,ignore.case=TRUE)) | (grepl('emails:',my_string,ignore.case=TRUE)) |(grepl('e-mails:',my_string,ignore.case=TRUE)))
              {
                position_value1 <- 0
                position_value2 <- 0
                position_value3 <- 0
                position_value4 <- 0
                if (grepl('email:',my_string,ignore.case=TRUE)) 
                  {position1 <- gregexpr('email:',my_string,ignore.case=TRUE)
                   position_value1 <- position1[[1]][length(position1[[1]])]}
                if (grepl('e-mail:',my_string,ignore.case=TRUE)) 
                  {position2 <- gregexpr('e-mail:',my_string,ignore.case=TRUE)
                   position_value2 <- position2[[1]][length(position2[[1]])]}
                if (grepl('emails:',my_string,ignore.case=TRUE)) 
                  {position3 <- gregexpr('emails:',my_string,ignore.case=TRUE)
                   position_value3 <- position3[[1]][length(position3[[1]])]}
                if (grepl('e-mails:',my_string,ignore.case=TRUE)) 
                  {position4 <- gregexpr('e-mails:',my_string,ignore.case=TRUE)
                   position_value4 <- position4[[1]][length(position4[[1]])]}
                position_value <- max(position_value1,position_value2,position_value3,position_value4)
                left_part <- trimws(substrLeft(my_string,position_value-1))
                last_char <- substrRight(left_part,1)
                while (last_char %in% special_string_endings)
                {
                  left_part <- substrLeft(left_part, nchar(left_part)-1)
                  last_char <- substrRight(left_part,1)
                }
                position <- gregexpr(',',left_part)
                position_value <- position[[1]][length(position[[1]])]
                country_name <- trimws(substrRight(left_part, nchar(left_part)-position_value))
              }
              else {   
                my_string <- gsub("#TAB#","",my_string) 
                my_string <- gsub(", EU","",my_string) 
                my_string <- gsub("#R#","",my_string) 
                my_string <- gsub("#N#","",my_string) 
                last_char <- substrRight(my_string,1)
                while (last_char %in% special_string_endings)
                {
                  my_string <- substrLeft(my_string, nchar(my_string)-1)
                  last_char <- substrRight(my_string,1)
                }
                
                position <-gregexpr(",",my_string)
                position_value <- position[[1]][length(position[[1]])]
                country_name <- trimws(substrRight(my_string, nchar(my_string)-position_value))
              }
            }
            
            if (!is.na(countrycode(country_name, 'iso3c', 'country.name'))){
              country_code <- as.character(country_name)
            } else { country_code <- countrycode(country_name, 'country.name', 'iso3c')}
            
            if (!is.na(country_code))
            {
              n_o_case1 <- n_o_case1 + 1
            }
            else      	#indentifying special cases
            {
              if (istheUS(country_name,my_string)) #identifying US states
              {
                country_code <- 'USA'
                n_o_case2 <- n_o_case2 + 1
              } else
              {
                if (istheUK(country_name)) #identifying UK component parts
                {
                  country_code <- 'GBR'
                  n_o_case3 <- n_o_case3 + 1 
                }
                else
                {
                  China_Taiwan <- isChina_Taiwan(country_name,my_string) #identifying China or Taiwan
                  if (!(China_Taiwan=='None') ){
                    if (China_Taiwan=='CHN') {country_code <- 'CHN'}
                    if (China_Taiwan=='TWN') {country_code <- 'TWN'}
                    n_o_case7 <- n_o_case7 + 1 }
                  else
                  {
                  my_query <- paste('https://query.wikidata.org/sparql?format=json&query=',RCurl::curlEscape(paste('PREFIX
                                                                                                                   schema: <http://schema.org/> PREFIX wdt:
                                                                                                                   <http://www.wikidata.org/prop/direct/> SELECT ?countryLabel WHERE
                                                                                                                   {<https://en.wikipedia.org/wiki/',gsub(' ','_',gsub("\\|","",gsub("\\\\","",country_name))),'> schema:about
                                                                                                                   ?datalink. ?datalink wdt:P17 ?country. SERVICE wikibase:label {
                                                                                                                   bd:serviceParam wikibase:language "en" .}}',sep='')),sep='')
                  mod2 <- try({
                    country_label <- fromJSON(url(my_query))
                  }, TRUE)
                  
                  if(isTRUE(class(mod2)=="try-error")) 
                  { 
                    n_o_case6 <- n_o_case6 + 1
                  } 
                  else { 
                    if(length(country_label$results$bindings$countryLabel$value) >0){
                      country_name <- country_label$results$bindings$countryLabel$value[1]
                      country_code <- countrycode(country_name, 'country.name', 'iso3c')
                      n_o_case4 <- n_o_case4 + 1 
                    }
                    else
                    {
                      n_o_case5 <- n_o_case5 + 1
                    }
                  } 
                } 
              }
            }
          }
            
            
            if (!is.na(country_code)) { flag_all_NA[j] <- FALSE }
            if (i==1)
            {
              p_authors_affil[j] <- country_code
            }
            else
            {
              p_authors_affil[j] <- paste (p_authors_affil[j],"#",country_code) 
            }
          }
        } 
      }
      p_authors_affil[j] <- str_split( p_authors_affil[j], " # ")
      j <- j+1  
    }
    
    close(con)
    
    full_df <- data.frame(p_year, cbind(p_n_o_Authors), cbind(p_n_o_Affiliations),cbind(p_authors_affil), cbind(p_fos), cbind(flag_all_NA), cbind(p_title), stringsAsFactors=FALSE)
    coauthored_df <- subset(full_df, p_n_o_Authors>1, select = c("p_year","p_n_o_Authors","p_n_o_Affiliations","p_authors_affil","p_fos","flag_all_NA", "p_title"))
    
    total_papers <- nrow(full_df)
    total_coAuthored_papers <- nrow(coauthored_df)
    total_coAuthored_Affiliations <-sum(coauthored_df$p_n_o_Affiliations)
    
    k <- 1
    for (l in 1: nrow(coauthored_df))
    {
      year <- coauthored_df[1]$p_year[l]
      field_o_study <- coauthored_df[5]$p_fos[l]
      titlle <- coauthored_df[7]$p_title[l]
      n_o_Authors <- coauthored_df[2]$p_n_o_Authors[l]
      country_list <- unique(coauthored_df[4]$p_authors_affil[[l]])
      country_list <- country_list[!is.na(country_list)]            
      country_list <- country_list[!country_list %in% "NA"]         
      
      n_o_Countries <- length(country_list) 
      n_o_Countries_notNA <- length(country_list[!is.na(country_list)]) 

      if (n_o_Countries_notNA > 1) 
      {
        total_coAuthored_papers_International <- total_coAuthored_papers_International +1 # added in version4_
        
        matched_flag <- FALSE
        for (q in 1: n_row_matched)
        {
          if (matched_data$Title[q]==titlle)
          {
            matched_flag <- TRUE
          }
        }
        
        if (matched_flag == TRUE)
        {
          total_matched_Affiliations <- total_matched_Affiliations + n_o_Countries 
        }
        else
        {
          for (m in 1:(n_o_Countries-1))
          {
            
            for (n in (m+1):n_o_Countries){
              Year [k] <- year
              Country1[k] <- country_list[m]
              Country2[k] <- country_list[n]
              if ((Country1[[k]]==Country2[[k]])|is.na(Country1[k])|is.na(Country2[k]) | (Country1[[k]]=="NA") | (Country2[[k]]=="NA") ) 
              {
                International[k]=0
              }
              else {
                International[k] <-1
                if (Country1[[k]]>Country2[[k]])
                {
                  temp_Country <- Country1[[k]]
                  Country1[[k]]  <- Country2[[k]]
                  Country2[[k]] <- temp_Country
                }
                write.table(data.frame(file=current_file,Year [k], Country1[k], Country2[k], stringsAsFactors = F), file = paste(current_folder,bilateral_file), sep=",",append = T,row.names = F, col.names = F)
              }
              k <-k+1
            }
          }          
        }
        
      }
    }
    
    relationship_df <- data.frame(cbind(Year), cbind(Country1), cbind(Country2), cbind (International))
    
    bilateral_df <- subset(relationship_df, (International==1)&(Country1!="NA")&(Country2!="NA") )
    
    if (length(bilateral_df[[1]][1][[1]])!=0){
      dd  <-  as.data.frame(matrix(unlist(bilateral_df), nrow=length(unlist(bilateral_df[1]))))
      colnames(dd) <- c("Year", "Country1", "Country2", "Article_count")
      
      IRC <- dd %>% group_by(Year,Country1,Country2) %>% summarise(Article_count=summary(Article_count))
      colnames(IRC) <- c("Year", "Country1", "Country2", "Article_count")
      
      if (t==1) {write.table(IRC, new_file, row.names=FALSE, col.names = TRUE,sep = ",")}
      else {write.table(IRC, new_file, append = TRUE, row.names=FALSE, col.names = FALSE,sep = ",")}
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
