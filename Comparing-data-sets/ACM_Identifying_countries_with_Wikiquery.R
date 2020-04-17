library(jsonlite)             # for opening json files
library(countrycode)          # for identifying countries' names
library(zipcode)              # for identifying the US's zip codes
library(stringr)              # for processing strings
library(dplyr)			          # for grouping and summarizing
library(RCurl)			          # for composing general HTTP requests

ACM_year_country <- "ACM_year_country.csv"

current_file <- "Papers2.csv"
MyData <- read.csv(current_file, header=TRUE, sep=",")
n_row <- nrow(MyData)

total_papers <- 0 
total_n_o_Authors <- 0 
total_n_o_Affiliations <- 0 

total_coAuthored_papers <- 0 
total_coAuthored_Affiliations <- 0  

p_authors_affil <- list()     
p_authors_affil_distint <- list()

Year <- list()
Country1 <- list()
Country2 <- list()
International <- list()

Year_string <- numeric() 
Year_NullNA <- numeric() 
Year_Wiki <- numeric() 
Year_unidentified <- numeric() 

for (y in 1900:2020)
{Year_string[y]<-0
Year_NullNA[y]<-0
Year_Wiki[y]<-0
Year_unidentified[y]<-0
}

flag_all_NA <- logical()

special_string_endings <- c(',','.',' ','(',')',';','|')

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))}

substrLeft = function(text, num_char) {
  substr(text, 1, num_char)
}

affiliation_data_preparation <- function(){
  
  my_string <<- gsub("Univ. of", "University of", my_string)
  my_string <<- gsub("Univ.,", "University,", my_string)
  
  
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
    country_code <- countrycode(country_name, 'country.name', 'iso3c') 
  }
  else
  {
    my_string <<- gsub("#TAB#","",my_string) 
    my_string <<- gsub(", EU","",my_string) 
    my_string <<- gsub("#R#","",my_string) 
    my_string <<- gsub("#N#","",my_string) 
    last_char <- substrRight(my_string,1)
    while (last_char %in% special_string_endings)
    {
      my_string <<- substrLeft(my_string, nchar(my_string)-1)
      last_char <- substrRight(my_string,1)
    }
    pcountry <-gregexpr(",",my_string)
    pcountry_value <- pcountry[[1]][length(pcountry[[1]])]
    
    if (pcountry_value[1]==-1)
    {
      pcountry <-gregexpr(";",my_string)
      pcountry_value <- pcountry[[1]][length(pcountry[[1]])]
    }
    if (pcountry_value[1]==-1)
    {
      pcountry <-gregexpr("\\|",my_string)
      pcountry_value <- pcountry[[1]][length(pcountry[[1]])]
    }
    
    country_name <<- trimws(substrRight(my_string, nchar(my_string)-pcountry_value))
    
    if ((nchar(my_string)-pcountry_value)>0)
    {
      
      my_string_left <- trimws(substrLeft(my_string, pcountry_value-1))     
      pcountry2 <-gregexpr(",",my_string_left)
      pcountry2_value <- pcountry2[[1]][length(pcountry2[[1]])]
      
      if (pcountry2_value==-1)
      {
        pcountry2 <-gregexpr(";",my_string_left)
        pcountry2_value <- pcountry2[[1]][length(pcountry2[[1]])]
      }
      if (pcountry_value[1]==-1)
      {
        pcountry <-gregexpr("\\|",my_string)
        pcountry_value <- pcountry[[1]][length(pcountry[[1]])]
      }
      
      country_name2 <<- trimws(substrRight(my_string_left, nchar(my_string_left)-pcountry2_value))
    }
  }
}

istheUS <- function(name,string){
  data(zipcode)
  if (is.na(name)) {return(FALSE)} else
  {
    if ((length(state.abb[which(state.name == name)])==1)|(name %in% state.abb) | (name=='USA') | (name =='U.S.A') | (name =='U.S.A.') |(substrRight(string,13)=='United States'))  
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
        if (!is.na(match(zip_code,zipcode$zip)))
        {
          s_abb <- zipcode$state[match(zip_code,zipcode$zip)]
          city_name <- zipcode$city[match(zip_code,zipcode$zip)]
          state_name <- state.name[grep(s_abb, state.abb)]
          
          if (length(state_name)==0) 
          {(grepl(paste(s_abb,' ',z_code),name)) | grepl(city_name,string)}
          else
          {(grepl(paste(s_abb,' ',z_code),name)) | grepl(city_name,string) | grepl(state_name,string)}
        }
    
        else
        {
          if (gsub("[ ]","",gsub("[.]","",name)) %in% state.abb)
          {
            s_abb <- gsub("[ ]","",gsub("[.]","",name))
            city_name <- zipcode$city[match(s_abb,zipcode$city)]
            state_name <- state.name[grep(s_abb, state.abb)]
            if (!is.na(city_name))
            {
              grepl(city_name,string) | grepl(state_name,string)
            }
            else
            {
              grepl(state_name,string)
            }
          }
          else
          {
            grepl("Washington,DC",gsub("[.]","",gsub("[ ]","",string)))
          }
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

string_matching <- function(country_name,my_string){
  
  country_code  <- NA 
  
  if (!is.na(countrycode(country_name, 'iso3c', 'country.name'))){
    country_code <- as.character(country_name)
  } else { country_code <- countrycode(country_name, 'country.name', 'iso3c')}
  
  if (!is.na(country_code))
  {
  
  }
  else      	
  {
    if (istheUS(country_name,my_string)) 
    {
      country_code <- 'USA'
    } else
    {
      if (istheUK(country_name)) 
      {
        country_code <- 'GBR'
      }
      else
      {
        China_Taiwan <- isChina_Taiwan(country_name,my_string)
        if (!(China_Taiwan=='None') ){
          if (China_Taiwan=='CHN') {country_code <- 'CHN'}
          if (China_Taiwan=='TWN') {country_code <- 'TWN'}
        }
        else
        {
          country_code <- NA
        } 
      }
    }
  }
  return(country_code)
}

Wikidata_query <- function(j, country_name2, country_name,my_string){
  country_code  <- NA 
  
  my_query <- paste('https://query.wikidata.org/sparql?format=json&query=',RCurl::curlEscape(paste('PREFIX
                                                                                                   schema: <http://schema.org/> PREFIX wdt:
                                                                                                   <http://www.wikidata.org/prop/direct/> SELECT ?countryLabel WHERE
                                                                                                   {<https://en.wikipedia.org/wiki/',gsub(' ','_',gsub("\\|","",gsub("\\\\","",my_string))),'> schema:about
                                                                                                   ?datalink. ?datalink wdt:P17 ?country. SERVICE wikibase:label {
                                                                                                   bd:serviceParam wikibase:language "en" .}}',sep='')),sep='')
  mod2 <- try({
    country_label <- fromJSON(url(my_query))
  }, TRUE)
  
  if(isTRUE(class(mod2)=="try-error")) 
  { 
  country_code  <- NA 
  } 
  else {
    if(length(country_label$results$bindings$countryLabel$value) >0){
      country_name <- country_label$results$bindings$countryLabel$value[1]
      country_code <- countrycode(country_name, 'country.name', 'iso3c')
    else
    {
      university_positions <- gregexpr('university',my_string,ignore.case=TRUE)
      comma_positions <- gregexpr(',',my_string,ignore.case=TRUE)
      university_list <-list()
      entity_list <-list()
      country_possibilities <-list()
      
      country_label_0 <- list () 
      
      k<-1
      if ((university_positions[[1]][1]>-1)&(comma_positions[[1]][1]>-1))
      {
      for (n in 1:length(university_positions[[1]]))
      {
        for (m in 1:length(comma_positions[[1]]))
        {
          if (university_positions[[1]][n]<comma_positions[[1]][m])
          {university_list[k] <- substrLeft(my_string, comma_positions[[1]][m]-university_positions[[1]][n])
          k<-k+1
          break
          }
        }
      }
      }
      entity_list <- university_list
      entity_list[k]<-country_name2
      entity_list[k+1]<-country_name
      
      for (l in 1: length(entity_list))
      {
        my_query_0 <- paste('https://query.wikidata.org/sparql?format=json&query=',RCurl::curlEscape(paste('PREFIX
                                                                                                           schema: <http://schema.org/> PREFIX wdt:
                                                                                                           <http://www.wikidata.org/prop/direct/> SELECT ?countryLabel WHERE
                                                                                                           {<https://en.wikipedia.org/wiki/',gsub(' ','_',gsub("\\|","",gsub("\\\\","",entity_list[l]))),'> schema:about
                                                                                                           ?datalink. ?datalink wdt:P17 ?country. SERVICE wikibase:label {
                                                                                                           bd:serviceParam wikibase:language "en" .}}',sep='')),sep='')
        mod2_0 <- try({
          country_label_0 <- fromJSON(url(my_query_0))
        }, TRUE)
        
        if ((isTRUE(class(mod2_0)=="try-error"))|(length(country_label_0$results$bindings$countryLabel$value)==0))
        {
          country_possibilities[l] <- NA   
        }
        else
        {
          country_possibilities[l] <- country_label_0$results$bindings$countryLabel$value[1]
        }
      }
      
      n_possibilities <-length(country_possibilities)
      
      if ((!is.na(country_possibilities[n_possibilities]))&(country_possibilities[n_possibilities] %in% country_possibilities[1:(n_possibilities-1)])){
        country_name <- as.character(country_possibilities[n_possibilities][[1]])
        country_code <- countrycode(country_name, 'country.name', 'iso3c')
      else
      {
        if ((length(country_possibilities)>2)&(!is.na(country_possibilities[n_possibilities-1]))&(country_possibilities[n_possibilities-1] %in% country_possibilities[1:(n_possibilities-2)])){
          country_name <- as.character(country_possibilities[n_possibilities-1][[1]])
          country_code <- countrycode(country_name, 'country.name', 'iso3c')
        }
        else{
          country_code <- NA
        } 
      }
    }
  }
  
  return (country_code)
}

flag_all_NA <- TRUE

p_n_o_Authors <- 0 
p_n_o_Affiliations <- 0 


for (i in 1:n_row)
{ 
  print(as.character(j))    
  
  year_string <- as.character(MyData$pdate[j][[1]])
  if (grepl("/",year_string)) {year_value <- str_split(year_string, '/')}
  if (grepl("-",year_string)) {year_value <- str_split(year_string, '-')}
  
  p_authors_affil <- NULL
  
  aff_string = as.character(MyData$Affiliation[j][[1]])
  aff_list <- str_split(aff_string, ' #, ')
  
  p_n_o_Authors <- MyData$n_o_Authors[j]  
  p_n_o_Affiliations <- length(aff_list[[1]]) 
  
  total_papers <- total_papers + 1 
  
  if (p_n_o_Authors>0)
  {
    total_n_o_Authors <- total_n_o_Authors + p_n_o_Authors
    if (p_n_o_Authors>1) 
    {
      total_coAuthored_papers <- total_coAuthored_papers + 1
    }
  }
  
  if (p_n_o_Affiliations>0)
  {
    total_n_o_Affiliations <- total_n_o_Affiliations + p_n_o_Affiliations
  }  
  if (p_n_o_Affiliations>1)
  {
    total_coAuthored_Affiliations <- total_coAuthored_Affiliations + p_n_o_Affiliations
  } 
  
  for (i in 1:p_n_o_Affiliations)
  {
    affiliation <- aff_list[[1]][i]
    my_string <- affiliation
    
    country_name <- "" 
    country_name2 <- "" 
    
    if (!is.na(my_string)&(!is.null(my_string))) 
    {
      affiliation_data_preparation ()
    }
    
    if ((is.na(my_string))|(is.null(my_string))|(my_string=="None"))
    {
      Year_NullNA[as.numeric(year_value[[1]][3])] <- Year_NullNA[as.numeric(year_value[[1]][3])] + 1
    }             
    
    else
    {
      country_code <- string_matching(country_name,my_string)
      Wikidata_query_used <- 0
      
      if (!is.na(country_code))
      {
        Year_string[as.numeric(year_value[[1]][3])] <- Year_string[as.numeric(year_value[[1]][3])] + 1
      }
      else 
      {
        country_code <- Wikidata_query(j, country_name2, country_name,my_string)
        Wikidata_query_used <- 1
        
        if (!is.na(country_code)) {            
          Year_Wiki[as.numeric(year_value[[1]][3])] <- Year_Wiki[as.numeric(year_value[[1]][3])] + 1
        }
        else
        {
          write.table(data.frame(year_value[[1]][3], country_code, stringsAsFactors = F), file = ACM_year_country, sep=",",append = T,row.names = F, col.names = F)    
          Year_unidentified[as.numeric(year_value[[1]][3])] <- Year_unidentified[as.numeric(year_value[[1]][3])] + 1
        }
      }
    }  
    } 
  }

Sys.time()

