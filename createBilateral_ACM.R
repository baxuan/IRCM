library(jsonlite)             # for opening json files
library(countrycode)          # for identifying countries' names
library(zipcode)              # for identifying the US's zip codes
library(stringr)              # for processing strings
library(dplyr)			          # for grouping and summarizing
library(RCurl)			          # for composing general HTTP requests

new_file <- "ACM_bilateral.csv"
current_file <- "Papers2.csv"
MyData <- read.csv(current_file, header=TRUE, sep=",")
n_row <- nrow(MyData)

n_o_case1 <- 0 #identify countries by countries' name
n_o_case2 <- 0 #identify the US by state name (US) or state abb (US)
n_o_case3 <- 0 #identify the UK by component parts' names
n_o_case4 <- 0 #identify countries using Wikidata Query Service
n_o_case5 <- 0 #could not identify
n_o_case6 <- 0 #errors while in query
n_o_case7 <- 0 #identify China/Taiwan

total_coAuthored_papers_International <- 0 

total_coAuthored_Affiliations <- 0 

p_authors_affil <- list()     
p_authors_affil_distint <- list()

Year <- list()
Country1 <- list()
Country2 <- list()
International <- list()

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

flag_all_NA <- TRUE

k <-1

for (i in 1:n_row)
{ 
  year_string <- as.character(MyData$pdate[j][[1]])
  if (grepl("/",year_string)) {year_list <- str_split(year_string, '/')}
  if (grepl("-",year_string)) {year_list <- str_split(year_string, '-')}
  
  p_authors_affil <- NULL
  
  aff_string = as.character(MyData$Affiliation[j][[1]])
  aff_list <- str_split(aff_string, ' #, ')
  

  if (length(aff_list[[1]])>1) 
  {total_coAuthored_Affiliations <- total_coAuthored_Affiliations + length(aff_list[[1]])}
  
  for (i in 1:length(aff_list[[1]]))
  {
    affiliation <- aff_list[[1]][i]
    my_string <- affiliation
    
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
      pcountry <-gregexpr(",",my_string)
      pcountry_value <- pcountry[[1]][length(pcountry[[1]])]
      country_name <- trimws(substrRight(my_string, nchar(my_string)-pcountry_value))
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
    
    if (!is.na(country_code)) { flag_all_NA <- FALSE }
    p_authors_affil[i] <- country_code
} 
  
  p_authors_affil_distint <- unique(p_authors_affil) 
  n_o_Countries_notNA <- length(p_authors_affil_distint[!is.na(p_authors_affil_distint)]) 
  n_o_Countries <- length(p_authors_affil_distint) 
  
  if (n_o_Countries_notNA > 1) 
  {
    total_coAuthored_papers_International <- total_coAuthored_papers_International +1 
    for (m in 1:(n_o_Countries-1))
    {
      for (n in (m+1):n_o_Countries){
        Country1[k] <- p_authors_affil_distint[m]
        Country2[k] <- p_authors_affil_distint[n]
        if ((Country1[[k]]==Country2[[k]]) | is.na(Country1[k]) | is.na(Country2[k]))   
          {
          International[k] <- 0                                                      
          Year[k][[1]]<-""                                                           
          if (length(year_list)>0) 
          {Year[k] <- year_list[[1]][length(year_list[[1]])]}
          k <-k+1 
          } else
          {  
            International[k] <-1
            if (Country1[[k]]>Country2[[k]])
              {
              temp_Country <- Country1[[k]]
              Country1[[k]]  <- Country2[[k]]
              Country2[[k]] <- temp_Country
              }
            Year[k][[1]]<-""
            if (length(year_list)>0) 
              {Year[k] <- year_list[[1]][length(year_list[[1]])]}
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
  write.table(IRC, new_file, sep=",", append = TRUE, row.names=FALSE, col.names = TRUE)
  
}
