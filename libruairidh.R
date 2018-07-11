## library functions for iva analysis and database connections
#by: Ruairidh Cumming
#(c) Grove Capital Management 2018
#updated 11/07/2018

sqlize<-function(x){
                      out<-paste0("'",as.character(x[1]),"'")
  
                    for (i in x[2:length(x)]){
                        out<-paste0(out,",'",as.character(i),"'")
                      }
                      return(out)
}
tofive<-function(x){
  x<-ceiling(x/5)*5
  y<-x-5
  z<-paste0(y,'-',x)
  return(z)
}
superQuery<-function(db, queryString, df,IDs,groups, stringsAsFactors = F){
  require(RODBC)
  sqlize<-function(x){
    out<-paste0("'",as.character(x[1]),"'")

    for (i in x[2:length(x)]){
      if(!is.na(i)){
        out<-paste0(out,",'",as.character(i),"'")
      }
    }
    return(out)
  }
  unique(as.character(df[,groups]))

  for ( id in unique(as.character(df[,groups]))){
    print(id)

    SQLIDs<-sqlize(unique(df[,IDs][df[,groups]==id]))##get accounts for each dealname


    fullQueryString <-paste0(queryString," (",SQLIDs,")")
    moreSQL<-sqlQuery(db,fullQueryString,stringsAsFactors = stringsAsFactors )
    if (exists("optdata")){

      optdata<-rbind(optdata,moreSQL)
    }else{
      optdata<-moreSQL
    }
  }
  if(dim(optdata)[1]==0){

    warning("No data returned with given IDs")
  }

  return (optdata)

}

distinguish <- function(x,from=15){
  top=from
  distinction = vector()
  for (i in top:1){
    
    distinctarray = substr(x,1,i)
    distinction[top+1-i]=length(unique(distinctarray))
  }
  return(distinction)
}


driver <- function(x,from=15) {
  require(stringi)
  top=from
  driver = vector()
  for (i in top:1)
   
  { 

    driver[top+1-i,1:10]=head(sort(table (substr(x,1,i)),decreasing = T),n=10)
    
  }
  return(driver)
}


#converts uk postcodes to region name strings using sotred data file 
postcodeToRegion<-function (postcodes){

require(readxl)
  codes= read_excel('E:/Users/Ruairidh.cumming/Desktop/useful scripts/r/postcodes2.xlsx')
  postcodes<- substring(postcodes,1,2)
  postcodes<- gsub('[0-9]+','',postcodes)
  for (i in 1:dim(codes ) [1]){
    paste0(codes[i,2])
    postcodes[postcodes==paste0(codes[i,1])]<-paste0(codes[i,2])
    
    
  }
  return(postcodes)
} 

Age<- function(dates) {
  require (lubridate)
  x <- ymd(as.period(interval(dates , Sys.Date() )))
  return (x)
}
ClosestMatch <- function(string, stringVector){
  require (RecordLinkage)
  distance = levenshteinSim(string, stringVector);
 
  return(stringVector[distance == max(distance)])
}

## creates list of "unique" values from array by fuzzy matching
fuzzy_unique<- function (inputarray,dist=0.1){
  
  ##remove data which will crash agrep and order for retention of fewest elements
  inputarray<-inputarray[inputarray!=""]
  inputarray<- unique(inputarray)
  result<-array(inputarray[1])
  inputarray= inputarray[order(-nchar(inputarray),inputarray)]
  
  for (i in 1:length(inputarray)){ 
    
    
    
    if (length(agrep(inputarray[i],result,max.distance = dist, ignore.case = TRUE)) == 0 ){
      result[length(result)+1]<-inputarray[i]
      
      
      
     
    }
   
  }
  
  
  
  return (result)  
}
##improved fuzzy experiment
fuzzy_unique2<- function (inputarray,dist=0.5){
  require (RecordLinkage)
  ##remove data which will crash agrep and order for retention of fewest elements
  inputarray<-inputarray[inputarray!=""]
  inputarray<- unique(inputarray)
  result<-array(as.character(inputarray[1]))
  result[2]<-as.character(inputarray[2])
  inputarray= inputarray[order(-nchar(inputarray),inputarray)]
  
  for (i in 1:length(inputarray)){ 
    
    
    
    if (max(levenshteinSim(as.character(inputarray[i]),as.character(result)))<dist){
      result[length(result)+1]<-inputarray[i]
      
      
      
      
    }
    
  }
  
  
  
  return (result)  
}

pruneIPnames<- function(ipnames){
  ##kill whitespace 
  
  ipnames<- trimws(ipnames, which = 'both')
  
  ipnames<- tolower(ipnames)
   ##kill random punctuation
  ipnames<- gsub('\\*','',ipnames)
  ipnames<- gsub('\\_','',ipnames)
  ipnames<- gsub('\\.',' ',ipnames)
  ipnames<- gsub(',','',ipnames)
  ipnames<- gsub('\\+',' ',ipnames)
  ipnames<- trimws(ipnames, which = 'both')
  ipnames<- gsub('  ',' ',ipnames)
  ##kill common suffixes 
  ipnames<- gsub('services','',ipnames)
  ipnames<- gsub(' consultancy','',ipnames)
  ipnames<- gsub('company','',ipnames)
  ipnames<- gsub('solicitors','',ipnames)
  ipnames<- gsub('insolvency','',ipnames)
  ipnames<- gsub(' advisory','',ipnames)
  ipnames<- gsub(' bussiness','',ipnames)
  ipnames<- gsub(' business','',ipnames)
  ipnames<- gsub(' chartered','',ipnames)
  ipnames<- gsub(' debt ','',ipnames)
  ipnames<- gsub(' consulting','',ipnames)
  ipnames<- gsub(' consultants','',ipnames)
  ipnames<- gsub(' accountants','',ipnames)
  ipnames<- gsub(' practioners','',ipnames)
  ipnames<- gsub(' practitioners','',ipnames)
  ipnames<- gsub(' practitioner','',ipnames)
  ipnames<- gsub('solutions','',ipnames)
  ipnames<- gsub(' financial','',ipnames)
  ipnames<- gsub(' partnership','',ipnames)
  ipnames<- gsub(' partners','',ipnames)
  ipnames<- gsub(' insolvency','',ipnames)
  ipnames<- gsub(' associates','',ipnames)
  ipnames<- gsub(' supervising','',ipnames)
  ipnames<- gsub(' recovery','',ipnames)
  ipnames<- gsub(' recovery','',ipnames)
  ipnames<- gsub('limited','',ipnames)
  ipnames<- gsub('ltd','',ipnames)
  ipnames<- gsub('llp','',ipnames)
  ipnames<- gsub('plc','',ipnames)
  ipnames<- gsub(' &','',ipnames)
  ipnames<- gsub(' co ','',ipnames)
  ipnames<- gsub(' co$','',ipnames)
  ipnames<- gsub('&co$','',ipnames)
  ipnames<- gsub(' and ','',ipnames)
  ipnames<- trimws(ipnames, which = 'both')
  ipnames<- gsub('  ',' ',ipnames)
  #ipnames<- gsub(' ','',ipnames)
  
  ## kill things in brackets [e.]
  ipnames<- gsub( "*\\(.*\\)", " ", ipnames)
  ipnames<- gsub( "*\\[.*\\]", " ", ipnames)
 ipnames<- trimws(ipnames, which = 'both')
 ipnames<- gsub('  ',' ',ipnames)
 ipnames<- gsub('  ',' ',ipnames)
 ipnames<- gsub('  ',' ',ipnames)
} 
## closest string match for fuzzy joins 

fuzzy_targets<-function(framename,datacol,targets){
  require(dplyr)
  IP=unique(framename[,datacol])
  temphold = as.data.frame(IP)
  temphold$IP=as.character(temphold$IP)
  targetname=as.character(lapply(IP, function(x){
    ClosestMatch(as.character(x),targets)[1]
  }
  ))
  temphold$targetname=targetname
 
  
  temphold$distance =lapply(temphold$IP,function(x){
    as.numeric(max(levenshteinSim(x, targets))[1])
    
  }
  )
  temphold$distance=as.numeric(temphold$distance)
  
  if ('distance' %in% colnames(framename)){
    print('update distance')
   framename<- framename%>%select(-distance)
  }
  if ('targetname' %in% colnames(framename)){
    print('update targetnames')
    framename<- framename%>%select(-targetname)
  }
  
  return(left_join(framename,temphold, by='IP'))
}


## connect to database
## uses grove server name correct as of 12/02/2018
ServerConnect<-function(username='name',password='pword',database='dbname'){
USERNAME = username
PASSWORD = password # don't save this in the repo 
CATALOG  = database
#Setting up 'DSN-less' ODBC Connection string
MSConnection <- "Driver={SQL Server Native Client 11.0};SERVER=lna4mlnrfd.database.windows.net"
MSConnection <- paste(MSConnection, USERNAME , sep = ";UID=") 
MSConnection <- paste(MSConnection, PASSWORD , sep = ";PWD=") 
MSConnection <- paste(MSConnection, CATALOG , sep = ";DATABASE=") 

# Connect to data backend
cat("\nConnecting to cloud database")
dev<-odbcDriverConnect(MSConnection)
if (dev == -1){cat('\n \n \ndatabase connection failed, check your inputs')}else{cat('\n \n \nconnetion successful \n\n')}
return (dev)
}
