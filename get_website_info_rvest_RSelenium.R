# Get Coordinates and street name from website www.codigo-postal.pt using rvest or RSelenium----

# install.packages("tseries")
library(tseries)
# install.packages("tidyverse")
library(tidyverse)
# install.packages("plyr")
library(plyr)
# install.packages("rvest")
library(rvest)
# install.packages("dplyr")
library(dplyr)
setwd("C:/Users/Utilizador/Downloads")

# rvest Version

# Read database of postal codes
questionario <- readxl::read_xlsx("C:/Users/Utilizador/Downloads/codigos_postais.xlsx")

Codigos <- questionario[,1]

counter=0
rm(Latitude)
rm(Longitude)
rm(NomeRua)

while (!empty(questionario)) {

first <- as.character(questionario[1,2])
second <- as.character(questionario[1,3])

url <- paste("https://www.codigo-postal.pt/?cp4=",first,"&cp3=",second,sep="")

webpage <- read_html(url)

gps <- tryCatch(html_nodes(webpage,'span.pull-right'), error=function(e) NULL)

if(is.null(gps)==FALSE){
  gps <- html_text(gps)
  gps <- gps[1]
  gps <- (sub("^\\s+\\S+\\s+",'',gps))
  latitude <- (sub(",.*$", "",gps))
  longitude <- sub(".*,", "",gps)
  longitude <- gsub( " .*$", "",longitude )
  
  rua <- html_nodes(webpage,'.search-title')
  rua <- html_text(rua)
  rua <- rua[1]
  
  if(counter==0){
    Latitude <- data.frame(latitude)
    Longitude <- data.frame(longitude)
    NomeRua <- data.frame(rua)
  }else{
    Latitude <- rbind(Latitude,latitude)
    Longitude <- rbind(Longitude,longitude)
    NomeRua <- rbind(NomeRua,rua)
  }
}else{
  if(counter==0){
    Latitude <- data.frame(NULL)
    Longitude <- data.frame(NULL)
    NomeRua <- data.frame(NULL)
  }else{
    Latitude <- rbind(Latitude,latitude)
    Longitude <- rbind(Longitude,longitude)
    NomeRua <- rbind(NomeRua,rua)
  }
}

questionario=as.data.frame(questionario[-1,])
counter=counter+1

cat(rua," ",counter,"\n")

}

codigosPostais <- cbind(Codigos,Latitude,Longitude,NomeRua)

write.csv(codigosPostais,file=paste("codigosPostais",".csv",sep=""))


# RSelenium Version

# install.packages("RSelenium")
library(RSelenium)

setwd("C:/Users/Utilizador/Downloads")

questionario <- readxl::read_xlsx("C:/Users/Utilizador/Downloads/codigos_postais.xlsx")

fprof <- makeFirefoxProfile(list(browser.download.dir = "C:/Users/Utilizador/Downloads",
                                 browser.download.folderList = 2L,
                                 browser.download.manager.showWhenStarting = FALSE,
                                 browser.helperApps.neverAsk.openFile = "text/csv;charset=UTF-8",
                                 browser.helperApps.neverAsk.saveToDisk = "text/csv;charset=UTF-8"))

codigoPostal <- remoteDriver(extraCapabilities = fprof)
Sys.sleep(1)
codigoPostal$open()
Sys.sleep(10)
codigoPostal$navigate("https://www.codigo-postal.pt/")

counter = 0
rm(Latitude)
rm(Longitude)
rm(NomeRua)

while (!empty(questionario)) {
  
  first <- as.character(questionario[1,2])
  second <- as.character(questionario[1,3])
  
  wxbox1 <- codigoPostal$findElement(using = 'css selector', "#cp4")
  wxbox1$sendKeysToElement(list("", key = "control", "a"))
  wxbox1$sendKeysToElement(list(first, key = "enter"))
  Sys.sleep(1)
  wxbox2 <- codigoPostal$findElement(using = 'css selector', ".cp3")
  wxbox2$sendKeysToElement(list("", key = "control", "a"))
  wxbox2$sendKeysToElement(list(second, key = "enter"))
  Sys.sleep(1)
  wxbox3 <- tryCatch(codigoPostal$findElement(using = 'css selector', "span.pull-right"), error=function(e) NULL)
  if(is.null(wxbox3)==FALSE){
    gps <- as.character(wxbox3$getElementText())
    
    wxbox4 <- codigoPostal$findElement(using = 'css selector', ".search-title")
    rua <- as.character(wxbox4$getElementText())
    
    gps <- (sub("^\\S+\\s+",'',gps))
    latitude <- (sub(",.*$", "",gps))
    longitude <- sub(".*,", "",gps)
    
    if(counter==0){
      Latitude <- data.frame(latitude)
      Longitude <- data.frame(longitude)
      NomeRua <- data.frame(rua)
    }else{
      Latitude <- rbind(Latitude,latitude)
      Longitude <- rbind(Longitude,longitude)
      NomeRua <- rbind(NomeRua,rua)
    }
  }else{
    if(counter==0){
      Latitude <- data.frame(NULL)
      Longitude <- data.frame(NULL)
      NomeRua <- data.frame(NULL)
    }else{
      Latitude <- rbind(Latitude,latitude)
      Longitude <- rbind(Longitude,longitude)
      NomeRua <- rbind(NomeRua,rua)
    }
  }
  
  questionario=as.data.frame(questionario[-1,])
  counter=counter+1
  
}

Codigos <- questionario[,1]

codigosPostais <- cbind(Codigos,Latitude,Longitude,NomeRua)

write.csv(codigosPostais,file=paste("codigosPostais",".csv",sep=""))

