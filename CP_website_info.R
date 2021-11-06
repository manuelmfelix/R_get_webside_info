library(RSelenium)
library(rvest)
library(htmltools)
library(readxl)
library(plyr)
library(beepr)

options(error = beep)

setwd("C:/Users/mfelix/Desktop")

# Upload Origin Destination table

odTable <- readxl::read_excel(path="C:/Users/mfelix/Desktop/OD_CP.xlsx",sheet = "OD")

### Uncomment for example file ###

# origin <- c("Lisboa","Lisboa","Porto","Porto","Coimbra","Coimbra")
# destination <- c("Porto","Coimbra","Lisboa","Coimbra","Lisboa","Porto")
# odTable <- data.frame(origin,destination)

# Load page----
tryCatch(remDr <- remoteDriver(), error=function(e) NULL)

counterOpen <- 1

# OD pairs----

while(!empty(odTable)){
  
  if(counterOpen==3){
    remDr$open()
    
    #Navigate to the page
    remDr$navigate("https://www.cp.pt/passageiros/pt")
    
    counterOpen <- 1
    
    #Desligar Pop-Up
    
    Sys.sleep(5)    
  }

###                                                           ###
### Need to ajust the initial day according with the calendar ###
###                                                           ###
    
    firstDay <- 2
    firstCalendarLine <- 2
    secondCalendarLine <- 3
    counterDays <- 1
    dayChoosen <- firstDay

  # Rotina de Escolha de Dia----
    
  while(counterDays<=7){
    
    remDr$navigate("https://www.cp.pt/passageiros/pt/")
    
    Sys.sleep(5)
    
    page_source<-remDr$getPageSource()
    
    # Origin
    
    pontoOrigem <- as.character(odTable[1,1])
    wxbox1 <- remDr$findElement(using = 'css selector', "div.col-md-10:nth-child(1) > div:nth-child(1) > div:nth-child(1) > input:nth-child(2)")
    wxbox1$sendKeysToElement(list(pontoOrigem))
    Sys.sleep(1)
    wxbox1$sendKeysToElement(list(key = 'enter'))
    Sys.sleep(0.5)
    
    # Destination
    
    pontoDestino <- as.character(odTable[1,2])
    wxbox2 <- remDr$findElement(using = 'css selector', "div.col-md-10:nth-child(1) > div:nth-child(2) > div:nth-child(1) > input:nth-child(2)")
    wxbox2$sendKeysToElement(list(pontoDestino))
    Sys.sleep(1)
    wxbox2$sendKeysToElement(list(key = 'enter'))
    Sys.sleep(0.5)
    
    # Choose new day

    nextDay <- tryCatch(remDr$findElement(using = 'css selector', "#datepicker-first"), error=function(e) NULL)
    nextDay$clickElement()
    
    
    # Sys.sleep(1)
    
    # nextDay <- remDr$findElement(using = 'css selector', "#datepicker-first_root > div:nth-child(1) > div:nth-child(1) > div:nth-child(1) > div:nth-child(1) > div:nth-child(1) > div:nth-child(4)")
    # nextDay$clickElement()
    
    Sys.sleep(2)
    
    # Change first Day on number
    
    if(dayChoosen>=2){
      nextDay <- remDr$findElement(using = 'css selector', paste("#datepicker-first_table > tbody:nth-child(2) > tr:nth-child(",firstCalendarLine,") > td:nth-child(",dayChoosen,") > div:nth-child(1)",sep=""))
      nextDay$clickElement()
    }else if(dayChoosen<2){
      nextDay <- remDr$findElement(using = 'css selector', paste("#datepicker-first_table > tbody:nth-child(2) > tr:nth-child(",secondCalendarLine,") > td:nth-child(",dayChoosen,") > div:nth-child(1)",sep=""))
      nextDay$clickElement()
    }
    
    # Examples of other days
    # 02.07  #datepicker-first_table > tbody:nth-child(2) > tr:nth-child(1) > td:nth-child(6) > div:nth-child(1)
    # 03.07  #datepicker-first_table > tbody:nth-child(2) > tr:nth-child(1) > td:nth-child(7) > div:nth-child(1)
    # 04.07  #datepicker-first_table > tbody:nth-child(2) > tr:nth-child(2) > td:nth-child(1) > div:nth-child(1)
    # 05.07  #datepicker-first_table > tbody:nth-child(2) > tr:nth-child(2) > td:nth-child(2) > div:nth-child(1)
    
    searchButton <- remDr$findElement(using = 'css selector', "input.btn")
    searchButton$clickElement()
    
    Sys.sleep(3)
      
      # Get Info on the page----

      page_source1<-remDr$getPageSource()
    Sys.sleep(0.5)
      
      # Nr Services
      
      nServicos <- read_html(page_source1[[1]]) %>% html_nodes(".results > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(1) > td:nth-child(1)") %>%
        html_text()
      nServicos <- as.integer(gsub("[\t\n]", "", nServicos) %>% sub(".*- ","",.) %>% sub(" .*","",.))
      
      counterServicos <- 1
      
      empresa <- "CP"
      
      # Origin and Destination
      
      od <- read_html(page_source1[[1]]) %>% html_nodes(".results > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(1) > td:nth-child(2)") %>%
        html_text()
      origem <- sub("\\ >.*","",od) %>% substr(.,1,nchar(.)-1)
      destino <-  sub(".*> ","",od) %>% substr(.,2,nchar(.))
      
      dia <- read_html(page_source1[[1]]) %>% html_nodes(".results > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(1) > td:nth-child(4)") %>%
        html_text() %>% gsub("[\t\n]", "", .)
      
      promo <- read_html(page_source1[[1]]) %>% html_nodes(paste("th.hidden-xs:nth-child(7)",sep="")) %>%
        html_text() %>% gsub("[\t\n]", "", .)
      
      # Get specifications on train services

      while(counterServicos<=nServicos){
        
        Sys.sleep(1.5)
        page_source1<-remDr$getPageSource()
        
        
        
        servico <- read_html(page_source1[[1]]) %>% html_nodes(paste(".table > tbody:nth-child(2) > tr:nth-child(",counterServicos,") > td:nth-child(2)",sep="")) %>%
          html_text() %>% gsub("[\t\n]", "", .) %>% gsub(" ", "", ., fixed = TRUE)
        
        partida <- read_html(page_source1[[1]]) %>% html_nodes(paste("#goDeparTime",counterServicos,sep="")) %>%
          html_text()
        
        chegada <- read_html(page_source1[[1]]) %>% html_nodes(paste("#goArrivalTime",counterServicos,sep="")) %>%
          html_text()
        
        duracao <- read_html(page_source1[[1]]) %>% html_nodes(paste(".table > tbody:nth-child(2) > tr:nth-child(",counterServicos,") > td:nth-child(5)",sep="")) %>%
          html_text() %>% gsub("[\t\n]", "", .) %>% gsub(" ", "", ., fixed = TRUE)

        preco <- read_html(page_source1[[1]]) %>% html_nodes(paste(".table > tbody:nth-child(2) > tr:nth-child(",counterServicos,") > td:nth-child(6)",sep="")) %>%
          html_text() %>% gsub("[\t\n]", "", .) %>% gsub(" ", "", ., fixed = TRUE) %>% sub(".*/","",.) %>% sub("\200","",.)
        
        # Get details of the schedule
        
        Sys.sleep(0.5)
        
        if(promo == "Bilhete Promo  1ª/2ª"){
          plusSign <- remDr$findElement(using = 'css selector', paste(".table > tbody:nth-child(2) > tr:nth-child(",counterServicos,") > td:nth-child(8) > a:nth-child(1)",sep=""))
          plusSign$clickElement()
        }else{
          plusSign <- remDr$findElement(using = 'css selector', paste(".table > tbody:nth-child(2) > tr:nth-child(",counterServicos,") > td:nth-child(7) > a:nth-child(1)",sep=""))
          plusSign$clickElement()
        }

        Sys.sleep(1)
        
        page_source2<-remDr$getPageSource()
        
        comboio1 <- read_html(page_source2[[1]]) %>% html_nodes("table.table:nth-child(4) > tbody:nth-child(2) > tr:nth-child(1) > td:nth-child(1)") %>%
          html_text() %>% gsub("[\t\n]", "", .) %>% sub(".* ","",.)
        
        if(identical(comboio1, character(0))==TRUE){comboio1 <- NA}
        
        comboio2 <- read_html(page_source2[[1]]) %>% html_nodes("table.table:nth-child(5) > tbody:nth-child(2) > tr:nth-child(1) > td:nth-child(1)") %>%
          html_text() %>% gsub("[\t\n]", "", .) %>% sub(".* ","",.)
        
        if(identical(comboio2, character(0))==TRUE){comboio2 <- NA}
        
        comboio3 <- read_html(page_source2[[1]]) %>% html_nodes("table.table:nth-child(6) > tbody:nth-child(2) > tr:nth-child(1) > td:nth-child(1)") %>%
          html_text() %>% gsub("[\t\n]", "", .) %>% sub(".* ","",.)
        
        if(identical(comboio3, character(0))==TRUE){comboio3 <- NA}
        
        comboio4 <- read_html(page_source2[[1]]) %>% html_nodes("table.table:nth-child(7) > tbody:nth-child(2) > tr:nth-child(1) > td:nth-child(1)") %>%
          html_text() %>% gsub("[\t\n]", "", .) %>% sub(".* ","",.)
        
        if(identical(comboio4, character(0))==TRUE){comboio4 <- NA}
        
        # Back to the overall schedule
        
        back <- remDr$findElement(using = 'css selector', "a.btn-primary:nth-child(1)")
        back$clickElement()
        
        Sys.sleep(0.5)

        horario1 <- as.data.frame(cbind(dia,origem,destino,partida,chegada,duracao,preco,empresa,comboio1,comboio2,comboio3,comboio4,servico))
            
            if(exists("horario")){
              horario <- rbind(horario,horario1)
            }else{
              horario <- horario1
            }

            cat(counterServicos,": ",partida,"\n")

            counterServicos <- counterServicos+1
            
          } # Schedule Loop
      
        cat(dia," terminou","\n")

        counterDays <- counterDays+1
        dayChoosen <- dayChoosen+1
        if(dayChoosen==8){dayChoosen <- 1}
        
        Sys.sleep(2)
          
        } # Day Loop
  
  odTable = odTable[-1,]
  
  cat("Next Origin: ", as.character(odTable[1,1]),"\n","Next Destination: ", as.character(odTable[1,2]))
  
  tryCatch(write.csv(horario,file=paste("CP.csv",sep="")), error=function(e) NULL)
  
  counterOpen <- counterOpen+1
  
  remDr$deleteAllCookies
  
  Sys.sleep(1)
  
} # End Search
