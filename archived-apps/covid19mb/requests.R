library(rvest)
library(tidyverse)
library(jsonlite)

readURL <- function(url) {
  out <- tryCatch(
    expr= {
      read_html(url)
    },
    error = function(e) {
      return(NA)
    }
  )
  return(out)
}

#MALE VS FEMALE AND AGE
today <- format(Sys.Date(),"%Y%m%d")
url <- paste0('https://manitoba.ca/health/publichealth/public_app/By_age_sex_',today,'.html')
webpage <- readURL(url)

if (is.na(webpage)) {
  yesterday <- format(Sys.Date()-1,"%Y%m%d")
  url <- paste0('https://manitoba.ca/health/publichealth/public_app/By_age_sex_',yesterday,'.html')
  webpage <- readURL(url)
}

data <- webpage %>% 
  html_nodes('script') %>% 
  html_text() %>% 
  grep(pattern = "x", value = TRUE) %>% 
  fromJSON()
males <- data$x$data$x[[1]]
females <- data$x$data$x[[2]]
categories <- data$x$data$y[[1]]
fullData <- data.frame(Age=categories,
                       MaleCount=males,
                       FemaleCount=females)
write.csv(fullData,'/srv/shiny-server/covid19mb/AgeGenderData.csv',row.names=FALSE)

#REGIONAL
mainpage <- read_html('https://www.gov.mb.ca/covid19/updates/index.html')
url <- paste0('https://manitoba.ca/health/publichealth/public_app/By_RHA_',today,'.html')
webpage <- readURL(url)

data <- webpage %>% 
  html_nodes('script') %>% 
  html_text() %>% 
  grep(pattern = "x", value = TRUE) %>% 
  fromJSON()
confirmed <- data$x$data$x[[1]]
presumptive <- data$x$data$x[[2]]
confirmed.cat <- data$x$data$y[[1]]
presumptive.cat <- data$x$data$y[[2]]
confirmed.df <- data.frame(Region=confirmed.cat,
                           Confirmed=confirmed)
presumptive.df <- data.frame(Region=presumptive.cat,
                          Presumptive=presumptive)
full.df <- merge(confirmed.df,presumptive.df,all.x=TRUE)
full.df$Presumptive[is.na(full.df$Presumptive)] <- 0
ch.df <- data.frame(Region="Winnipeg Regional Health Authority (Churchill)",
                    Confirmed=0,
                    Presumptive=0)
full.df <- rbind(full.df,ch.df)

full.df$HR_UID <- c("IERHA","NRHA","PMH","SHR","WRHA","WRHA-CH")
full.df$Region <- c("Interlake-Eastern Regional Health Authority",
            "Northern Regional Health Authority",
            "Prairie Mountain Health",
            "Southern Health-Sante Sud",
            "Winnipeg Regional Health Authority",
            "Winnipeg Regional Health Authority (Churchill)")
full.df$Total <- full.df$Confirmed+full.df$Presumptive

write.csv(full.df,'/srv/shiny-server/covid19mb/CaseData.csv',row.names=FALSE)

#EPICURVE
epiCurve <- read.csv('/srv/shiny-server/covid19mb/epicurve.csv')
epiCurve$date <- as.Date(epiCurve$date)

lastDate <- tail(epiCurve$date,1)
newDate <- lastDate+1

newCum <- sum(full.df$Total)
newNew <- newCum-tail(epiCurve$cumulative,1)
newRow <- data.frame(date=newDate,
                       new=newNew,
                       cumulative=newCum)
newData <- rbind(epiCurve,newRow)
write.csv(newData,'/srv/shiny-server/covid19mb/epicurve.csv',row.names = FALSE)

#TESTS COMPLETED
numTests <- html_nodes(mainpage,xpath="//*[(@id = 'test-button')]//h1") %>%
  html_text()
numTests <- as.numeric(gsub("\\,","",numTests))

testsCompleted <- read.csv('/srv/shiny-server/covid19mb/TestsCompleted.csv')
newTests <- numTests - tail(testsCompleted$TestsCompleted,1)
testsCompleted$Date <- as.Date(testsCompleted$Date,format="%Y-%m-%d")
newRow <- data.frame(Date=Sys.Date(),
                     TestsCompleted=numTests,
                     newTests=newTests)
newData <- rbind(testsCompleted,newRow)

write.csv(newData,'/srv/shiny-server/covid19mb/TestsCompleted.csv',row.names=FALSE)


#DIR
numRecovered <- html_nodes(mainpage,xpath="//*[(@id = 'recovered-button')]//h1") %>%
  html_text()
numRecovered <- as.numeric(gsub("\\,","",numRecovered))

numDeaths <- html_nodes(mainpage,xpath="//*[(@id = 'deaths-button')]//h1") %>%
  html_text()
numDeaths <- as.numeric(gsub("\\,","",numDeaths))

numinfected <- html_nodes(mainpage,xpath="//*[(@id = 'active-button')]//h1") %>%
  html_text()
numInfected <- as.numeric(gsub("\\,","",numInfected))

DIR <- data.frame(Status=c("Infected", "Recovered", "Deaths", "Tested"),
                  Number=c(numInfected,numRecovered,numDeaths,numTests))
write.csv(DIR,"/srv/shiny-server/covid19mb/Recovered.csv",row.names=FALSE)
