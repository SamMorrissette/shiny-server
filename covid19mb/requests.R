library(rvest)
library(tidyverse)
library(jsonlite)

print(getwd())
today <- format(Sys.Date(),"%Y%m%d")
#MALE VS FEMALE AND AGE
url <- paste0('https://manitoba.ca/health/publichealth/public_app/By_age_sex_',today,'.html')
webpage <- read_html(url)
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
write.csv(fullData,'AgeGenderData.csv',row.names=FALSE)

#TABLE
mainpage <- read_html('https://www.gov.mb.ca/covid19/updates/index.html')
tbls_ls <- html_nodes(mainpage, "table") %>% 
  html_table(fill = TRUE,header=TRUE)
fullTable <- tbls_ls[[1]]
HR_UID <- c("IERHA","NRHA","PMH","SHR","WRHA","WRHA-CH")
Region <- c("Interlake-Eastern Regional Health Authority",
            "Northern Regional Health Authority",
            "Prairie Mountain Health",
            "Southern Health-Sante Sud",
            "Winnipeg Regional Health Authority",
            "Winnipeg Regional Health Authority (Churchill)")

#Add zeros for churchill
fullTable <- fullTable[1:5,]
Presumptive <- c(fullTable$`Probable Positive **`,0)
Confirmed <- c(fullTable$`Confirmed Positive *`,0)
Total <- c(fullTable$`Total Cases`,0)

fullDataFrame <- data.frame(HR_UID=HR_UID,
                            Region=Region,
                            Presumptive=Presumptive,
                            Confirmed=Confirmed,
                            Total=Total)
write.csv(fullDataFrame,'CaseData.csv',row.names=FALSE)

epiCurve <- read.csv('/srv/shiny-server/covid19mb/epicurve.csv')
epiCurve$date <- as.Date(epiCurve$date)

#Check to see if the last updated date is the same as the last date in the csv file.
#If not, then add the last updated date to the last row along with the "total" row from the table
#New cases will be the new row cumulative minus the last row cumulative.
updatedDate <- html_nodes(mainpage,xpath="//h3[contains(., 'Cases and Risk of COVID')]/following-sibling::p") %>%
  grep(pattern="Last updated:",value=TRUE)
updatedDate <- updatedDate[1] %>% 
  str_extract(pattern='(Jan(uary)?|Feb(ruary)?|Mar(ch)?|Apr(il)?|May|Jun(e)?|Jul(y)?|Aug(ust)?|Sep(tember)?|Oct(ober)?|Nov(ember)?|Dec(ember)?)\\s+\\d{1,2},\\s+\\d{4}') %>%
  as.Date(format="%B %d, %Y")
lastDate <- tail(epiCurve$date,1)
if (updatedDate != lastDate) {
  newCum <- tbls_ls[[1]]$`Total Cases`[6]
  newNew <- newCum-tail(epiCurve$cumulative,1)
  newRow <- data.frame(date=updatedDate,
                       new=newNew,
                       cumulative=newCum)
  newData <- rbind(epiCurve,newRow)
  write.csv(newData,'epicurve.csv',row.names = FALSE)
}

#Number of tests
numTests <- html_nodes(mainpage,"p") %>%
  grep(pattern="tests for COVID",value=TRUE) %>%
  str_extract(pattern="(\\d{0,3},)?(\\d{3},)?\\d{0,3}(?=\\s+tests)")
numTests <- as.numeric(gsub("\\,", "", numTests))


#DIR
recovered <- html_nodes(mainpage,"li") %>% 
  grep(pattern="recovered from COVID-19",value=TRUE) %>%
  str_extract("(\\d{0,3},)?(\\d{3},)?\\d{0,3}(?=\\s+individuals)")
recovered <- as.numeric(gsub("\\,", "", recovered))
deaths <- tbls_ls[[1]]$Deaths[6]
infected <- tbls_ls[[1]]$`Total Cases`[6] - (recovered+deaths)
DIR <- data.frame(Status=c("Infected", "Recovered", "Deaths", "Tested"),
                  Number=c(infected,recovered,deaths,numTests))
write.csv(DIR,"Recovered.csv",row.names=FALSE)
