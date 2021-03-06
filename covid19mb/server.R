library(leaflet)
library(plotly)
library(lubridate)
library(DT)
library(rgdal)
library(tidyverse)

mb_raw <- readOGR( 
  dsn= "mapData",
  layer = "HR_046a18a_e", verbose = FALSE,
  use_iconv=TRUE, encoding="CP1250"
)

mb_map <- spTransform(mb_raw, CRS("+proj=longlat +datum=WGS84"))
new <- data.frame(HR_UID='100',
                  ENGNAME='Winnipeg Regional Health Authority (Churchill',
                  FRENAME = 'NULL1',
                  SHAPE_AREA = '1',
                  SHAPE_LEN = '1')
mb_map@data <- rbind(mb_map@data,new)
mb_map@plotOrder <- as.integer(c(1,5,4,2,3,6))

p1 <- Polygon(mb_map@polygons[[3]]@Polygons[[2]])
ps <- Polygons(list(p1),ID=11)
mb_map@polygons[[6]] <- ps
mb_map@polygons[[3]]@Polygons[[2]] <- NULL

function(input, output, session) {
  cases <- read.csv('CaseData.csv')
  ageGender <- read.csv("AgeGenderData.csv")
  DIR <- read.csv("Recovered.csv",stringsAsFactors = TRUE)
  epiCurve <- read.csv("epicurve.csv")
  tests <- read.csv("TestsCompleted.csv")
  infstat <- read.csv("InfectedStatus.csv",stringsAsFactors = TRUE)
  
  mb_map$HR_UID <- c("NRHA","SHR","WRHA","PMH","IERHA","WRHA-CH")
  mb_map <- merge(mb_map,cases,all.x=TRUE)
  mb_map$Total[is.na(mb_map$Total)] <- 0
  mb_map$Confirmed[is.na(mb_map$Confirmed)] <- 0
  mb_map$Presumptive[is.na(mb_map$Presumptive)] <- 0
  
  mb_map$FULLNAME <- c("Northern Regional Health Authority",
                       "Southern Health-Sante Sud",
                       "Winnipeg Regional Health Authority",
                       "Prairie Mountain Health",
                       "Interlake-Eastern Regional Health Authority",
                       "Winnipeg Regional Health Authority (Churchill)")
  
  popup_info = paste0("<b> Region: </b>", mb_map$FULLNAME, "<br>",
                      "<b> Presumptive Cases: </b>", mb_map$Presumptive, "<br>",
                      "<b> Confirmed Cases: </b>", mb_map$Confirmed, "<br>",
                      "<b> Total Cases: </b>", mb_map$Total)
  
  
  ageGender$Age <- as.factor(ageGender$Age)
  ageGender$Age <- ordered(ageGender$Age, levels = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90-99","100+"))
  
  
  timeData <- epiCurve
  timeData$date <- as.Date(timeData$date)
  
  tests$Date <- as.Date(tests$Date,format="%Y-%m-%d")
  
  font <- list(
    size = 15
    #color = "black"
  )
  label <- list(
    namelength = -1
  )
  
  DIR$Status <- fct_inorder(DIR$Status)
  infstat$Status <- fct_inorder(infstat$Status)
  
  qpal <- colorNumeric(colorRamp(c("#FFFFFF", "#FF0000")), 
                       domain=c(0,mb_map$Total))
  output$map <- renderLeaflet({
    m <- leaflet(mb_map) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addPolygons(weight=1,
                  color="Grey",
                  fillOpacity=0.5,
                  fillColor=~qpal(mb_map$Total),
                  popup=popup_info)  %>%
      leaflet::addLegend(position=c("bottomright"),
                pal=qpal,
                values=~mb_map$Total,
                opacity=.5,
                title="# of Cases") %>%
      setView(lat=53.7609,lng=-98.8139,zoom=5)
    m
    })
  
  output$ageHist <- renderPlotly({
    p <- plot_ly(ageGender,x=~Age,y=~MaleCount,type='bar', name="Male",
                 marker = list(color = 'rgb(135,206,235)'),
                 hovertemplate = '<b>Age:</b> %{x} <br> <b>Gender:</b> Male <br> <b># of Cases:</b> %{y} <br><extra></extra>') %>%
      add_trace(y=~FemaleCount,name="Female",
                marker = list(color = 'rgb(223,82,134)'),
                hovertemplate = '<b>Age:</b> %{x} <br> <b>Gender:</b> Female <br> <b># of Cases:</b> %{y}<extra></extra>') %>%
      config(displayModeBar = F) %>%
      style(hoverlabel=label) %>%
      layout(title="Age Distribution",
             xaxis=list(title="Age"),
             yaxis=list(title="# of cases"),
             font=list(family="Arial",size=13),
             legend=list(x=0.8,y=0.8))
    })
  
  output$DIR <- renderPlotly({
    DIR2 <- DIR %>% filter(Status != "Tested")
    d <- plot_ly(DIR2,x=~Status,y=~Number,type='bar',
                 marker = list(color = c('rgb(255,165,0)','rgb(0,137,0)','rgb(118,0,0)')),
                 hovertemplate = '<b>Status:</b> %{x} <br> <b>Number:</b> %{y}<extra></extra>')
    d <- d %>% 
      config(displayModeBar = F) %>%
      layout(title="Infected/Recovered/Deaths",
             xaxis=list(title="Status"),
             yaxis=list(title=""),
             font=list(family="Arial",size=13))
  })
  
  output$Tested <- renderPlotly({
    a <- plot_ly(tests,x=~Date,y=~TestsCompleted,type='scatter',name="Tests Completed",mode='markers+lines',
                 line=list(color="Purple"),
                 marker=list(color="Purple"))
                 #hovertemplate = '<b>Date:</b> %{x} <br> <b>Total Tests:</b> %{y}<extra></extra>')
    a <- a %>% add_trace(tests,y=~newTests,type='bar',name="New Tests",
                         marker=list(color=c('rgb(153,204,153')))
                         #hovertemplate = '<b>Date:</b> %{x} <br> <b>New Tests:</b> %{y}<extra></extra>')
    a <- a %>% 
      config(displayModeBar = F) %>%
      style(hoverlabel=label) %>%
      layout(title="Total Tests Completed",
             legend=list(x=0.1,y=0.9,bgcolor='#E2E2E2'),
             xaxis=list(title="",dtick=86400000.0,tickangle=45,ticklen=5,showgrid=TRUE,automargin=FALSE),
             margin=list(l=50,r=25,b=50),
             yaxis=list(title="# of Tests"),
             font=list(family="Arial",size=13),
             hovermode="x unified")
  })
  
  output$infected <- renderPlotly({
    s <- plot_ly(infstat,x=~Status,y=~Number,type='bar',
                 marker = list(color = c('rgb(51,102,153)','rgb(208,138,175)','rgb(170,1,120)')),
                 hovertemplate = '<b>Status:</b> %{x} <br> <b>Number:</b> %{y}<extra></extra>')
    s <- s %>% 
      config(displayModeBar = F) %>%
      layout(title="Infected Status",
             xaxis=list(title="Status of Infected Cases"),
             yaxis=list(title=""),
             font=list(family="Arial",size=13))
  })

  
  output$timePlot <- renderPlotly({
    t <- plot_ly(timeData,x=~date,y=~cumulative,type='scatter',name="Cumulative Cases",mode='markers+lines')
                 #hovertemplate = '<b>Cumulative Cases:</b> %{y}<extra></extra>')
    t <- t %>% add_trace(timeData,y=~new,type='bar',name="New Cases")
                         #hovertemplate = '<b>New Cases:</b> %{y}<extra></extra>')
    t <- t %>% 
      config(displayModeBar = F) %>%
      style(hoverlabel=label) %>%
      layout(title="Cases over Time",
             legend=list(x=0.1,y=0.9,bgcolor='#E2E2E2'),
             xaxis=list(title="",dtick=86400000.0,tickangle=45,ticklen=5,showgrid=TRUE),
             yaxis=list(title="# of Cases"),
             font=list(family="Arial",size=13),
             hovermode="x unified")
    t
    })
  
  output$genderHist <- renderPlotly({
      male <- sum(ageGender$MaleCount)
      female <- sum(ageGender$FemaleCount)
      gender_data <- data.frame(Gender=c("M","F"),Count=c(male,female))
      t <- plot_ly(gender_data,x=~Gender,y=~Count,type='bar',
                   marker = list(color = c('rgb(135,206,235)','rgb(223,82,134)','rgb(192,192,192)')),
                   hovertemplate = '<b>Gender:</b> %{x} <br> <b># of Cases:</b> %{y}<extra></extra>')
      t <- t %>% 
        config(displayModeBar = F) %>%
        style(hoverlabel=label) %>%
        layout(title="Total Gender Distribution",
               xaxis=list(title="Gender"),
               yaxis=list(title=""),
               font=list(family="Arial",size=13))
      t
    })
    
    
  # output$data.table <- renderDataTable({
  #   DT::datatable(table.data,
  #                 rownames = FALSE,
  #                 options = list(autoWidth=TRUE)
  #   )
  # })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, "covid19-mb.csv", sep = "")
    },
    content = function(file) {
      write.csv(table.data, file, row.names = FALSE)
    }
  )
}
