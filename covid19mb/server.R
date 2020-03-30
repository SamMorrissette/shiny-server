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

cases <- read.csv('CaseData.csv')
ageData <- read.csv("AgeData.csv")
gender_data <- read.csv("GenderData.csv")
DIR <- read.csv("Recovered.csv",stringsAsFactors = TRUE)


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



ageData$Age <- as.factor(ageData$Age)
ageData$Age <- ordered(ageData$Age, levels = c("0-9"," 10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90+","Pending"))


dates <- seq.Date(from=as.Date("2020/03/01"),to=as.Date("2020/03/30"),by="day")
newcases <- c(0,0,0,0,0,0,0,0,0,0,0,3,1,0,3,1,7,2,0,0,2,1,0,1,14,1,3,25,8,24)
cumcases <- cumsum(newcases)
timeData <- data.frame(dates,newcases,cumcases)

font <- list(
  size = 15,
  color = "black"
)
label <- list(
  bgcolor = "#FFFFFF",
  bordercolor = "transparent",
  font=font
)

DIR$Status <- fct_inorder(DIR$Status)

qpal <- colorNumeric(colorRamp(c("#FFFFFF", "#FF0000")), mb_map$n, n = 4)

function(input, output, session) {
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
    ageData$fills <- ifelse(ageData$Age == "Pending","gray","darkgreen")
    ageData <- ageData %>% 
      mutate(prop=Count/sum(Count))
    ageData$prop <- round(ageData$prop,2)
    p <- ggplot(ageData,aes(text= paste0("<b> Age: </b>",Age, "<br>",
                                        "<b> # of Cases: </b>",Count,"<br>",
                                        "<b> Proportion of Cases: </b> ",prop))) + 
      geom_bar(aes(x=Age,y=Count,fill=fills),stat="identity") +
      scale_fill_identity() +
      theme_minimal() + 
      ylim(c(0,max(ageData$Count+2))) +
      ylab("# of Cases") + 
      ggtitle("Age Distribution") +
      theme(plot.title = element_text(hjust=0.5),legend.position = "none")
    p %>% ggplotly(tooltip=c("text")) %>%
      style(hoverlabel=label) %>%
      config(displayModeBar = F)
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
  
  output$Tested <- renderText({
    ntest <- DIR %>% filter(Status == "Tested")
    ntest$Number
  })

  
  output$timePlot <- renderPlotly({
    t <- plot_ly(timeData,x=~dates,y=~cumcases,type='scatter',name="Cumulative Cases",mode='lines',
                 hovertemplate = '<b>Date:</b> %{x} <br> <b>Cumulative Cases:</b> %{y}<extra></extra>')
    t <- t %>% add_trace(y=newcases,type='bar',name="New Cases",
                         hovertemplate = '<b>Date:</b> %{x} <br> <b>New Cases:</b> %{y}<extra></extra>')
    t <- t %>% 
      config(displayModeBar = F) %>%
      style(hoverlabel=label) %>%
      layout(title="Cases over Time",
             legend=list(x=0.1,y=0.9,bgcolor='#E2E2E2'),
             xaxis=list(title="Date",dtick=86400000.0,tickangle=45,ticklen=5,showgrid=TRUE),
             yaxis=list(title="# of Cases"),
             font=list(family="Arial",size=13))
    t
    })
  
  output$genderHist <- renderPlotly({
      t <- plot_ly(gender_data,x=~Gender,y=~Count,type='bar',
                   marker = list(color = c('rgb(135,206,235)','rgb(223,82,134)','rgb(192,192,192)')),
                   hovertemplate = '<b>Gender:</b> %{x} <br> <b># of Cases:</b> %{y}<extra></extra>')
      t <- t %>% 
        config(displayModeBar = F) %>%
        style(hoverlabel=label) %>%
        layout(title="Gender Distribution",
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
