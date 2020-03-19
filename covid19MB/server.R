library(leaflet)
library(dygraphs)
library(plotly)
library(xts)
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

corona_data <- read.csv("Data.csv")
cases <- corona_data %>% 
  group_by(HR_UID) %>% 
  count()

mb_map$HR_UID <- c("NRHA","SHR","WRHA","PMH","IERHA","WRHA-CH")
mb_map <- merge(mb_map,cases,all.x=TRUE)
mb_map$n[is.na(mb_map$n)] <- 0

presumptive <- corona_data %>% 
  group_by(HR_UID,Presumptive.Confirmed) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  spread(Presumptive.Confirmed,n,fill=0)

mb_map <- merge(mb_map,presumptive,all.x=TRUE)
mb_map$Confirmed[is.na(mb_map$Confirmed)] <- 0
mb_map$Presumptive[is.na(mb_map$Presumptive)] <- 0

max.cases <- max(mb_map$n)
qpal <- colorNumeric(colorRamp(c("#FFFFFF", "#FF0000")), mb_map$n, n = 4)

marker_coords <- data.frame(lat=c(56.938653,49.874008,51.255479),
                            lon=c(-97.468105,-97.155128,-96.737120))
marker_radius=c(8,8,8)

mb_map$FULLNAME <- c("Northern Regional Health Authority",
                     "Southern Health-Sante Sud",
                     "Winnipeg Regional Health Authority",
                     "Prairie Mountain Health",
                     "Interlake-Eastern Regional Health Authority",
                     "Winnipeg Regional Health Authority (Churchill)")

popup_info = paste0("<b> Region: </b>", mb_map$FULLNAME, "<br>",
                    "<b># of Presumptive Cases: </b>", mb_map$Presumptive, "<br>",
                    "<b># of Confirmed Cases: </b>", mb_map$Confirmed, "<br>",
                    "<b># of Total Cases: </b>", mb_map$n)

ageData <- corona_data %>% 
  group_by(Age) %>% 
  count() %>%
  as.data.frame()
empty <- data.frame(Age=c("0-9","10-19","20-29","90+"),
           n=c(0,0,0,0))
empty$Age <- as.factor(empty$Age)
ageData <- rbind(ageData,empty)

ageData$Age <- ordered(ageData$Age, levels = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90+"))


dates <- seq.Date(from=as.Date("2020/03/01"),to=as.Date("2020/03/18"),by="day")
newcases <- c(0,0,0,0,0,0,0,0,0,0,0,3,1,0,3,1,7,2)
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

HR_UID_dict <- list('Winnipeg Health Authority' = 'WRHA',
                    'Northern Region Authority' = 'NRHA',
                    'Prairie Mountain Health' = 'PMH',
                    'Interlake-Eastern Regional Health Authority' = 'IERHA',
                    'Southern Health Sante Sud'= 'SHR')
corona_data$Region <- `levels<-`(corona_data$HR_UID, HR_UID_dict)
table.data <- corona_data %>%
  select(Region,Date.Reported,Gender,Age,Presumptive.Confirmed)
names(table.data) <- c("Region","Date Reported","Gender","Age Category", "Presumptive/Confirmed")

function(input, output, session) {
  output$map <- renderLeaflet({
    m <- leaflet(mb_map) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addPolygons(weight=1,
                  color="Grey",
                  fillOpacity=0.5,
                  fillColor=~qpal(mb_map$n),
                  popup=popup_info)  %>%
      leaflet::addLegend(position=c("bottomright"),
                pal=qpal,
                values=~mb_map$n,
                opacity=.5,
                title="# of Cases") %>%
      setView(lat=53.7609,lng=-98.8139,zoom=5)
    m
    })
  output$ageHist <- renderPlotly({
    ageData <- ageData %>% 
      mutate(prop=n/sum(n))
    ageData$prop <- round(ageData$prop,2)
    p <- ggplot(ageData,aes(text= paste0("<b> Age: </b>",Age, "<br>",
                                        "<b> # of Cases: </b>",n,"<br>",
                                        "<b> Proportion of Cases: </b> ",prop))) + 
      geom_bar(aes(x=Age,y=n),stat="identity",fill="lightblue") +
      theme_minimal() + 
      ylim(c(0,5)) +
      ylab("# of Cases") + 
      ggtitle("Age Distribution") +
      theme(plot.title = element_text(hjust=0.5))
    p %>% ggplotly(tooltip=c("text")) %>%
      style(hoverlabel=label) %>%
      config(displayModeBar = F)
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
  
  output$data.table <- renderDataTable({
    DT::datatable(table.data,
                  rownames = FALSE,
                  options = list(autoWidth=TRUE)
    )
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, "covid19-mb.csv", sep = "")
    },
    content = function(file) {
      write.csv(table.data, file, row.names = FALSE)
    }
  )
}
