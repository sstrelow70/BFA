# load required packages
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(DT)) install.packages("DT", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")


library(shiny)
library(shinythemes)
library(shinyWidgets)
library(lubridate)
library(ggplot2)
library(DT)
library(leaflet)
library(dplyr)
library(plotly)

# Choices for drop-downs
vars <- c(
  "MODIS"="m6",
  "VIIRS" = "v1"
)

vara <- c(
  "Archive"="archive",
  "NTR"="ntr"
)

#import data
bfa_inf2 <- read.csv("D:\\apps\\fire_archive_M6_96619.csv")
bfa_inf2_date <- bfa_inf2$acq_date
bfa2_min_date <- as.Date(min(bfa_inf2_date),"%Y-%m-%d")
bfa2_max_date <- as.Date(max(bfa_inf2_date),"%Y-%m-%d")
bfa_inf4 <- read.csv("D:\\apps\\fire_nrt_M6_96619.csv")
bfa_inf4_date <- bfa_inf4$acq_date
bfa4_min_date <- as.Date(min(bfa_inf4_date),"%Y-%m-%d")
bfa4_max_date <- as.Date(max(bfa_inf4_date),"%Y-%m-%d")
aa_data <- read.csv("D:\\apps\\fire_archive_V1_96617.csv", stringsAsFactors = FALSE )
aa_data <- data.frame(aa_data)
aa_data$latitude <-  as.numeric(aa_data$latitude)
aa_data$longitude <-  as.numeric(aa_data$longitude)
aa_data=filter(aa_data, latitude != "NA") # removing NA values
bb_data <- read.csv("D:\\apps\\fire_archive_M6_96619.csv", stringsAsFactors = FALSE )
bb_data <- data.frame(bb_data)
bb_data$latitude <-  as.numeric(bb_data$latitude)
bb_data$longitude <-  as.numeric(bb_data$longitude)
bb_data=filter(bb_data, latitude != "NA") # removing NA values
cc_data <- read.csv("D:\\apps\\fire_nrt_M6_96619.csv", stringsAsFactors = FALSE )
cc_data <- data.frame(cc_data)
cc_data$latitude <-  as.numeric(cc_data$latitude)
cc_data$longitude <-  as.numeric(cc_data$longitude)
cc_data=filter(cc_data, latitude != "NA") # removing NA values
dd_data <- read.csv("D:\\apps\\fire_nrt_V1_96617.csv", stringsAsFactors = FALSE )
dd_data <- data.frame(dd_data)
dd_data$latitude <-  as.numeric(dd_data$latitude)
dd_data$longitude <-  as.numeric(dd_data$longitude)
dd_data=filter(dd_data, latitude != "NA") # removing NA values

#function define
find_average_2 = function(circ,date){
  range1 = which(bfa_inf2_date == date)
  x = range1[1]
  y = range1[length(range1)]
  u = circ[x:y]
  ceil = mean(u)
  ceil = ceiling(ceil)
  ceil
}

find_average_4 = function(circ,date){
  range1 = which(bfa_inf4_date == date)
  x = range1[1]
  y = range1[length(range1)]
  u = circ[x:y]
  ceil = mean(u)
  ceil = ceiling(ceil)
  ceil
}

simplify_date_2 = function(start_date){
  range2 <- which(bfa_inf2_date == start_date)
  start_date_index = range2[1]
  range3 <- which(bfa_inf2_date == "2019-09-30")
  last_date_index = range3[length(range3)]
  new_date = bfa_inf2_date[start_date_index:last_date_index]
  new_date <- new_date[!duplicated(new_date)]
  new_date
}

simplify_date_4 = function(start_date){
  range2 <- which(bfa_inf4_date == start_date)
  start_date_index = range2[1]
  range3 <- which(bfa_inf4_date == "2020-01-11")
  last_date_index = range3[length(range3)]
  new_date = bfa_inf4_date[start_date_index:last_date_index]
  new_date <- new_date[!duplicated(new_date)]
  new_date
}

date_limit_2 = function(start_date){
  range2 <- which(bfa_inf2_date == start_date)
  start_date_index = range2[1]
  range3 <- which(bfa_inf2_date == "2019-09-30")
  last_date_index = range3[length(range3)]
  new_date = bfa_inf2[start_date_index:last_date_index,]
  new_date
}

date_limit_4 = function(start_date){
  range2 <- which(bfa_inf4_date == start_date)
  start_date_index = range2[1]
  range3 <- which(bfa_inf4_date == "2020-01-11")
  last_date_index = range3[length(range3)]
  new_date = bfa_inf4[start_date_index:last_date_index,]
  new_date
}

accord_2 = function(circ,date_set){
  date_set_average = vector()
  for (i in date_set){
    q = find_average_2(circ,i)
    date_set_average = append(date_set_average,q)
  }
  date_set_average
}

accord_4 = function(circ,date_set){
  date_set_average = vector()
  for (i in date_set){
    q = find_average_4(circ,i)
    date_set_average = append(date_set_average,q)
  }
  date_set_average
}


shinyServer(function(input,output,session){
  observe({
    instrumentBy <- input$instrument
    data_typeBy <- input$data_type
    if (instrumentBy == "m6") {
      if(data_typeBy == "archive"){
        colorData=bb_data
      }else{
        colorData=cc_data
      }
    }else{
      if(data_typeBy == "archive"){
        colorData=aa_data
      }else{
        colorData=dd_data
      }
    }
    
    colorData <- mutate(colorData, cntnt=paste0('<Embr><strong>scan:</strong> ', scan,
                                                '<br><strong>track:</strong> ', track,
                                                '<br><strong>acq_date:</strong> ',acq_date,
                                                '<br><strong>acq_time:</strong> ',acq_time,
                                                '<br><strong>satellite:</strong> ',satellite,
                                                '<br><strong>instrument:</strong> ',instrument,
                                                '<br><strong>confidence:</strong> ',confidence,
                                                '<br><strong>version:</strong> ',version,
                                                '<br><strong>frp:</strong> ',frp)) 
    
    
    #create a leaflet map
    output$bbmap <- renderLeaflet({
      leaflet(colorData) %>% 
        addCircles(lng =  ~longitude, lat =  ~latitude) %>% 
        addTiles() %>%
        addCircleMarkers(data = colorData, lat = ~latitude, lng =~longitude, 
                         radius = 3, popup = ~as.character(cntnt), 
                         color = "#03F",
                         stroke = FALSE, fillOpacity = 0.8)%>%
        addEasyButton(easyButton(
          icon="fa-crosshairs", title="ME",
          onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
    })
    #create a data object to display data
    output$data <-DT::renderDataTable(datatable(
      colorData[,c(1:2,4:11,13)],filter = 'top',
      colnames = c("latitude","longitude", "Scan", "Track", "Acq_data", "Acq_time", "Satellite","Instrument",
                   "Confidence","Vertion","Frp")
    ))
    
  })
  

  output$type_plot_trend_2 <- renderPlotly({
    if (input$data=="latitude"){ss = bfa_inf2$latitude}
    if (input$data=="longitude"){ss = bfa_inf2$longitude}
    if (input$data=="brightness"){ss = bfa_inf2$brightness}
    if (input$data=="scan"){ss = bfa_inf2$scan}
    if (input$data=="track"){ss = bfa_inf2$track}
    if (input$data=="acq_time"){ss = bfa_inf2$acq_time}
    if (input$data=="satellite"){ss = bfa_inf2$satellite}
    if (input$data=="instrument"){ss = bfa_inf2$instrument}
    if (input$data=="confidence"){ss = bfa_inf2$confidence}
    if (input$data=="version"){ss = bfa_inf2$version}
    if (input$data=="bright t31"){ss = bfa_inf2$bright_t31}
    if (input$data=="frp"){ss = bfa_inf2$frp}
    if (input$data=="daynight"){ss = bfa_inf2$daynight}
    x= simplify_date_2(input$minimum_date)
    y = accord_2(ss,x)
    df <- data.frame(x,y)
    ggplot(data = df,mapping = aes(x=x,y=y,group = 1)) + labs(x = "Date of bush fire",y = "selected outcome") +
      geom_line(color = "#045a8d") +
      theme_bw() +
      theme(axis.text.x=element_blank())
    
  })
  
  output$count_plot_2 <- renderPlotly({
    new_data = date_limit_2(input$minimum_date)
    if (input$data=="latitude"){ss = new_data$latitude}
    if (input$data=="longitude"){ss = new_data$longitude}
    if (input$data=="brightness"){ss = new_data$brightness}
    if (input$data=="scan"){ss = new_data$scan}
    if (input$data=="track"){ss = new_data$track}
    if (input$data=="acq_time"){ss = new_data$acq_time}
    if (input$data=="satellite"){ss = new_data$satellite}
    if (input$data=="instrument"){ss = new_data$instrument}
    if (input$data=="confidence"){ss = new_data$confidence}
    if (input$data=="version"){ss = new_data$version}
    if (input$data=="bright t31"){ss = new_data$bright_t31}
    if (input$data=="frp"){ss = new_data$frp}
    if (input$data=="daynight"){ss = new_data$daynight}
    ggplot(data = new_data,aes(x = ss))+
      labs(x = "select outcome",y = "count") +
      geom_histogram(bins = 50,fill = "#662506")+
      #stat_function(fun = "funss",geom = "line",colour = "#cc4c02")
      geom_density(size=1)
  })
  
  output$type_plot_trend_4 <- renderPlotly({
    if (input$data1=="latitude"){ss = bfa_inf4$latitude}
    if (input$data1=="longitude"){ss = bfa_inf4$longitude}
    if (input$data1=="brightness"){ss =bfa_inf42$brightness}
    if (input$data1=="scan"){ss = bfa_inf4$scan}
    if (input$data1=="track"){ss = bfa_inf4$track}
    if (input$data1=="acq_time"){ss = bfa_inf4$acq_time}
    if (input$data1=="satellite"){ss = bfa_inf4$satellite}
    if (input$data1=="instrument"){ss = bfa_inf4$instrument}
    if (input$data1=="confidence"){ss = bfa_inf4$confidence}
    if (input$data1=="version"){ss = bfa_inf4$version}
    if (input$data1=="bright t31"){ss = bfa_inf4$bright_t31}
    if (input$data1=="frp"){ss = bfa_inf4$frp}
    if (input$data1=="daynight"){ss = bfa_inf4$daynight}
    x= simplify_date_4(input$minimum_date1)
    y = accord_4(ss,x)
    df <- data.frame(x,y)
    ggplot(data = df,mapping = aes(x=x,y=y,group = 1)) + labs(x = "Date of bush fire",y = "selected outcome") +
      geom_line(color = "#045a8d") +
      theme_bw() +
      theme(axis.text.x=element_blank())
  })
  
  output$count_plot_4 <- renderPlotly({
    new_data = date_limit_4(input$minimum_date1)
    if (input$data1=="latitude"){ss = new_data$latitude}
    if (input$data1=="longitude"){ss = new_data$longitude}
    if (input$data1=="brightness"){ss = new_data$brightness}
    if (input$data1=="scan"){ss = new_data$scan}
    if (input$data1=="track"){ss = new_data$track}
    if (input$data1=="acq_time"){ss = new_data$acq_time}
    if (input$data1=="satellite"){ss = new_data$satellite}
    if (input$data1=="instrument"){ss = new_data$instrument}
    if (input$data1=="confidence"){ss = new_data$confidence}
    if (input$data1=="version"){ss = new_data$version}
    if (input$data1=="bright t31"){ss = new_data$bright_t31}
    if (input$data1=="frp"){ss = new_data$frp}
    if (input$data1=="daynight"){ss = new_data$daynight}
    ggplot(data = new_data,aes(x = ss))+
      labs(x = "select outcome",y = "count") +
      geom_histogram(bins = 50,fill = "#662506")+
      #stat_function(fun = "funss",geom = "line",colour = "#cc4c02")
      geom_density(size=1)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
})
