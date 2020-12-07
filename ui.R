library(shiny)
library(shinythemes)
library(shinyWidgets)
library(plotly)
library(ggplot2)

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
bfa_inf2 <- read.csv("D:\\apps\\fire_archive_V1_96617.csv")
bfa_inf2_date <- bfa_inf2$acq_date
bfa2_min_date <- as.Date(min(bfa_inf2_date),"%Y-%m-%d")
bfa2_max_date <- as.Date(max(bfa_inf2_date),"%Y-%m-%d")
bfa_inf4 <- read.csv("D:\\apps\\fire_nrt_V1_96617.csv")
bfa_inf4_date <- bfa_inf4$acq_date
bfa4_min_date <- as.Date(min(bfa_inf4_date),"%Y-%m-%d")
bfa4_max_date <- as.Date(max(bfa_inf4_date),"%Y-%m-%d")

ui <- bootstrapPage(
  navbarPage(
    theme = shinytheme("flatly"), collapsible = TRUE,"Bush fire in Australia",id="nav",
    tabPanel(h4("Australia mapper"),
             sidebarLayout(
               sidebarPanel(
                 h4("Map Explorer"),
                 helpText(tags$i(h6("The name of the dataset is the name of the satellite for MODIS, the pixel is about 1 km, for VIIRS, the pixel is about 375 metersThe name of the dataset is the name of the satellite for MODIS, the pixel is about 1 km, for VIIRS, the pixel is about 375 meters.")),style="color:#045a8d"),
                 selectInput("instrument", "Instrument", vars),
                 selectInput("data_type", "Data Type", vara),
                 helpText(tags$i(h5("The data output time may be a little long, please be patient.")),style="color:#662506")
               ),
               mainPanel(
                 leafletOutput("bbmap",height=500))
             )
    ),
    tabPanel(h4("Archive"),
             sidebarLayout(
               sidebarPanel(
                 helpText(tags$i(h6("This section will show the early spread of bushfires (2019.08.01 to 2019.09.30)")),style="color:#045a8d"),
                 helpText(tags$i(h6("Output line charts of different data over different time ranges to show data changes more intuitively.")), style="color:#045a8d"),
                 helpText(tags$i(h6("The following are the control widgets:")), style="color:#045a8d"),
                 pickerInput("data","outcome:",
                             choices = c("latitude","longitude","brightness","scan","track","acq_time","satellite","instrument","confidence","version","bright t31","frp","daynight"),
                             selected = "latitude",
                             multiple = FALSE
                             ),

                 sliderInput("minimum_date","minimum date:",
                             min = as.Date(bfa2_min_date,"%Y-%m-%d"),
                             max = as.Date(bfa2_max_date,"%Y-%m-%d"),
                             value = as.Date(bfa2_min_date),
                             timeFormat = "%d %b"
                             ),
                 helpText(tags$i(h6("select outcome and minimum date to update plot.")),style="color:#045a8d")
                 
                 
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Trend",plotlyOutput("type_plot_trend_2")),
                   tabPanel("Quantity statistics",plotlyOutput("count_plot_2"))
                 )
               )
             )
    ),
    tabPanel(h4("NRT"),
             sidebarLayout(
               sidebarPanel(
                 helpText(tags$i(h6("This section will show the late spread of bushfires (2019.10.01 to 2020.01.11)")), style="color:#045a8d"),
                 helpText(tags$i(h6("The following are the control widgets:")), style="color:#045a8d"),
                 pickerInput("data1","outcome:",
                             choices = c("latitude","longitude","brightness","scan","track","acq_time","satellite","instrument","confidence","version","bright t31","frp","daynight"),
                             selected = "latitude",
                             multiple = FALSE
                 ),
                 
                 sliderInput("minimum_date1","minimum date:",
                             min = as.Date(bfa4_min_date,"%Y-%m-%d"),
                             max = as.Date(bfa4_max_date,"%Y-%m-%d"),
                             value = as.Date(bfa4_min_date),
                             timeFormat = "%d %b"
                 ),
                 helpText(tags$i(h6("select outcome and minimum date to update plot.")),style="color:#045a8d"),
                 helpText(tags$i(h6("The data output time may be a little long, please be patient.")),style="color:#045a8d")
                 
                 
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Trend",plotlyOutput("type_plot_trend_4")),
                   tabPanel("Quantity statistics",plotlyOutput("count_plot_4"))
                 )
               )
             )
    ),
    tabPanel(h4("About Data"),
             verticalLayout(
               sidebarPanel(
                 #numericInput("maxrows", "Rows to show", 5),
                 helpText(tags$i(h6("You can choose to download files to your local files.")),style="color:#045a8d"),
                 downloadButton("downloadData", "Download")
               ),
               mainPanel(h3("Observations"),
                         DT::dataTableOutput("data"))
             )
             
    ),
    tabPanel(h4("About this site"),
             tags$div(
               tags$h4("Last update"),
               h6("11 January 2020"),
               "The purpose of this website is to supplement these resources by providing several interactive features that are not currently available elsewhere,
               including timeline functionality and the ability to cover past outbreaks.",
               tags$br(),tags$h4("Background"),
               "Bushfires in Australia are a frequent wildfire during Australia's hot and dry season.
               Large areas of land are destroyed every year, causing property damage and casualties.
               High temperatures and drought have been the main causes of wildfires since the country entered the forest fire season in July 2019, officials have announced. 
               From the most economically developed and densely populated south-east coasts of New South And Victoria, to Tasmania, Western Australia and the Northern Territory, almost every state has a forest fire burning.",
               tags$br(),
               "In isolation, the data can be un intuitive. How fast is the bushfire spreading? What's the trend? From this we hope to have a better understanding of the fire.",
               tags$br(),tags$h4("Code"),
               "Code and input data used to generate this Shiny mapping tool are available on ",tags$a(href="", "Github."),
               tags$br(),tags$h4("Sources"),
               "The data comes from Kaggle",tags$a(href="https://www.kaggle.com/carlosparadis/fires-from-space-australia-and-new-zeland","Kaggle."),
               tags$br(),tags$h4("Authors"),
               "LiAn Li,Central South University",tags$br(),
               "JiaXin Wang,Central South University"
             ))
  )
)