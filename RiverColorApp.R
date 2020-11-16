
library(tidyverse)
library(viridis)
library(sf)
library(rgdal)
library(maps)
library(magrittr)
library(tmap)
library(lubridate)
library(mapview)
library(ggthemes)
library(leaflet)
library(tidyr)
library(leafpop)
library(scales)
library(shiny)
library(shinyalert)
library(shinybusy)
library(shinyjs)

### load data
flowline<- read_rds("D:/Dropbox/projects/RiverColorShiny/out/flowline_shiny.rds")

trend_annual <- read_rds("D:/Dropbox/projects/RiverColorShiny/out/trend_shiny.rds")

sum_ID <- read_rds("D:/Dropbox/projects/RiverColorShiny/out/sum_shiny.rds")

sum_ID_year <- read_rds("D:/Dropbox/projects/RiverColorShiny/out/sum_year_shiny.rds")

sum_ID_month <- read_rds("D:/Dropbox/projects/RiverColorShiny/out/sum_month_shiny.rds")

clust <- read_rds("D:/Dropbox/projects/RiverColorShiny/out/clust_shiny.rds")

riverSR <- read_rds("D:/Dropbox/projects/RiverColorShiny/out/riverSR_shiny.rds")


################################################
# Define UI for application 
ui <- fluidPage(
   
   # Application title
   titlePanel("The Color of US Rivers"),
   
   # figure out how to add info button
   
   
   
   # Show maps
    fluidRow(
      column(8, 
             selectInput("mapInput", "Map type", 
                         choices = c("Modal Color (nm)", "Trends", "Seasonality")),
                    
             leafletOutput(outputId = "map",  height = 600)),
      
   # plot long-term trend, seasonal patternd, and colro distribution when click on a river
    column(4, 
           plotOutput("plot_trend", height = 200),
           plotOutput("plot_season",   height = 200),
           plotOutput("plot_hist",  height = 200))    
   )
)


# Define server logic
server <- function(input, output, session) {

# reactivelt create data for drop-down menu for selecting map data
  #NOTE: figure out how to make this faster, maybe leafletProxy
map_out <- reactive({
  
  x <- input$mapInput
  
  if (is.null(x)) {
    
    return(flowline %>%
             inner_join(sum_ID,  by="ID") %>%
             mutate(trend = dw_mode1)) 
    
  } else if(x == "Modal Color (nm)") {
    
    return(flowline %>%
             inner_join(sum_ID,  by="ID") %>%
             mutate(trend = dw_mode1)) 
    
  } else if(x == "Trends") {
  
    return(flowline %>%
      left_join(trend_annual,  by="ID") %>%
        mutate(trend = ifelse(is.na(trend), "w/o enough data", trend)))
      
  } else if(x == "Seasonality") {
    
    return(flowline %>%
      left_join(clust, by="ID") %>%
        mutate(trend = ifelse(is.na(trend), "w/o enough data", trend)))
  }
  })
    

# make color palette reactive for each map
pal <- reactive({
  
  x <- input$mapInput
  
  if (is.null(x)) {
    
    pal<-  colorNumeric(
      palette = "viridis",
      domain = map_out()$trend)
    
  } else if(x == "Modal Color (nm)") {
    
    pal<-  colorNumeric(
      palette = "viridis",
      domain = map_out()$trend)
    
  } else if(x == "Trends") {
    
    pal<- colorFactor(
      palette = c("green3", "gold2", "darkmagenta","gray50", "grey90"),
      domain = map_out()$trend)

   #  pal<- colorFactor(
   #    palette = c("turquoise4", "orangered3", "darkmagenta","gray50", "grey90"),
    #   domain = map_out()$trend)
    
  } else if(x == "Seasonality") {
    pal <- colorFactor(
      palette = c("green3","darkmagenta","darkorange1", "grey90"),
      domain = map_out()$trend)
  }
}) 

# plot map 
  output$map <- renderLeaflet({
       leaflet(map_out()) %>%
       addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Esri.WorldGrayCanvas") %>%
       addProviderTiles(providers$CartoDB.DarkMatter, group = "DarkMatter (CartoDB)") %>%
       addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri.WorldTopoMap") %>%
       addProviderTiles(providers$Esri.WorldImagery, group = "Esri.WorldImagery") %>%
       addLayersControl(baseGroups = c("Esri.WorldGrayCanvas", "DarkMatter (CartoDB)", "Esri.WorldTopoMap", 
                                       "Esri.WorldImagery"),
                        options = layersControlOptions(collapsed = TRUE, autoZIndex = T)) %>%
      setView(zoom=3.5, lat=42, lng=-98) %>%
       addPolylines(data=map_out(),
                    color = ~pal()(trend),
                    layerId = ~ID,
                    opacity=1,
                    weight=2)  %>%
       addLegend("bottomleft", pal=pal(), values = ~trend, title="", opacity = 1)
  })

  
###################
   
# generate reactive data for ggplots

# trends
  ggplot_trend <- reactive({
    
    site <- input$map_shape_click$id
    
    # give plot some default data to plot on opening
    if (is.null(site)) {
      return(sum_ID_year[sum_ID_year$ID == 26739,])
      # make plot reactive to clicks  
    } else {
      return(sum_ID_year[sum_ID_year$ID == site,])
    }
  })

# seasonal pattern
  ggplot_season <- reactive({
    
    site <- input$map_shape_click$id
    
    # give plot some default data to plot on opening
    if (is.null(site)) {
      return(sum_ID_month[sum_ID_month$ID == 26739, ])
    # make plot reactive to clicks  
    } else{
      return(sum_ID_month[sum_ID_month$ID == site, ])
    }
  })

# full color distribution 
  ggplot_hist <- reactive({
    
    site <- input$map_shape_click$id
    
    # give plot some default data to plot on opening
    if (is.null(site)) {
      return(riverSR[riverSR$ID == 26739, ])
      # make plot reactive to clicks  
    } else{
      return(riverSR[riverSR$ID == site, ])
    }
  })

     output$plot_trend <- renderPlot({
       
    # color options
       cols <- c("Blue-shifted" = "green3", "Red-shifted"= "gold2", "Steady"="darkmagenta", 
                 "Variable"= "gray50", "w/o enough data" ="grey90")

      # cols <- c("Blue-shifted" = "turquoise4", "Red-shifted"= "orangered3", "Steady"="darkmagenta", 
       #          "Variable"= "gray50", "w/o enough data" ="grey90")
       
       ggplot()+
         geom_point(data= ggplot_hist(),
                    aes(x=date, y=dw), color="lightgrey", alpha=0.5, size=1)  +
         geom_line(data = ggplot_trend(),
                   aes(x=as.Date(paste(as.character(year), 6, 1, sep = "-")), y=dw_mean, color=as.character(trend)), size=1.5)  +
         scale_x_date(breaks = as.Date(c("1985-01-01", "2000-01-01", "2015-01-01")),
                      date_labels = "%Y") +
         scale_color_manual(values=cols, name="") +
         theme_few() +
         ylab(expression(lambda~(nm))) +
         xlab("Year") +
         theme(legend.position = c(0.7, 0.98),
               legend.background = element_blank(),
               axis.text = element_text(size=14),
               axis.title = element_text(size=14),
               legend.text = element_text(size=11)) +
         ggtitle("Long-term trend")
     }) 
     
     output$plot_season <- renderPlot({

       cols2 <- c("Summer red-shift" = "darkorange1",  "Spring red-shift"="darkmagenta", 
                 "Aseasonal"= "springgreen3", "w/o enough data" ="grey90")
       
       ggplot(data = ggplot_season())+
         geom_point(aes(x=month, y=dw_mean, color=as.character(trend)), size=2.5)  +
         geom_smooth(method="loess", aes(x=month, y=dw_mean, color=as.character(trend) ), se=F)  +
         scale_color_manual(values=cols2, name="") +
         scale_x_continuous(breaks=seq(1,12,1), name = "Month") +
         theme_few() +
         ylab(expression(Mean~lambda~(nm))) +
         theme(legend.position = c(0.7, 0.98),
               legend.background = element_blank(),
               axis.text = element_text(size=14),
               axis.title = element_text(size=14),
               legend.text = element_text(size=11)) +
         ggtitle("Seasonal pattern")
     }) 
     
     output$plot_hist <- renderPlot({
       
       ggplot(data = ggplot_hist())+
         geom_histogram(aes(dw), fill="grey", color="black")  +
         xlim(450,600) +
         theme_few() +
         ylab("Count") +
         xlab(expression(lambda~(nm))) +
         theme(axis.text = element_text(size=14),
              axis.title = element_text(size=14)) +
         ggtitle("Color Distribution")
     })
     
}

# Run the application 
shinyApp(ui = ui, server = server)



