library(shiny)
library(plotly)
library(gridlayout)
library(bslib)
library(DT)
library(tidyverse)
library(googlesheets4)
library(lubridate)

nh <- read.csv('./nhdata.csv')
nh$date <- as_date(nh$date)
subject <- unique(nh$subject)
decode <- function(y=nh){
  p<-gs4_find(order_by='createdTime desc')
  t <- length(row.names(p))
  #read in names from drive 
  
  names<-which(p$name =='Names')
  names<-read_sheet(p[[3]][[names]]$webViewLink)
  names<- names$Names
  n_names <- length(names)
  set.seed(57)
  rnumb <- sample(n_names)
  code <- data.frame(names,as.character(rnumb))
  y$student <- as.character(y$student)
  #decode
  for (x in 1:n_names) {
    y <- y%>% 
      mutate(student=case_when(student==code[x,2]~code[x,1],TRUE~student))  
  }
  return(y)
}
#nh<- decode()
#if making new page for individual parents names <- c(student name)
names <- unique(nh$student)
max <- max(nh$date)
min <- min(nh$date)

ui <- grid_page(
  layout = c(
    "header header header",
    "a      plotly plotly",
    ".      area3  area3 ",
    ".      .      .     "
  ),
  row_sizes = c(
    "100px",
    "1.62fr",
    "0.72fr",
    "0.56fr"
  ),
  col_sizes = c(
    "265px",
    "0.59fr",
    "1.41fr"
  ),
  gap_size = "1rem",
  grid_card_text(
    area = "header",
    content = "NH",
    alignment = "start",
    is_title = FALSE
  ),
  grid_card(
    area = "plotly",
    card_body(
      plotlyOutput(
        outputId = "distPlot",
        width = "100%",
        height = "100%"
      )
    )
  ),
  grid_card(
    area = "a",
    card_body(
      area = "a",
      selectInput(
        inputId = "variable",
        label = "student:",
        choices = names
      )
    )
  ),
  grid_card(
    area = "area3",
    card_body(
      sliderInput(
        min = min,
        max = max,
        value = c(min,max),
        inputId = "range",
        label = "Date Range:"
      )
    )
  )
)


server <- function(input, output) {
   
  output$distPlot <- renderPlotly({
    
    p<- nh %>% 
      filter(student==input$variable,date>=input$range[1],date<=input$range[2]) %>% 
      ggplot(aes(x=date,y=grade*100,color=subject,group=subject))+
      geom_point()+
      theme(axis.text.x = element_text(angle = 90,vjust = .5))+
      geom_line()+
      scale_x_date(date_breaks = '1 month',date_labels = '%B')+
      labs(y='Grade',x='Date',title =input$variable )
    
    ggplotly(p)
    
    
    
  })
  
 
}

shinyApp(ui, server)
  

