library(shiny)
library(plotly)
library(gridlayout)
library(bslib)
library(DT)
library(tidyverse)
library(lubridate)




#data frames
#nh <- read.csv('./NHapp/nhdata.csv')

nh <- read.csv('./nhdata.csv')
nh$date <- as_date(nh$date)

max <- max(nh$date)
min <- min(nh$date)

d3 <- nh %>% filter(student==670|student==508)
#names of 'behavior' subjects
b <- c('cp',"resets",'cyclecomp','cycleincomp')
subject <- unique(nh$subject)
p <- c('test',"test1")



ui <- grid_page(
  layout = c(
    "header header key",
    "student     plotly plotly",
    "student     plotly plotly ",
    "student     area3  area3   "
  ),
  row_sizes = c(
    "150px",
    "1.62fr",
    "0.72fr",
    "0.56fr"
  ),
  col_sizes = c(
    "265px",
    "0.59fr",
    "1.41fr"
  ),
#title 
  gap_size = "1rem",
  grid_card_text(
    area = "header",
    content = "NH Data",
    alignment = "start",
    is_title = FALSE
  ),
#shows interactive graph 
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
#password input
grid_card(
  area = "key",
  card_body(
    passwordInput(
      inputId = "password",
      label = "Enter password",
      
   ),
   actionButton("btn_action", "Submit")
  )
),
#pick student 
  grid_card(
    area = "student",
    card_body(
      uiOutput("Box1")
      
    )
    ,
    #option to filter between academic data or behavioral data
    radioButtons(inputId = 'b',
                label = '',
                 choices = c( 'academic'=TRUE,'behavior'=FALSE
                              ))
   
  
  ),
 #date range slider works better than the plotly provided option when switching between students 
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
  ),
 tags$style(type="text/css",
            ".shiny-output-error { visibility: hidden; }",
            ".shiny-output-error:before { visibility: hidden; }"
 )
)
#######################################################################
#######################################################################




server <- function(input, output) {
#give wrong password notification#################
  observeEvent(input$btn_action, {
 

  if(!input$password %in% p
       ){showNotification("Incorrect password")}
  
  })
 # display student picker based on password###################
  observeEvent(input$btn_action, {key <- input$password
    
     output$Box1 <-  renderUI({ 
   
      
      data <- if_else(key==p[1],TRUE, FALSE)
   if(data){names <- unique(nh$student)} 
    
 #potential code for adding parent specific data, likely to change for a less bulky option
    k3 <- if_else(key==p[2],TRUE, FALSE)
    if(k3){names <- unique(d3$student)} 
    
   

    selectInput("num","student",names)
    })

  
 #render plotly plot based on password############################    
   output$distPlot <- renderPlotly({
     
    
     
     data <- if_else(key==p[1],TRUE, FALSE)
    if (data) {
     
        ifelse(input$b,

      plot<-nh %>%
      filter(student==input$num,date>=input$range[1],date<=input$range[2]) %>%
      filter(!subject%in%b) %>%
      ggplot(aes(x=date,y=grade*100,color=subject,group=subject))+
      geom_point()+
      theme(axis.text.x = element_text(angle = 90,vjust = .5))+
      geom_line()+
      scale_x_date(date_breaks = '1 month',date_labels = '%B')+
      labs(y='Grade',x='Date',title =input$num )+
          theme_bw()
       ,
       plot<-nh %>%
         filter(student==input$num,date>=input$range[1],date<=input$range[2]) %>%
         filter(subject%in%b) %>%
         ggplot(aes(x=date,y=grade*30,color=subject,group=subject))+
         geom_point()+
         theme(axis.text.x = element_text(angle = 90,vjust = .5))+
         geom_line()+
         scale_x_date(date_breaks = '1 month',date_labels = '%B')+
         labs(y='occurances',x='Date',title =input$num )+
        theme_bw()


        )
      }
 
    
    k3 <- if_else(key==p[2],TRUE, FALSE)
    if (k3) {ifelse(input$b,
                    
                    plot<-d3 %>%
                      filter(student==input$num,date>=input$range[1],date<=input$range[2]) %>%
                      filter(!subject%in%b) %>%
                      ggplot(aes(x=date,y=grade*100,color=subject,group=subject))+
                      geom_point()+
                      theme(axis.text.x = element_text(angle = 90,vjust = .5))+
                      geom_line()+
                      scale_x_date(date_breaks = '1 month',date_labels = '%B')+
                      labs(y='Grade',x='Date',title =input$num )+
                      theme_bw()
                    ,
                    plot<-d3 %>%
                      filter(student==input$num,date>=input$range[1],date<=input$range[2]) %>%
                      filter(subject%in%b) %>%
                      ggplot(aes(x=date,y=grade*30,color=subject,group=subject))+
                      geom_point()+
                      theme(axis.text.x = element_text(angle = 90,vjust = .5))+
                      geom_line()+
                      scale_x_date(date_breaks = '1 month',date_labels = '%B')+
                      labs(y='occurances',x='Date',title =input$num )+
                      theme_bw()
    )
    }
    
  
    ggplotly(plot)
    })
  })
}
#################################################################################
#################################################################################
shinyApp(ui, server)
  

