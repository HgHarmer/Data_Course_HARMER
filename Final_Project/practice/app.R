library(shiny)
library(tidyverse)
library(plotly)
nh <- read.csv('../nhdata.csv')
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
nh <- decode()
names <- unique(nh$student)
#want to add
#way to filter out subjects
#way to download graph 
#some sort of model(how does time of year impact grades? are different subjects impacted differently?)

# Define UI for miles per gallon app ----
ui <- fluidPage(
  
  
  titlePanel("test data"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    sidebarPanel(
      
      selectInput("variable", "student:",
                  names),
      
     
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Formatted text for caption ----
      h3(textOutput("caption")),
      
      # Output: Plot of the requested variable against mpg ----
      plotlyOutput("nhPlot")
      
    )
  ),
  
  grid_card(
    area = "a",
    card_body(
      checkboxGroupInput(
        inputId = "Checkbox",
        label = "Subject",
        choices = list("cp","directive","art",
                       "activity","spelling","math",
                       "reading","books","resets",
                       "pre-k","readingpassedoff","la",
                       "phonics","writing","rc",
                       "vocab","maps","countingmoney",
                       "money,fr,time","geography","handwriting",
                       "time","money","languagearts",
                       "hw","grammar","science",
                       "reset","readingcomp","geog",
                       "langarts,longvowels","sightwords",
                       "pre-handwriting","extramathpggiven","trashcans",
                       "redirects","testprep","algebra",
                       "geometry","longvowles","wordproblems",
                       "reallifeskills,100daymoney","money,fractime",
                       "workbook","testpractice","criticalthinking",
                       "shapes","testpractive","book",
                       "1-1positive","vocabulary","basicreading",
                       "mathalg,100days","lifeskillmath",
                       "promptforpill")
      )
    )
  )
)

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  # Compute the formula text ----
  # This is in a reactive expression since it is shared by the
  # output$caption and output$mpgPlot functions
  
  
  formulaText <- reactive({
    paste("student", input$variable)
  })
  
  
 
   
 output$nhPlot <- renderPlotly({
     
   
    p<- nh %>% 
      filter(student==input$variable) %>% 
    ggplot(aes(x=date,y=grade,color=subject,group=subject))+
      geom_point()+
      geom_line()
   
    ggplotly(p)
    
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)