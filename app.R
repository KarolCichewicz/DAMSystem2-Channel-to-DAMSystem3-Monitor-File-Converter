#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  #Java script for Google Analytics
  tags$head(includeScript("google-analytics.js")),
  
  
  
  fluidRow(column(12,
                  
                  tags$h1("DAMSystem2-Channel to DAMSystem3-Monitor File Converter", align = "center"),
                  tags$h4("ShinyR-DAM accepts only DAMSystem3 Monitor files. This converter provides backwards compatibility for users still using the old DAMSystem2 data acquisition software", align = "center"),
                  tags$h4("Karol Cichewicz, Jay Hirsh laboratory, University of Virginia, Charlottesville, VA", align = "center")             
          
                  )),
  
  
  sidebarPanel(
    
    
  fluidRow(column(6,
    fileInput('file1', 'Upload 32 Channel files', multiple = TRUE,
            accept=c('text/csv', 
                     'text/comma-separated-values,text/plain', 
                     '.csv'))
    ),
    column(4,
           tags$br(),
                  tags$head(
                    tags$style(HTML('#refresh_1{background-color:#67EB5E}'))
                  ),     
                  actionButton("refresh_1", "Start file conversion")
           
           ),
    column(4, "")
    
    
    ),
  
  fluidRow(column(12,
  textInput("Monitor_name", "Name your Monitor file", value = "Monitor_", width = NULL, placeholder = NULL))),
  
  tags$br(),
  tags$br(),
  
  tags$h4(strong("Reorder channel files if necessary")),
  
  fluidRow(column(12,
                  uiOutput("Order_channels")            
                  
                  ))
  ),
  
  
  fluidPage(
    
    tags$br(),
    tags$br(),
    
    column(8,
           tags$head(
             tags$style(HTML('#download_Monitor_file{font-size: 30px}')),
             tags$style(HTML('#download_Monitor_file{color:#1DB53B}'))
           ),     
           
           downloadLink("download_Monitor_file", "Download Monitor file.txt")   
    ),
    
    
  column(8,
    tags$br(),     
    tags$h3(" The first few rows of a Monitor file:")              
  ),
  
  column(8,
         fluidRow(
  tableOutput('Monitor_file')
  )),
  

  
  column(8,
         tags$h3("Handling status codes"),
         tags$h5("Status codes from the first channel are used for the whole monitor."),
         tags$h5("Please keep that in mind if you decide to combine channel files from multiple monitors.")
         ),
  column(8,
         h3('Contact information:'),
         tags$div(
           tags$p('Karol Cichewicz - kc3fb@virginia.edu, Jay Hirsh - jh6u@virginia.edu.'), 
           style = "font-size: 19px")
         
         
         )
  
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  library(shiny)
  library(plyr)
  library(dplyr)
  library(zoo)
  library(gtools)
  library(scales)
  library(lubridate)


  
  All_inputs <- reactive(reactiveValuesToList(input))
  
  
  
  
  Channel_desired_order <- eventReactive(input$refresh_1, {
    files <- input$file1
    p <- unlist(lapply(1:length(files$name), function(i){All_inputs()[[paste0("Channel_order",i)]]}))
    p
    
  })
  
  
  
  channels <- reactive(input$file1)
  
  

  df_sss <- reactive({
    function(z) {
      
      #Reads channel files    
      x <- data.frame(read.table(z, fill = T))
      
      
      #This extracts count data
      count_data <- x[5:nrow(x),1]
      
      #This constructs a vector containing all the time points
      first_reading <- as.numeric(paste(x[4,1]))
      interval <- as.numeric(paste(x[3,1]))
      m <- substr(first_reading, nchar(first_reading)-1, nchar(first_reading))
      h <- substr(first_reading, 0, nchar(first_reading)-2)
      h2 <-sub("^$", "0", h)
      
      hm <- paste0(h2, ':', m, ':', '00')
      
      
      first_day <- format(seq(from=as.POSIXct(paste("2013-01-01", hm), tz="GMT"),
                              to=as.POSIXct("2013-01-01 23:59:59", tz="GMT"),
                              by=paste(interval, 'min')), '%H:%M:%S')
      
      
      other_days <- format(seq(from=as.POSIXct(paste("2013-01-01 00:00:00"), tz="GMT"),
                               to=as.POSIXct("2013-01-01 23:59:59", tz="GMT"),
                               by=paste(interval, 'min')), '%H:%M:%S')
      
      
      #number of days in x. +1 is for an incomplete last day of the data. That day is later trimmed by restricting the length of a vector to the length of data [1:length(count_data)]
      number_of_days <- round(nrow(x)/(1440/interval)) + 1
      
      time_string <- c(first_day, rep(other_days, number_of_days))
      time_string_2 <- time_string[1:length(count_data)]
      
      
      #converting and reformating the first daily date
      
      day <- as.numeric(x[1,2])
      month <- as.character(x[1,3])
      year <- as.numeric(x[1,4])
      
      first_date<- as.Date(paste(day, month, paste0(year), sep="-"), "%d-%b-%Y")
      
      #The followin days are inferred from hte length of data and reading interval.
      other_dates <- lapply(1:number_of_days, function(i){
        rep(first_date + i, 1440/interval)
      })  
      
      date_vector <- c(rep(first_date, length(first_day)), as.Date(unlist(other_dates)))
      
      
      #This last incomplete day is trimmed based on the number of records
      date_vector_2 <- date_vector[1:(nrow(x)-4)]
      
      #
      days <- format(date_vector_2, "%d")
      months <- format(date_vector_2, "%b")
      years <- format(date_vector_2, "%y")
      
      
      #Error assesment 
      Status_codes <- ifelse(as.numeric(count_data) > 0, 1, as.numeric(count_data))
      
      #Index 
      Five <- rep(0, length(count_data))
      Six <- rep(0, length(count_data))
      Seven <- rep(0, length(count_data))
      Eight <- rep(0, length(count_data))
      Nine <- rep(0, length(count_data))
      Ten <- rep(0, length(count_data))
      
      #Five, Six, Seven, Eight, Nine, Ten,
      
      # This builds a df
      d_1 <- data.frame(date_vector_2, Time=as.character(time_string_2),
                        Counts=as.numeric(paste(count_data)), Status_codes
      )
      
      #colnames(d_1) <- NULL
      
      print(d_1)
      
    }
})
  
  channel_list <- reactive({
    fil <- channels()
    fil_ordered <- fil[match(Channel_desired_order(), fil$name),]
    lapply(fil_ordered$datapath, function(x) df_sss()(x))
    })
  
  
  
  
  
  
  monitor_data <- reactive({
    Reduce(function(x, y) merge(x, y, all=T, 
                                              by=c("date_vector_2", "Time")), channel_list(), accumulate=F)
  
  })
  
  days <-   reactive(format(as.Date(monitor_data()$date_vector_2,format="%Y-%m-%d"), "%d"))
  months <- reactive(format(as.Date(monitor_data()$date_vector_2,format="%Y-%m-%d"), "%b"))
  years <-  reactive(format(as.Date(monitor_data()$date_vector_2,format="%Y-%m-%d"), "%y"))
  
  Five <- reactive(rep(0, nrow(monitor_data())))
  Six <- reactive(rep(0, nrow(monitor_data())))
  Seven <- reactive(rep(0, nrow(monitor_data())))
  Eight <- reactive(rep(0, nrow(monitor_data())))
  Nine <- reactive(rep(0, nrow(monitor_data())))
  Ten <- reactive(rep(0, nrow(monitor_data())))
  
  
  monitor_data_2 <- reactive({
    withProgress(message = 'Converting channel files to monitor files', {
      incProgress(1, detail = paste("In progress")) 
    
    
    p <- data.frame(days(), months(), years(), monitor_data()$Time, status_codes=monitor_data()[,4],
               Five(), Six(), Seven(), Eight(), Nine(), Ten(), 
               monitor_data()[, c(3,5,7,9,11, 13, 15, 17, 19, 21, 23,25,27,29,31,33,35,37,39,41,43,45,47,49,51, 53, 55, 57,59,61,63,65)])
    
    p
    
    })
  })
  
  monitor_data_3 <- reactive({
    
    p <- monitor_data_2()
    colnames(p) <- NULL
    p
    
    
  })
    
  
  
  
  output$Order_channels <- renderUI({
    channel_files <- input$file1
    lapply(setdiff(1:length(channel_files$name), 0), function(i) {
      selectInput(paste0('Channel_order', i), paste('Channel file order:', i), channel_files$name, selected = channel_files$name[i])
    })
  })
  
  
  output$download_Monitor_file <- downloadHandler(

    
    filename = function() {
      paste(input$Monitor_name,"_" , Sys.Date(),  ".txt", sep="")
    },
    content = function(file) {
      write.table(monitor_data_3(), file, sep="\t")
    }
  )
  
  
  
  output$Monitor_file <- renderTable({
    
    All_inp <<- All_inputs()
    Chan_des <<- Channel_desired_order()
    
    if (is.null(input$file1))
      return(NULL)
    
    if (input$refresh_1 == 0)
      return()
    isolate({
    
    withProgress(message = 'Rendering monitor file table', {
      incProgress(1, detail = paste("In progress"))
      
    p <- monitor_data_2()
    colnames(p) <- NULL
    head(p)
    
    })
      
      
  
  })
    
    
  })
  
  
  
  
 
}

# Run the application 
shinyApp(ui = ui, server = server)

