
library(haven)

stata_data <- function(x) {
  

    
    path_dta <- substring(x, 5)
    path_strip <- gsub("\"", "", path_dta)

  
    return(read_dta(path_strip))
}



stata2r <- function(x) {
    
    
  #need method to detect the first word after a colon (command)


    if (str_detect(x, "^use")) {
      
      stata_data(x)
        
        
        
    }  

    
}
    
    
    


library(shiny)
library(shinyAce)

init <- "df <- data.frame(
  num=1:4,
  let=LETTERS[2:5],
  rand=rnorm(4)
)
df"

# Define UI for application that draws a histogram
ui <-   fluidPage(
    h1("Stata editor"),
    fluidRow(
        column(
            6,
            h2("Source Code"),
            aceEditor("code", mode = "text", height = "200px", value = init),
            actionButton("eval", "Evaluate")
        ),
        column(
            6,
            h2("Output"),
            verbatimTextOutput("output")
            #verbatimTextOutput("inp")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
        output$output <- renderPrint({
            input$eval
          
          eval(stata_data(isolate(input$code)))
          
          
            #eval(stata_data(isolate(input$code)))
            #eval(parse(text = isolate(input$code)))
        })
        
        output$inp <- renderPrint({
          input$eval
          
          parse(text = isolate(input$code))
          
        })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
