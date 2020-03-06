
library(haven)
library(stringr)
library(magrittr)
library(data.table)

stata_data <- function(x) {
  

    
    path_dta <- substring(x, 5)
    path_strip <- gsub("\"", "", path_dta)

  
    return(read_dta(path_strip))
}


parse_code <- function(x) {
  
  code <- as.data.frame(stringr::str_split(x, "\n"))
  
  return(code)
  
  
}

tab <- function(x) {
  
  vars <- substring(x, 5)
  
  vars_list <- str_split(vars, " ")
  
  
  
  if (length(vars_list[[1]])>1) {
    
    vars_eval <- paste0("as_factor(test_df$", vars_list[[1]], ")")
    
    vars_eval_list <- paste0(vars_eval, collapse = ", ")
    
    val <- eval(parse(text = paste0("table(", vars_eval_list, ")" )))
    
    return(val)
    
  }
  
  else {
    
    tib <- as.data.frame(rbind(table(as_factor(test_df[[vars_list[[1]]]])), round(prop.table(table(as_factor(test_df[[vars_list[[1]]]]))), digits = 7)))
    
    
    
    return(tib)
    
  }
  
}

gen <- function(x) {
  
  com <- substring(x, 5)
  
  com_spl <- stringr::str_split(com, "=")
  
  x <- trimws(com_spl[[1]][1])
  y <- trimws(com_spl[[1]][2])
  
  if (y==".") {
    y <- NA
    
  }
  
  test_df[[x]] <<- y
  
  
  
}

replace <- function(x) {
  com <- substring(x, 9)
  
  com_spl <- stringr::str_split(com, "if")
  com_spl_2 <- stringr::str_split(com_spl[[1]], "=")
  
  x <- trimws(com_spl_2[[1]][1])
  y <- trimws(com_spl_2[[1]][2])
  z <- trimws(com_spl_2[[2]][1])
  
  #replace y = 1 if x > 3
  
  #test_df[["newvar"]][test_df[["mpg"]] > 20] <- 1
  eval(parse(text = paste0("test_df$", x, "[", "test_df$", z, "]", "<<-", y)))
  #test_df[[x]][eval(parse(paste0("test_df$", z)))] <- y
  
}

stata_glm <- function(x) {
  
  vars <- substring(x, 5)
  
  vars_list <- str_split(vars, " ")
  
  covs <- paste0(vars_list[[1]][-1], collapse = " + ")
  
  formula_st <- as.formula(paste0(vars_list[[1]][1], " ~ ", covs))
  
  glm(formula_st, data = test_df)
  
}



stata2r <- function(x) {
    
    
  #need method to detect the first word after a colon (command)
  
  if (is.null(x)) {
    
    return(NULL)
  }


    if (str_detect(x, "^use")) {
      
      test_df <<- stata_data(x)
      
      return(test_df)
        
        
        
    }
  
    if (str_detect(x, "^tab")) {
      
      test_table <- tab(x)
      
      return(test_table)
      
    }
  
  if (str_detect(x, "^glm")) {
    
    test_glm <- stata_glm(x)
    
    return(test_glm)
    
  }
  
  if (str_detect(x, "^gen")) {
    
    gen(x)
    
  }
  
  if (str_detect(x, "^replace")) {
    
    replace(x)
    
  }

    
}
    
    
    


library(shiny)
library(shinyAce)

init <- "use \"auto.dta\""

# Define UI for application that draws a histogram
ui <-   fluidPage(
    h1("Stayta editor"),
    fluidRow(
      column(
        6,
        h2("Do-file Editor"),
      ),
      column(
        6,
        h2("Output"),
      )
      
      
    ),
    fluidRow(
        column(
            6,
            aceEditor("code", mode = "text", height = "600px", value = init),
            style = "overflow-y:scroll; max-height: 620px"
        ),
        column(
            6,
            verbatimTextOutput("output"),
            style = "overflow-y:scroll; max-height: 620px"
            #,
            #verbatimTextOutput("inp")
        )
    ),
    
    fluidRow(
      column(
        6,
        br(),
        actionButton("eval", "Evaluate"),

      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
        output$output <- renderPrint({
            input$eval
          
          #eval(stata2r(isolate(input$code)))
          
          code_df <- parse_code(isolate(input$code)) %>%
            `colnames<-`(c("test"))
          
          print <- eval(lapply(code_df$test, stata2r))
          
          names(print) <- seq_along(print)
          
          print[sapply(print, is.null)] <- NULL
          
          return(print)
          

        })
        
        output$inp <- renderPrint({
          input$eval
          
          input$code
          
          #parse(text = isolate(input$code))
          
        })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
