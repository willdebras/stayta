#Stayta is a project to parse and evaluate stata code into R
#This application allows one to run stata code in a text editor and receive R output


#Load necessary libraries for parsing functions
library(haven) #reads .dta files
library(stringr) #for parsing and manipulating stata commands as text
library(magrittr) #for piping functions together
library(data.table) #for fast, efficient manipulation of data

#Function that parses "use x" type commands 
stata_data <- function(x) {
  

    
    path_dta <- substring(x, 5)
    path_strip <- gsub("\"", "", path_dta)

  
    return(read_dta(path_strip))
}


#Function that takes input of text editor and converts it into line by line commands
#To add: splitting and parsing of , options
parse_code <- function(x) {
  
  code <- as.data.frame(stringr::str_split(x, "\n"), stringsAsFactors = FALSE)
  
  
  return(code)
  
  
}

#Function that parses and evaluates "tab x y" and "tab x" inputs
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

#Function that parses and evaluates "gen x" inputs
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

#Function that parses and evaluates "replace x" inputs
replace <- function(x) {
  com <- substring(x, 9)
  
  com_spl <- stringr::str_split(com, "if")
  com_spl_2 <- stringr::str_split(com_spl[[1]][1], "=")
  
  x <- trimws(com_spl_2[[1]][1])
  y <- trimws(com_spl_2[[1]][2])
  z <- trimws(com_spl[[1]][2])
  
  #replace y = 1 if x > 3
  
  #test_df[["newvar"]][test_df[["mpg"]] > 20] <- 1
  eval(parse(text = paste0("test_df$", x, "[", "test_df$", z, "]", "<<-", y)))
  #test_df[[x]][eval(parse(paste0("test_df$", z)))] <- y
  
}

#Functions to parse model inputs:

#Function to parse "glm dependent cov_x cov_y" inputs
stata_glm <- function(x) {
  
  vars <- substring(x, 5)
  
  vars_list <- str_split(vars, " ")
  
  covs <- paste0(vars_list[[1]][-1], collapse = " + ")
  
  formula_st <- as.formula(paste0(vars_list[[1]][1], " ~ ", covs))
  
  summary(glm(formula_st, data = test_df))
  
}

#Function to parse "logit dependent cov_x cov_y" inputs
stata_logit <- function(x) {
  
  vars <- substring(x, 7)
  
  vars_list <- str_split(vars, " ")
  
  covs <- paste0(vars_list[[1]][-1], collapse = " + ")
  
  formula_st <- as.formula(paste0("as.factor(", vars_list[[1]][1], ") ~ ", covs))
  
  summary(glm(formula_st, family=binomial(link='logit'), data = test_df))
  
}


#Function that detects the stata input and returns R output
stata2r <- function(x) {
    
    
  #need method to detect the first word after a colon (command)
  
  if (is.null(x)) {
    
    return(NULL)
  }


    if (str_detect(x, "^use")) {
      
      test_df <<- stata_data(x)
      as_factor(test_df)
      
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
  
  if (str_detect(x, "^logit")) {
    
    test_logit <- stata_logit(x)
    
    return(test_logit)
    
  }
  
  if (str_detect(x, "^gen")) {
    
    gen(x)
    
    return(paste0(x))
    
  }
  
  if (str_detect(x, "^replace")) {
    
    replace(x)
    
    return(paste0(x))
    
  }
  
  if (str_detect(x, "^pwd")) {
    
    getwd()
    
  }
  
  #If line is empty, return NULL
  
  if (str_detect(x, "^$")) {
    
    return(NULL)
    
  }
  
  #If line is meaningless (e.g. an error) or is not a feature yet, return error message
  else {
    
    err <- str_split_fixed(x, " ", n = 2)[1]
    
    return(paste0("The function `", err, "` is incorrect or not available in stayta v.0.0.0.0.1"))
    
  }

    
}
    
    
    

#Load shiny libraries
library(shiny)
library(shinyAce)
library(shinyWidgets)


#Add example stata code to evaluate

init <- "use \"auto.dta\"

gen mpg_binary = 0
replace mpg_binary = 1 if mpg > 25

tab mpg_binary mpg

glm mpg price trunk weight
"

# Define UI of application with a do file editor via aceEditor, an output section, a list of variables, and a variable explanation/breakdown

ui <-   fluidPage(
    h1("Stayta editor"),
    fluidRow(
      column(
        5,
        h2("Do-file Editor"),
      ),
      column(
        4,
        h2("Output"),
      )
      
      
    ),
    fluidRow(
        column(
            5,
            aceEditor("code", mode = "text", height = "600px", value = init,
                      selectionId = "selection",
                      cursorId = "cursor",
                      hotkeys = list(
                        run_key = list(win = "Ctrl-D",
                                       mac = "CMD-D")
                        
                      )),
            style = "overflow-y:scroll; max-height: 620px"
        ),
        column(
            4,
            verbatimTextOutput("output"),
            style = "overflow-y:scroll; max-height: 620px"
            #,
            #verbatimTextOutput("inp")
        ),
        column(
          1,
          uiOutput("radio_vars")
        ),
        column(
          2,
          verbatimTextOutput("var_info"),
          style = "overflow-y:scroll; max-height: 620px"
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

# Define server logic

server <- function(input, output, session) {
  
  
  values <- reactiveVal(NULL)
  
  #If we observe a press to evaluate or a hotkey to evaluate, paste old output to evaluated new output to have a continous output
  
        observeEvent(input$eval, {
          
          old_values <- values()
          
          
          #If there is no code highlighted/selected and cursor is not in the editor, evaluate it all on evaluate button  
          
            if (is.null(input$code_selection)) {
              
              code_df <- parse_code(isolate(input$code)) %>%
                `colnames<-`(c("test"))
              
              print <- eval(lapply(code_df$test, stata2r))
              
              names(print) <- seq_along(print)
              
              print[sapply(print, is.null)] <- NULL
              
              new_str <- c(old_values,
                           print)
              
              values(new_str) 
              
            }
          
          #If cursor is in the editor, but the highlighted code is empty, evaluate it all on evaluate button
          
          else if (str_detect(isolate(input$code_selection), "^$")) {
            
            code_df <- parse_code(isolate(input$code)) %>%
              `colnames<-`(c("test"))
            
            print <- eval(lapply(code_df$test, stata2r))
            
            names(print) <- seq_along(print)
            
            print[sapply(print, is.null)] <- NULL
            
            new_str <- c(old_values,
                         print)
            
            values(new_str) 
            
          }
            
        
          #If cursor is in the editor and is highlighting code, only evaluate the highlighted code when evaluate button is pressed
          
          else {
            
            code_df <- parse_code(input$code_selection) %>%
              `colnames<-`(c("test"))
            
            print <- eval(lapply(code_df$test, stata2r))
            
            names(print) <- seq_along(print)
            
            print[sapply(print, is.null)] <- NULL
            
            new_str <- c(old_values,
                         print)
            
            values(new_str) 
            
          }
          
        })
        
        #Mimick the same for hot keys:
        #ctr-D will evaluate all if no selection or just the line if highlighted code selection
        
        observeEvent(input$code_run_key, {
          
          old_values <- values()
          
          if (is.null(input$code_selection)) {
            
            code_df <- parse_code(isolate(input$code)) %>%
              `colnames<-`(c("test"))
            
            print <- eval(lapply(code_df$test, stata2r))
            
            names(print) <- seq_along(print)
            
            print[sapply(print, is.null)] <- NULL
            
            new_str <- c(old_values,
                         print)
            
            values(new_str) 
            
          }
          
          else if (str_detect(isolate(input$code_selection), "^$")) {
            
            code_df <- parse_code(isolate(input$code)) %>%
              `colnames<-`(c("test"))
            
            print <- eval(lapply(code_df$test, stata2r))
            
            names(print) <- seq_along(print)
            
            print[sapply(print, is.null)] <- NULL
            
            new_str <- c(old_values,
                         print)
            
            values(new_str) 
            
          }
          
          
          
          else {
            
            code_df <- parse_code(input$code_selection) %>%
              `colnames<-`(c("test"))
            
            print <- eval(lapply(code_df$test, stata2r))
            
            names(print) <- seq_along(print)
            
            print[sapply(print, is.null)] <- NULL
            
            new_str <- c(old_values,
                         print)
            
            values(new_str) 
            
          }
          
          
        })
        
  
        #Print the output
  
        output$output <- renderPrint({
          
          if (is.null(values()))
            return(c("Welcome to stayta v. 0.0.0.0.0.1.", "Enjoy your stay ;)"))
          
          else(
            return(values())
          )

          

        })
        
        #Printable for debugging to see the selection
        
        output$inp <- renderPrint({
          
          
          input$eval
          
          input$code
          
          #parse(text = isolate(input$code))
          
        })
        
        #Render a list of buttons to see variable info based on dataset stored as "test_df"
        # "use x.dta" will load these radio buttons
        
        output$radio_vars <- renderUI({
          
          input$eval
          
          prettyRadioButtons(inputId = "radio_vars",
                             label = "Variable Info",
                             choices = colnames(test_df),
                             plain = TRUE, 
                             shape = "square",
                             bigger = TRUE)
          
        })
        
        # Render information about the variable selected by the radio button
        # This wil render attributes and class information about that variable
        
        output$var_info <- renderPrint({
          
          input$radio_vars
          
          print_info <- list(eval(parse(text = paste0("attributes(test_df$", isolate(input$radio_vars), ")"))),
                             eval(parse(text = paste0("class(test_df$", isolate(input$radio_vars), ")")))
          )
          
          return(print_info)
          
        })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
