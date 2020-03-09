
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
  com_spl_2 <- stringr::str_split(com_spl[[1]][1], "=")
  
  x <- trimws(com_spl_2[[1]][1])
  y <- trimws(com_spl_2[[1]][2])
  z <- trimws(com_spl[[1]][2])
  
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
  
  summary(glm(formula_st, data = test_df))
  
}

stata_logit <- function(x) {
  
  vars <- substring(x, 7)
  
  vars_list <- str_split(vars, " ")
  
  covs <- paste0(vars_list[[1]][-1], collapse = " + ")
  
  formula_st <- as.formula(paste0("as.factor(", vars_list[[1]][1], ") ~ ", covs))
  
  summary(glm(formula_st, family=binomial(link='logit'), data = test_df))
  
}



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
  
  else {
    
    return(NULL)
    
  }

    
}
    
    
    


library(shiny)
library(shinyAce)
library(shinyWidgets)

init <- "use \"auto.dta\"

gen mpg_binary = 0
replace mpg_binary = 1 if mpg > 25

tab mpg_binary mpg

glm mpg price trunk weight
"

# Define UI for application that draws a histogram
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

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  values <- reactiveVal(NULL)
  
  
        observeEvent(input$eval, {
          
          old_values <- values()
          
          
          code_df <- parse_code(isolate(input$code)) %>%
            `colnames<-`(c("test"))
          
          print <- eval(lapply(code_df$test, stata2r))
          
          names(print) <- seq_along(print)
          
          print[sapply(print, is.null)] <- NULL
          
          new_str <- c(old_values,
                          print)
          
          values(new_str)
          
        })
        
        observeEvent(input$code_run_key, {
          
          old_values <- values()
          
          
          code_df <- parse_code(isolate(input$code)) %>%
            `colnames<-`(c("test"))
          
          print <- eval(lapply(code_df$test, stata2r))
          
          names(print) <- seq_along(print)
          
          print[sapply(print, is.null)] <- NULL
          
          new_str <- c(old_values,
                          print)
          
          values(new_str)
          
        })
        
  
        
  
        output$output <- renderPrint({
          
          if (is.null(values()))
            return(c("Welcome to stayta v. 0.0.0.0.0.1.", "Enjoy your stay ;)"))
          
          else(
            return(values())
          )

          

        })
        
        output$inp <- renderPrint({
          input$eval
          
          input$code
          
          #parse(text = isolate(input$code))
          
        })
        
        output$radio_vars <- renderUI({
          
          input$eval
          
          prettyRadioButtons(inputId = "radio_vars",
                             label = "Variable Info",
                             choices = colnames(test_df),
                             plain = TRUE, 
                             shape = "square",
                             bigger = TRUE)
          
        })
        
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
