#' Drag and drop to reorder levels of a factor.
#' Opens a shiny app to help resort levels of a factor.
#' Drag and drop levels in the appropriate order.
#' Copy the code output and replace the factor_sort() call.
#'
#' @param data A data.frame
#' @param col A factor or character vector
#'
#' @return
#' @export
#'
#' @examples
#' factor_sort(chickwts, col=feed)
#'
#'
#'
factor_sort <- function(data,col){
  require(shiny)
  require(sortable)
  require(rclipboard)
  require(tidyverse)

  # Ensure column exists in data
  t <- try(data %>% pull(var = !!enquo(col)), silent = TRUE)
  if("try-error" %in% class(t)) stop("column provided does not exist")

  # enquo column name
  column_name <- enquo(col)

  # Ensure column provided is either a factor or a character
  if(!is.factor(data %>% pull(var = !!column_name)) &
     !is.character(data %>% pull(var = !!column_name))){
    stop("column must be either a factor or a character")
  }

  # Ensure there is more than one unique value in the column
  if((data %>% pull(var = !!column_name) %>% unique() %>% length())==1){
    warning("only one unique value exists in column provided")
  }

  # Extract levels
  labels <- as.list(levels(factor(data %>%
                                    pull(var = !!column_name))))

  # Set up soratable rank_list
  rank_list_basic <- rank_list(
    text = "Drag the items to the desired order",
    labels = labels,
    input_id = "rank_list_basic"
  )

  # Start app
  shinyApp(
    ui = fluidPage(
      fluidRow(

        # main page
        column(10, offset = 1,
               tabPanel(
                 "Default",
                 tags$h1(str_glue("Reorder_factor for {quo_name(column_name)}")),
                 rank_list_basic,
                 br(),
                 # tags$b("Result"),
                 # verbatimTextOutput("results_basic"),
                 # br(),
                 tags$b("Releveling Code"),
                 verbatimTextOutput("code")
               )
        )



      ),

      rclipboardSetup(),

      # copy buttom
      fluidRow(
        column(6,offset = 1,uiOutput("clip")
        )),

      # close button
      fluidRow(
        column(3, offset = 9,actionButton("close","Done", class = "btn-warning")
        ))

    ),

    # Server
    server = function(input,output){

      # # Output for results section
      #
      # output$results_basic <- renderPrint({
      #   input$rank_list_basic
      # })

      # Main text generation
      text_out <- reactive({

        new_order_text <- paste0('\"',input$rank_list_basic,'\"', collapse=',')
        text_out_code <- str_c("    mutate(",quo_name(column_name)," = fct_relevel(",
                               quo_name(column_name),", levels = ",  new_order_text,"))")

        text_out <- paste(" %>%",text_out_code, sep="\n")

        # add convert to factor if data is character
        if(!is.factor(data %>%
                      pull(var = !!column_name))){
          fct_text <- str_c("    mutate(",quo_name(column_name)," = as_factor(",
                            quo_name(column_name),")) %>%")
          fct_text <- paste(" %>%",fct_text, sep="\n")
          text_out <- paste(fct_text,str_replace(text_out," %>%\n",""),sep = "\n")
        }

        # output text
        text_out
      })

      # code output
      output$code <- renderText({

        text_out()

      })

      # copy text when copy button pressed
      output$clip <- renderUI({

        rclipButton(
          inputId = "clipbtn",
          label = "Copy Code",
          clipText = text_out(),
          icon = icon("clipboard")
        )
      })

      observeEvent(input$close, stopApp())
    }


  )

}
