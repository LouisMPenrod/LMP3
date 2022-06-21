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
  # Ensure column exists in data
  t <- try(data %>% dplyr::pull(var = !!dplyr::enquo(col)), silent = TRUE)
  if("try-error" %in% class(t)) stop("column provided does not exist")

  # enquo column name
  column_name <- dplyr::enquo(col)

  # Ensure column provided is either a factor or a character
  if(!is.factor(data %>% dplyr::pull(var = !!column_name)) &
     !is.character(data %>% dplyr::pull(var = !!column_name))){
    stop("column must be either a factor or a character")
  }

  # Ensure there is more than one unique value in the column
  if((data %>% dplyr::pull(var = !!column_name) %>% unique() %>% length())==1){
    warning("only one unique value exists in column provided")
  }

  # Extract levels
  labels <- as.list(levels(factor(data %>%
                                    dplyr::pull(var = !!column_name))))

  # Set up soratable rank_list
  rank_list_basic <- sortable::rank_list(
    text = "Drag the items to the desired order",
    labels = labels,
    input_id = "rank_list_basic"
  )

  # Start app
  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::fluidRow(

        # main page
        shiny::column(10, offset = 1,
                      shiny::tabPanel(
                        "Default",
                        shiny::tags$h1(stringr::str_glue("factor_sort for {dplyr::quo_name(column_name)}")),
                        rank_list_basic,
                        shiny::br(),
                        # shiny::tags$b("Result"),
                        # shiny::verbatimTextOutput("results_basic"),
                        # shiny::br(),
                        shiny::tags$b("Releveling Code"),
                        shiny::verbatimTextOutput("code")
                      )
        )



      ),

      rclipboard::rclipboardSetup(),

      # copy button
      shiny::fluidRow(
        shiny::column(6,offset = 1,shiny::uiOutput("clip")
        )),

      # close button
      shiny::fluidRow(
        shiny::column(3, offset = 9,shiny::actionButton("close","Done", class = "btn-warning")
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
      text_out <- shiny::reactive({

        new_order_text <- paste0('\"',input$rank_list_basic,'\"', collapse=',')
        text_out_code <- stringr::str_c("    mutate(",dplyr::quo_name(column_name)," = fct_relevel(",
                                        dplyr::quo_name(column_name),", levels = ",  new_order_text,"))")

        text_out <- paste(" %>%",text_out_code, sep="\n")

        # add convert to factor if data is character
        if(!is.factor(data %>%
                      dplyr::pull(var = !!column_name))){
          fct_text <- stringr::str_c("    mutate(",dplyr::quo_name(column_name)," = as_factor(",
                                     dplyr::quo_name(column_name),")) %>%")
          fct_text <- paste(" %>%",fct_text, sep="\n")
          text_out <- paste(fct_text,stringr::str_replace(text_out," %>%\n",""),sep = "\n")
        }

        # output text
        text_out
      })

      # code output
      output$code <- shiny::renderText({

        text_out()

      })

      # copy text when copy button pressed
      output$clip <- shiny::renderUI({

        rclipboard::rclipButton(
          inputId = "clipbtn",
          label = "Copy Code",
          clipText = text_out(),
          icon = shiny::icon("clipboard")
        )
      })

      shiny::observeEvent(input$close, shiny::stopApp())
    }


  )

}
