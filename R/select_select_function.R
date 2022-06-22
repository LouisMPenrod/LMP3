#' Select which columns to put in dplyr::select()
#'
#' Opens a shiny app to help select and sort desired columns.
#' Drag and drop levels in the appropriate order.
#' Copy the code output and replace the select_select() call.
#'
#' @param data  A data frame or tibble.
#'
#' @return
#' @export
#'
#' @examples
#' mtcars %>% select_select()
#'
#'
select_select <- function(data){
  # Check data is a data.frame or tibble
  if(!"data.frame" %in% class(data)){
    stop("Object data must have a class data.frame (or tibble).")
  }

  # Check that there is more than one column
  if(length(data)==1){
    warning("Function unnecessary. Only one column exists in dataset provided.")
  }

  # Extract columns
  labels <- as.list(colnames(data))

  # Set up soratable rank_list
  rank_list_basic <- sortable::rank_list(
    text = "Drag the items to the desired order",
    labels = labels,
    input_id = "rank_list_basic"
  )


  rank_list_basic <- sortable::bucket_list(
    header = "Drag the items in any desired bucket",
    group_name = "bucket_list_group",
    orientation = "horizontal",
    sortable::add_rank_list(
      text = "Columns to remove",
      labels = labels,
      input_id = "rank_list_1"
    ),
    sortable::add_rank_list(
      text = "Columns to keep and arrange in order below",
      labels = NULL,
      input_id = "rank_list_2"
    ))

  # Start app
  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::fluidRow(

        # main page
        shiny::column(10, offset = 1,
                      shiny::tabPanel(
                        "Default",
                        shiny::tags$h1("select_select()"),
                        rank_list_basic,
                        shiny::br(),
                        shiny::tags$b("select Code"),
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

      # Main text generation
      text_out <- shiny::reactive({

        new_order_text <- paste0(input$rank_list_2, collapse=', ')
        text_out_code <- stringr::str_c("    select(", new_order_text,")")

        text_out <- paste(" %>%",text_out_code, sep="\n")

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
