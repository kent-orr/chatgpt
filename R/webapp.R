
prompt_shiny <- function() {
  ui <- fluidPage(
    tags$head(tags$script("
      Shiny.addCustomMessageHandler('jsSubmit', function(message) {
        Shiny.setInputValue('submit'
        , Shiny.shinyapp.$inputValues['submit:shiny.action'] += message
        , {priority: 'event'});
      });
                          "))
    , useKeys()
    , keysInput("keys", "ctrl+enter", TRUE)
    , column(12,
           h3("Input")
           ,tags$div(class="input", textAreaInput("prompt", "", width = "100%"))
           , actionButton("submit", "Ask", icon = icon("paper-plane"))
           ) # end top column
    , column(12,
             h3("Output")
             , tags$div(class="output", uiOutput("output"))
             , verbatimTextOutput("console")
             ) # end bottom column
  )

  server <- function(input, output, session) {

    output$console <- renderPrint({
      reactiveValuesToList(input)
    })

    i = reactiveVal(0)
    out <- reactiveVal()
    chat = reactiveVal()

    observeEvent(input$keys, {
      if (input$keys == "ctrl+enter") {
        session$sendCustomMessage("jsSubmit", 1)
      }
    })

    observeEvent(input$submit, ignoreInit = TRUE, once = FALSE, {
      out(c(out(), paste("You:", input$prompt)))
      x = Chat$new(input$prompt)
      chat(x)
      out(c(out(), paste("OpenAI:", x$latest_response)))
      updateTextAreaInput(session, "prompt", value = "")
    })

    output$output <- renderUI({
      tags$pre(
        tags$code(
          paste(out(), collapse = "\n")
        )
      )
      })

  }

  shinyApp(ui, server) |> runApp()
}
