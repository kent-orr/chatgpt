
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
             h3("Output")
             , tags$div(id="output")
             # , verbatimTextOutput("console")
             ) # end bottom column
    , column(12,
             h3("Input")
             ,tags$div(class="input", textAreaInput("prompt", "", width = "100%"))
             , actionButton("submit", "Ask", icon = icon("paper-plane"))
    ) # end top column
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

      # browser()

      # add text you sent
      insertUI("#output"
               , ui = tags$div(
                 class = "user"
                 , tags$p(input$prompt)
                 )
               , immediate = TRUE)

      # add waiting anim
      insertUI("#output"
               , ui = tags$div(
                 class = "response"
                 , id = "waiting"
                 , tags$p("...")
               )
               , immediate = TRUE)

      x = Chat$new(input$prompt)
      chat(x)
      out(c(out(), paste("OpenAI:", x$latest_response)))

      removeUI("#waiting", immediate = TRUE)
      insertUI("#output"
               , ui = tags$div(
                 class = "response"
                 , HTML(markdown::mark(x$latest_response, template = FALSE))
               )
               , immediate = TRUE)


      updateTextAreaInput(session, "prompt", value = "")

      # add response

      # browser()
    })

  }

  shinyApp(ui, server) |> runApp()
}
