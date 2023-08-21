#' Createa a running chat dialogue
#' @description This R6 class stores chat messages to be used in the future  calls, so gpt can retain 'memory' of the conversation. Additionally there are some helpers to display token usage and to reset the chat.
#' @export
#' @examples chat = Chat$new('List ingredients in spaghetti')
#'
Chat <- R6::R6Class("Chat"
  , public = list(
    start_time = Sys.time()
    , model = NULL
    , messages = NULL
    , functions = NULL
    , function_call = NULL
    , token = NULL
    , usr = "system"
    , latest_response = NULL
    , tokens = 0
    #' @description reset the chat, this removes all "memory" of the previous conversation.
    , reset = function() {self$messages = NULL; self$usr = "system"}


# Initialize --------------------------------------------------------------

    #' @description create a new chat session
    #' @param message the message you'd like to send
    #' @param payload additional post body options, usually the model
    #' @param token your openai auth token
    , initialize = function(message
                            , payload = list(model = "gpt-3.5-turbo")
                            , token = config::get()$openai) {
      self$model = payload$model
      self$functions = payload$functions
      self$function_call = payload$function_call
      self$token = token
      x = chats(list(list(role = "system", content = message)), payload, token = token)
      print(x)
      self$latest_response = x$choices$message$content
      self$messages = c(list(list(role = self$usr, content = message)), list(as.list(x$choices$message)))
      self$usr = "user"
      self$tokens = self$tokens + x$usage$total_tokens
    }


# Add Function ------------------------------------------------------------

    #' @description add a function to the payload functions
    #' @param name function name
    #' @param description what the function does
    #' @param parameters list of parameters the function returns
    , add_function = function(name, description = NULL, parameters) {
      f = list(
        name = name
        , description = description
        , parameters = list(type = "object", properties = parameters)
      )
      # browser()
      if (length(self$functions) == 0)
        self$functions = list()
      self$functions = append(self$functions, list(f))
    }


# Chat --------------------------------------------------------------------

    #' @description continue the chat conversation
    #' @param message the message you'd like to send
    , chat = function(message) {

      msg = c(self$messages, list(list(role = self$usr, content = message)))
      payload = list(model = self$model)
      if (length(self$functions) > 0)
        payload$functions = self$functions
      if (length(self$function_call) > 0)
        payload$function_call = self$function_call
      # browser()
      x = chats(msg, payload = payload
                , token = self$token)
      print(x)
      self$latest_response = x$choices$message

      if ("function_call" %in% names(x$choices$message)) {
        self$messages = c(
          self$messages,
          list(list(
              role = "function"
              , name = x$choices$message$function_call$name
              , content = x$choices$message$function_call$arguments))
          )

      } else {
        self$messages = c(
          self$messages
          , list(list(role = self$usr, content = message))
                          , list(as.list(x$choices$message)))
      }


      self$tokens = self$tokens + x$usage$total_tokens
    }


# Usage -------------------------------------------------------------------

    #' @description view the current token usage for this chat session
    , usage = function() {
      recent = paste("🧑", self$latest_response$usage$prompt_tokens
                     , "🖥️"  , self$latest_response$usage$prompt_tokens
                     , "Total:", self$latest_response$usage$total_tokens, collapse = " | ")
      glue::glue("Most Recent: {recent}\nTotal: {self$tokens}\n") |> cat()
    }
    )
)


