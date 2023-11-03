#' Create a a running chat dialogue
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
    , reset = function() {self$messages = NULL; self$usr = "system"},


# Initialize --------------------------------------------------------------

    #' @description create a new chat session
    #' @param message the message you'd like to send
    #' @param payload additional post body options, usually the model
    #' @param token your openai auth token
    initialize = function(message
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
    },


# Add Function ------------------------------------------------------------

    #' @description add a function to the payload functions
    #' @param name function name
    #' @param description what the function does
    #' @param parameters list of parameters the function returns
    add_function = function(name, description = NULL, parameters) {
      f = list(
        name = name
        , description = description
        , parameters = list(type = "object"
                            , properties = parameters)
               )
      # browser()
      if (length(self$functions) == 0)
        self$functions = list()
      self$functions = append(self$functions, list(f))
    },


# Call a Function ---------------------------------------------------------

    #' call_function method for calling predefined functions
    #'
    #' This method sends a request to call a predefined function in the model.
    #' It constructs a payload that includes the function call and its arguments,
    #' then sends this as part of the chat messages to the API.
    #'
    #' @param name The name of the function to call.
    #' @param arguments A list of arguments for the function call (default is an empty list).
    #' @return The updated Chat object, reflecting the latest state after the function call.
    #' @export
    call_function = function(name, msg) {
      if (!is.character(name) || nchar(name) == 0) {
        stop("A valid function name must be provided as a non-empty string.", call. = FALSE)
      }

      # browser()

      # Construct the function call payload
      self$function_call <- list(name = name)
      # if (length(arguments) > 0) {
      #   self$function_call$arguments <- arguments
      # }

      # Add the function call to the chat payload
      msg <- list(list(role = "user", content = msg))

      # Create the payload for the API request
      payload <- list(
        model = self$model,
        messages = c(self$messages, msg)
      )

      if (!is.null(self$functions)) {
        payload$functions <- self$functions
      }

      # Perform the API call
      x <- chats(payload$messages, payload = payload, token = self$token)
      print(x)

      # return functions payload to default for future chats
      self$function_call <- NULL

      # Update the latest response and messages history
      self$latest_response <- x$choices$message$content
      self$messages <- c(self$messages, list(as.list(x$choices$message)))
      self$tokens <- self$tokens + x$usage$total_tokens

      # Return the updated Chat object
      return(self)
    },

# Chat --------------------------------------------------------------------

    #' @description continue the chat conversation
    #' @param message the message you'd like to send
    chat = function(message) {

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

      return(self)
    },


# Usage -------------------------------------------------------------------

    #' @description view the current token usage for this chat session
    usage = function() {
      glue::glue("Total: {self$tokens}\n") |> cat()
    }
    )
)


