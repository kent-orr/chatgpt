
#' Send a request to openai
#'
#' @param base_url The url fo the endpoint
#' @param payload the payload, an R list to be converted to JSON
#' @param token your API auth token
#'
#' @export
#'
gpt_request <- function(base_url, payload, token = config::get()$openai) {

  payload = jsonlite::toJSON(payload, auto_unbox = T)

  h <- curl::new_handle() |>
    curl::handle_setheaders(
      `Content-Type` = "application/json"
      , Authorization = paste("Bearer", token)
    ) |>
    curl::handle_setopt(postfields = payload)

  x = curl::curl_fetch_memory(base_url, h)
  if (x$status_code != 200) {warning(x$status_code); return()}

  x = x$content |> rawToChar() |> jsonlite::fromJSON()
  class(x) <- c(class(x), x$object)
  x
}

#' Send a request to the completions endpoint
#'
#' @param prompt a text prompt
#' @param payload the POST request payload given
#' @param token api token
#'
#' @export
#'
completions <- function(prompt
                    , payload = list(model = "text-davinci-003")
                    , token = config::get()$openai) {

  payload = c(payload, prompt = prompt)
  x = gpt_request("https://api.openai.com/v1/completions", payload, token)
  x
}

#' Send a request to the chat endpoint
#'
#' @param prompt a text prompt
#' @param payload the POST request payload given
#' @param token api token
#'
#' @export
#'
chats <- function(messages
                  , payload = list(model = "gpt-3.5-turbo")
                  , token = config::get()$openai) {

  payload = c(payload, list(messages = messages))
  x = gpt_request("https://api.openai.com/v1/chat/completions", payload, token)
  x
}

print.chat.completion <- function(x, ...) {
  cat(stringr::str_squish(x$choices$message$content))
}

print.text_completion <- function(x, ...) {
  cat(stringr::str_squish(x$choices$text))
}
