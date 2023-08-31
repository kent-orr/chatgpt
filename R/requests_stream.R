gpt_request_stream <- function(base_url, callback, payload, token = config::get()$openai) {
  # browser()
  payload = jsonlite::toJSON(c(payload, list(stream = TRUE)), auto_unbox = T)

  h <- curl::new_handle() |>
    curl::handle_setheaders(
      `Content-Type` = "application/json"
      , Authorization = paste("Bearer", token)
    ) |>
    curl::handle_setopt(postfields = payload)

  x = curl::curl_fetch_stream(base_url, fun = callback, h)
  if (x$status_code != 200) {warning(x$status_code); return()}

  class(x) <- c(class(x), x$object)
  x
}

callback_ = function(x) {
  y = rawToChar(x)
  y2 = stringr::str_split_1(y, "\\n\\n")
  y2 = y2[y2 != ""]
  for (i in seq_along(y2)) {
    y2[i] = gsub("data: ", "", y2[i])
  }

  y2 = sapply(y2, \(x) {
    if (x == "[DONE]")
      return("")
    else
      jsonlite::fromJSON(x)$choices$delta$content
  }) |> paste(collapse = "")
  cat(y2)
}

completions_stream <- function(prompt
                               , callback = callback_
                               , payload = list(model = "gpt-3.5-turbo")
                               , token = config::get()$openai) {

  payload = c(payload, messages = list(list(list(role = "system", content = prompt))))
  x = gpt_request_stream("https://api.openai.com/v1/chat/completions", callback, payload, token)
  x
}

# x = completions_stream("write an R function that calculates the standard deviation from the mean for a set of numbers")

