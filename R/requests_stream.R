gpt_request_stream <- function(base_url, callback, payload, token = config::get()$openai) {

  payload = jsonlite::toJSON(c(payload, list(stream = TRUE)), auto_unbox = T)

  h <- curl::new_handle() |>
    curl::handle_setheaders(
      `Content-Type` = "application/json"
      , Authorization = paste("Bearer", token)
    ) |>
    curl::handle_setopt(postfields = payload)

  x = curl::curl_fetch_stream(base_url, fun = callback, h)
  if (x$status_code != 200) {warning(x$status_code); return()}

  # class(x) <- c(class(x), x$object)
  # x
}

completions_stream <- function(prompt
                               , callback = function(x) {
                                 # cat("howdy")
                                 # browser()
                                 x = rawToChar(x)
                                 x = stringr::str_split(x, "\\\n\\n")[[1]]
                                 x = gsub("data: ", "", x = x)
                                 x = sapply(x, \(x) if (nchar(x) != 0 & x != "[DONE]") jsonlite::fromJSON(x))
                                 x = x[!sapply(x, is.null)]
                                 for (i in x) {
                                   if (i[["choices"]][["text"]] != "[DONE]") {
                                     cat(i[["choices"]][["text"]])
                                     text_results <<- paste0(text_results, i[["choices"]][["text"]])
                                   }}
                               }
                               , payload = list(model = "text-davinci-003")
                               , token = config::get()$openai) {

  payload = c(payload, prompt = prompt)
  text_results <- ""
  # browser()
  x = gpt_request_stream("https://api.openai.com/v1/completions", callback, payload, token)
  text_results
}
