gpt_request_stream <- function(base_url, callback, payload, token = config::get()$openai) {

  payload = jsonlite::toJSON(c(payload, list(stream = TRUE)), auto_unbox = T)

  h <- curl::new_handle() |>
    curl::handle_setheaders(
      `Content-Type` = "application/json"
      , Authorization = paste("Bearer", token)
    ) |>
    curl::handle_setopt(postfields = payload)

  x = curl::curl_fetch_stream(base_url, callback, h)
  if (x$status_code != 200) {warning(x$status_code); return()}

  browser()
  x = x$content |> rawToChar() |> jsonlite::fromJSON()
  class(x) <- c(class(x), x$object)
  x
}



completions_stream <- function(prompt
                               , callback = function(x, X) {
                                 # browser()
                                 x = rawToChar(x)
                                 x = stringr::str_split(x, "\\\n\\n")[[1]]
                                 x = gsub("data: ", "", x = x)
                                 x = sapply(x, \(x) if (nchar(x) != 0 & x != "[DONE]") jsonlite::fromJSON(x))
                                 x = x[!sapply(x, is.null)]
                                 for (i in x) {
                                   if (i[["choices"]][["text"]] != "[DONE]") {
                                     cat(i[["choices"]][["text"]])
                                     X <- paste(X, i[["choices"]][["text"]])
                                   }
                                 }}
                               , payload = list(model = "text-davinci-003")
                               , token = config::get()$openai) {

  payload = c(payload, prompt = prompt)
  text_results <- ""
  gpt_request_stream("https://api.openai.com/v1/completions", callback(x, text_results), payload, token)
  text_results
}


completions_stream("good day sir")

"{\"id\":\"cmpl-7DYOj9oKaKKzs7btMEBKFuX3EI03k\"
  ,\"object\":\"text_completion\"
  ,\"created\":1683464845
  ,\"choices\":[
    {\"text\":\"\\n\"
    ,\"index\":0
    ,\"logprobs\":null
    ,\"finish_reason\":null}]
    ,\"model\":\"text-davinci-003\"}
\n\n
    {\"id\":\"cmpl-7DYOj9oKaKKzs7btMEBKFuX3EI03k\",\"object\":\"text_completion\",\"created\":1683464845,\"choices\":[{\"text\":\"\\n\",\"index\":0,\"logprobs\":null,\"finish_reason\":null}],\"model\":\"text-davinci-003\"}\n\n{\"id\":\"cmpl-7DYOj9oKaKKzs7btMEBKFuX3EI03k\",\"object\":\"text_completion\",\"created\":1683464845,\"choices\":[{\"text\":\"Good\",\"index\":0,\"logprobs\":null,\"finish_reason\":null}],\"model\":\"text-davinci-003\"}\n\n{\"id\":\"cmpl-7DYOj9oKaKKzs7btMEBKFuX3EI03k\",\"object\":\"text_completion\",\"created\":1683464845,\"choices\":[{\"text\":\" day\",\"index\":0,\"logprobs\":null,\"finish_reason\":null}],\"model\":\"text-davinci-003\"}"
