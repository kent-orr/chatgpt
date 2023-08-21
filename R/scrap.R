# devtools::load_all()
# if (interactive()) {
# chat = Chat$new("hello")
# chat$chat("what day is it?")
#
#
# chat$add_function("log_name", "return an object that can be used to log a name"
#                   , list(first_name = list(type = "string"
#                                            , description = "the first nae of a person")
#                          , last_name = list(type = "string"
#                                               , description = "the last name of a person")))
#
# chat$chat("log that my name is Kent Orr, I'm 33")
#
# log_name <- function(first_name, last_name = NULL) {
#   cat("Hello, my name is", first_name, last_name)
# }
#
# do.call(chat$latest_response$function_call$name
#         , jsonlite::fromJSON(chat$latest_response$function_call$arguments)[1])
#
#
# chat = Chat$new("hello")
#
# chat$add_function("resume_txt"
#                   , "write a resume in plain text"
#                   , list(body = list(type = "string"
#                                      , description = "the corpus of a plain text resume"))
# )
#
# chat$add_function("google_query"
#                   , "write an advanced google search query to find relevant job and career pages on corporate websites."
#                   , list(job_titles = list(type = "array"
#                                            , items = list(type = "string")
#                                            , description = "the user job title and similar titles"),
#                          site_params = list(type = "array", items = list(type = "string") description = "wildcard keywords such as site:careers.* site:jobs.* to narrow search to employer websites instead of job boards"),
#                          skillset1 = list(type = "array", items = list(type = "string") description = "keywords that describe a skillset of the user"),
#                          skillset2 = list(type = "array", items = list(type = "string") description = "keywords that describe a skillset of the user"),
#                          skillset3 = list(type = "array", items = list(type = "string") description = "keywords that describe a skillset of the user"),
#                          negative_words = list(type = "array", items = list(type = "string") description = "keywords to exclude fom the search"),
#                          location_params = list(type = "array", items = list(type = "string") description = "keywords describing either the desired location, or remote work.")
#                          ) # end params
#                   ) # end add function
#
# chat$chat("what day is it?")
# chat$chat("what day is tomorrow?")
#
# chat$functions
#
# chat$function_call = list("name" = "google_query")
# chat$chat("I am a data scientist versed in R, Julia, and Python. I have experience with AWS in the form of s3 and Redshift databases. I am comfortable working with Docker. I have experience writing R shiny applications for dashboarding. I have a Masters in Recreation Management from Ohio UNiversity and a Bachelor's in Recreation Management from Ohio University. In January 2023 I started y current role as Senior Data Analyst. In this role I develop and deploy models for various tasks ranging form forecasting supply chain and inventory, to market segmentation and analysis. In July 2022 I was a research Analyst III at Ohio UNiversity. At the behest of the provost I developed models that provided decision support for academic programs and university-wide initiatives. In October of 2020 I worked at Lehigh CustomFit as a Digital Marketing ANAlyst. In that role I developed and implemented a custom integration between a third party ecommerce site host and email service provider. My role used data to develop automated email campaigns that generate approximately 8 million in sales per year.
#
# write a google query to serach for jobs for me.")
#
# chat$latest_response$function_call$arguments|> jsonlite::fromJSON()
#
# }
