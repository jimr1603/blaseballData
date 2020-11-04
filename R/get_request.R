base_url <- "https://api.blaseball-reference.com/v1/"
user_agent <- httr::user_agent("https://github.com/jimr1603/blaseballData")

#' Gets the data from the API server
#'
#' @param path the endpoint being queried
#' @param  query additional parameters to the query.
#' @return data from the server, as a data.frame. Or a HTTP error.

get_request <- function(path, query = NULL) {
  for (i in seq_along(query)) { # Validate that each argument is length 1.
    assertthat::assert_that(length(query[[i]]) <= 1, msg = paste(names(query)[i], "must be length 1"))
  }

  response <- httr::GET(
    url = paste0(base_url, path),
    query = query,
    user_agent
  )


  # Handle errors:
  if (response$status_code >= 400) {
    err_msg <- httr::http_status(response)
    stop(err_msg)
  } else if (response$status_code == 204) {
    response <- NULL
  } else {

    # Convert response from binary to JSON:
    json_text <- httr::content(response, "text")
    response <- jsonlite::fromJSON(json_text)
  }

  return(response)
}
