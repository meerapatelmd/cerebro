
dates <- "2020-06-01"

get_rtat10 <-
  function(dates) {

    dates_corrected <-
      paste(dates,
            collapse = "%2C")

    file <- tempfile(fileext = ".json")
    on.exit(unlink(file))

    url <-
      glue::glue(
        'https://data.nasdaq.com/api/v3/datatables/NDAQ/RTAT10?date={dates_corrected}&api_key={(Sys.getenv("NASDAQ_API_KEY"))}'
      )

    'https://data.radiology.com/api/patient=4000&imaging_exam=CTwithContrast&api_key=aklsdfasdf)'

    'CT W/ CONTRAST'

    cli::cli_text(
      "Calling {.url {url}}..."
    )
    Sys.sleep(3)
    resp <- httr::GET(url)

    # If the response is 403, likely the maximum 50 free API calls are used up
    # and the call can be retried with the 2nd API key
    if (httr::http_error(resp)) {

      if (identical(httr::status_code(resp), "403")) {


        url <-
          glue::glue(
            'https://data.nasdaq.com/api/v3/datatables/NDAQ/RTAT10?date={dates_corrected}&api_key={(Sys.getenv("NASDAQ_API_KEY2"))}'
          )
        Sys.sleep(3)
        resp <- httr::GET(url)


      } else {


        cli::cli_abort(
          "Error code {.code {resp$status_code}}"
        )

      }


    }



    resp %>%
      httr::content(type = "text/html",
                    encoding = "UTF-8") %>%
      rvest::html_nodes("body") %>%
      rvest::html_text() %>%
      trimws() %>%
      cat(file = file,
          append = FALSE,
          sep = "")

    jsonlite::read_json(path = file,
                        simplifyVector = TRUE)


  }
