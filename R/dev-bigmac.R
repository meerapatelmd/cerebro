

bigmac_lookup <-
  list(
  'Vietnam' = 'VNM',
  'Argentina' = 'ARG',
  'Australia' = 'AUS',
  'Brazil' = 'BRA',
  'Britain' = 'GBR',
  'Canada' = 'CAN',
  'Chile' = 'CHL',
  'China' = 'CHN',
  'Colombia' = 'COL',
  'Costa Rica' = 'CRI',
  'Czech Republic' = 'CZE',
  'Denmark' = 'DNK',
  'Egypt' = 'EGY',
  'Euro area' = 'EUR',
  'Hong Kong' = 'HKG',
  'Hungary' = 'HUN',
  'India' = 'IND',
  'Indonesia' = 'IDN',
  'Israel' = 'ISR',
  'Japan' = 'JPN',
  'Lithuania' = 'LTU',
  'Malaysia' = 'MYS',
  'Mexico' = 'MEX',
  'New Zealand' = 'NZL',
  'Norway' = 'NOR',
  'Pakistan' = 'PAK',
  'Peru' = 'PER',
  'Philippines' = 'PHL',
  'Poland' = 'POL',
  'Russia' = 'RUS',
  'Saudi Arabia' = 'SAU',
  'Singapore' = 'SIN',
  'South Africa' = 'ZAF',
  'South Korea' = 'KOR',
  'Sri Lanka' = 'LKA',
  'Sweden' = 'SWE',
  'Switzerland' = 'CHE',
  'Taiwan' = 'ROC',
  'Thailand' = 'THA',
  'Turkey' = 'TUR',
  'UAE' = 'UAE',
  'Ukraine' = 'UKR',
  'United States' = 'USA',
  'Uruguay' = 'URY',
  'Venezuela' = 'VEN',
  'Austria' = 'AUT',
  'Belgium' = 'BEL',
  'Estonia' = 'EST',
  'Finland' = 'FIN',
  'France' = 'FRA',
  'Germany' = 'DEU',
  'Greece' = 'GRC',
  'Ireland' = 'IRL',
  'Italy' = 'ITA',
  'Netherlands' = 'NLD',
  'Portugal' = 'PRT',
  'Spain' = 'ESP',
  'Latvia' = 'LVA'
)


get_bigmac <-
  function(country) {

    country_code <-
      bigmac_lookup[[country]]

    if (is.null(country_code)) {

      cli::cli_abort("No country code found for the country {.code {country}}. See {.var bigmac_lookup} for valid countries.",
                     call = NULL)

    }

    file <- tempfile(fileext = ".json")
    on.exit(unlink(file))

    url <-
      glue::glue(
        'https://data.nasdaq.com/api/v3/datasets/ECONOMIST/BIGMAC_{country_code}?api_key={(Sys.getenv("NASDAQ_API_KEY"))}'
      )

    cli::cli_text(
      "Calling {.url {url}}..."
    )
    Sys.sleep(3)
    httr::GET(url) %>%
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
