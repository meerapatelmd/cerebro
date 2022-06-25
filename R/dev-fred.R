#' @title
#' Nasdaq FRED API
#'
#' @section GROWTH:
#' \tabular{ll}{
#'   CODE \tab INDICATOR \cr
#'   GDP \tab Gross Domestic Product \cr
#'   GDPC1 \tab Real Gross Domestic Product \cr
#'   GDPPOT \tab Real Potential Gross Domestic Product \cr
#' }
#'
#' @section PRICES_AND_INFLATION:
#' \tabular{ll}{
#'   CODE \tab INDICATOR \cr
#'   CPIAUCSL \tab Consumer Price Index for All Urban Consumers: All Items \cr
#'   CPILFESL \tab Consumer Price Index for All Urban Consumers: All Items Less Food & Energy \cr
#'   GDPDEF \tab Gross Domestic Product: Implicit Price Deflator \cr
#' }
#'
#' @section MONEY_SUPPLY:
#' \tabular{ll}{
#'   CODE \tab INDICATOR \cr
#'   BASE \tab St. Louis Adjusted Monetary Base \cr
#'   M1 \tab M1 Money Stock \cr
#'   M2 \tab M2 Money Stock \cr
#'   M1V \tab Velocity of M1 Money Stock \cr
#'   M2V \tab Velocity of M2 Money Stock \cr
#' }
#'
#' @section INTEREST_RATES:
#' \tabular{ll}{
#'   CODE \tab INDICATOR \cr
#'   DFF \tab Effective Federal Funds Rate \cr
#'   DTB3 \tab 3-Month Treasury Bill: Secondary Market Rate \cr
#'   DGS5 \tab 5-Year Treasury Constant Maturity Rate \cr
#'   DGS10 \tab 10-Year Treasury Constant Maturity Rate \cr
#'   DGS30 \tab 30-Year Treasury Constant Maturity Rate \cr
#'   T5YIE \tab 5-year Breakeven Inflation Rate \cr
#'   T10YIE \tab 10-year Breakeven Inflation Rate \cr
#'   T5YIFR \tab 5-Year, 5-Year Forward Inflation Expectation Rate \cr
#'   TEDRATE \tab TED Spread \cr
#'   DPRIME \tab Bank Prime Loan Rate \cr
#' }
#'
#' @section EMPLOYMENT:
#' \tabular{ll}{
#'   CODE \tab INDICATOR \cr
#'   UNRATE \tab Civilian Unemployment Rate \cr
#'   NROU \tab Natural Rate of Unemployment (Long-Term) \cr
#'   NROUST \tab Natural Rate of Unemployment (Short-Term) \cr
#'   CIVPART \tab Civilian Labor Force Participation Rate \cr
#'   EMRATIO \tab Civilian Employment-Population Ratio \cr
#'   UNEMPLOY \tab Unemployed \cr
#'   PAYEMS \tab All Employees: Total nonfarm \cr
#'   MANEMP \tab All Employees: Manufacturing \cr
#'   ICSA \tab Initial Claims \cr
#'   IC4WSA \tab 4-Week Moving Average of Initial Claims \cr
#' }
#'
#' @section INCOME_AND_EXPENDITURE:
#' \tabular{ll}{
#'   CODE \tab INDICATOR \cr
#'   MEHOINUSA672N \tab Real Median Household Income in the United States \cr
#'   DSPIC96 \tab Real Disposable Personal Income \cr
#'   PCE \tab Personal Consumption Expenditures \cr
#'   PCEDG \tab Personal Consumption Expenditures: Durable Goods \cr
#'   PSAVERT \tab Personal Saving Rate \cr
#'   RRSFS \tab Real Retail and Food Services Sales \cr
#'   DSPI \tab Disposable personal income \cr
#' }
#'
#' @section OTHER_ECONOMIC_INDICATORS:
#' \tabular{ll}{
#'   CODE \tab INDICATOR \cr
#'   INDPRO \tab Industrial Production Index \cr
#'   TCU \tab Capacity Utilization: Total Industry \cr
#'   HOUST \tab Housing Starts: Total: New Privately Owned Housing Units Started \cr
#'   GPDI \tab Gross Private Domestic Investment \cr
#'   CP \tab Corporate Profits After Tax (without IVA and CCAdj) \cr
#'   STLFSI \tab St. Louis Fed Financial Stress Index \cr
#'   DCOILWTICO \tab Crude Oil Prices: West Texas Intermediate (WTI) - Cushing, Oklahoma \cr
#'   USSLIND \tab Leading Index for the United States \cr
#'   DTWEXM \tab Trade Weighted U.S. Dollar Index: Major Currencies \cr
#'   DTWEXB \tab Trade Weighted U.S. Dollar Index: Broad \cr
#' }
#'
#' @section DEBT:
#' \tabular{ll}{
#'   CODE \tab INDICATOR \cr
#'   GFDEBTN \tab Federal Debt: Total Public Debt \cr
#'   GFDEGDQ188S \tab Federal Debt: Total Public Debt as Percent of Gross Domestic Product \cr
#'   EXCSRESNW \tab Excess Reserves of Depository Institutions \cr
#'   TOTCI \tab Commercial and Industrial Loans, All Commercial Banks \cr
#' }
#'
#'
#' @rdname get_fred_data
#' @export

get_fred_data <-
  function(code) {

    code_corrected <-
    match.arg(
      arg = code,
      choices =   c(
        'BASE',
        'CIVPART',
        'CP',
        'CPIAUCSL',
        'CPILFESL',
        'DCOILWTICO',
        'DFF',
        'DGS10',
        'DGS30',
        'DGS5',
        'DPRIME',
        'DSPI',
        'DSPIC96',
        'DTB3',
        'DTWEXB',
        'DTWEXM',
        'EMRATIO',
        'EXCSRESNW',
        'GDP',
        'GDPC1',
        'GDPDEF',
        'GDPPOT',
        'GFDEBTN',
        'GFDEGDQ188S',
        'GPDI',
        'HOUST',
        'IC4WSA',
        'ICSA',
        'INDPRO',
        'M1',
        'M1V',
        'M2',
        'M2V',
        'MANEMP',
        'MEHOINUSA672N',
        'NROU',
        'NROUST',
        'PAYEMS',
        'PCE',
        'PCEDG',
        'PSAVERT',
        'RRSFS',
        'STLFSI',
        'T10YIE',
        'T5YIE',
        'T5YIFR',
        'TCU',
        'TEDRATE',
        'TOTCI',
        'UNEMPLOY',
        'UNRATE',
        'USSLIND'
      ),
      several.ok = FALSE
    )

    file <- tempfile(fileext = ".json")
    on.exit(unlink(file))

    url <-
      glue::glue(
        'https://data.nasdaq.com/api/v3/datasets/FRED/{code_corrected}?api_key={(Sys.getenv("NASDAQ_API_KEY"))}'
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
