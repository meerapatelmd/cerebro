get_sp500_div_yield <-
  # https://data.nasdaq.com/data/MULTPL-sp-500-ratios/usage/quickstart/api
  function() {
file <- tempfile(fileext = ".json")
on.exit(unlink(file))

Sys.sleep(3)
httr::GET(
glue::glue(
'https://data.nasdaq.com/api/v3/datasets/MULTPL/SP500_DIV_YIELD_MONTH?api_key={(Sys.getenv("NASDAQ_API_KEY"))}'
)) %>%
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

# "https://data.nasdaq.com/data/FRED/DFF"

fred_lookup <-
list(
  GROWTH =
tibble::tribble(
  ~`CODE`, ~`INDICATOR`,
  'GDP','Gross Domestic Product',
  'GDPC1','Real Gross Domestic Product',
  'GDPPOT','Real Potential Gross Domestic Product'
),
PRICES_AND_INFLATION =
tibble::tribble(
  ~`CODE`, ~`INDICATOR`,
  'CPIAUCSL','Consumer Price Index for All Urban Consumers: All Items',
  'CPILFESL','Consumer Price Index for All Urban Consumers: All Items Less Food & Energy',
  'GDPDEF','Gross Domestic Product: Implicit Price Deflator'
),
MONEY_SUPPLY =
tibble::tribble(
  ~`CODE`, ~`INDICATOR`,
  'BASE','St. Louis Adjusted Monetary Base',
  'M1','M1 Money Stock',
  'M2','M2 Money Stock',
  'M1V','Velocity of M1 Money Stock',
  'M2V','Velocity of M2 Money Stock'
),
INTEREST_RATES =
tibble::tribble(
  ~`CODE`, ~`INDICATOR`,
  'DFF','Effective Federal Funds Rate',
  'DTB3','3-Month Treasury Bill: Secondary Market Rate',
  'DGS5','5-Year Treasury Constant Maturity Rate',
  'DGS10','10-Year Treasury Constant Maturity Rate',
  'DGS30','30-Year Treasury Constant Maturity Rate',
  'T5YIE','5-year Breakeven Inflation Rate',
  'T10YIE','10-year Breakeven Inflation Rate',
  'T5YIFR','5-Year, 5-Year Forward Inflation Expectation Rate',
  'TEDRATE','TED Spread',
  'DPRIME','Bank Prime Loan Rate'
),
EMPLOYMENT =
tibble::tribble(
  ~`CODE`, ~`INDICATOR`,
  'UNRATE','Civilian Unemployment Rate',
  'NROU','Natural Rate of Unemployment (Long-Term)',
  'NROUST','Natural Rate of Unemployment (Short-Term)',
  'CIVPART','Civilian Labor Force Participation Rate',
  'EMRATIO','Civilian Employment-Population Ratio',
  'UNEMPLOY','Unemployed',
  'PAYEMS','All Employees: Total nonfarm',
  'MANEMP','All Employees: Manufacturing',
  'ICSA','Initial Claims',
  'IC4WSA','4-Week Moving Average of Initial Claims'
),
INCOME_AND_EXPENDITURE =
  tibble::tribble(
    ~`CODE`, ~`INDICATOR`,
    'MEHOINUSA672N','Real Median Household Income in the United States',
    'DSPIC96','Real Disposable Personal Income',
    'PCE','Personal Consumption Expenditures',
    'PCEDG','Personal Consumption Expenditures: Durable Goods',
    'PSAVERT','Personal Saving Rate',
    'RRSFS','Real Retail and Food Services Sales',
    'DSPI','Disposable personal income'
  ),
OTHER_ECONOMIC_INDICATORS =
tibble::tribble(
  ~`CODE`, ~`INDICATOR`,
  'INDPRO','Industrial Production Index',
  'TCU','Capacity Utilization: Total Industry',
  'HOUST','Housing Starts: Total: New Privately Owned Housing Units Started',
  'GPDI','Gross Private Domestic Investment',
  'CP','Corporate Profits After Tax (without IVA and CCAdj)',
  'STLFSI','St. Louis Fed Financial Stress Index',
  'DCOILWTICO','Crude Oil Prices: West Texas Intermediate (WTI) - Cushing, Oklahoma',
  'USSLIND','Leading Index for the United States',
  'DTWEXM','Trade Weighted U.S. Dollar Index: Major Currencies',
  'DTWEXB','Trade Weighted U.S. Dollar Index: Broad'
),
DEBT =
  tibble::tribble(
    ~`CODE`, ~`INDICATOR`,
    'GFDEBTN','Federal Debt: Total Public Debt',
    'GFDEGDQ188S','Federal Debt: Total Public Debt as Percent of Gross Domestic Product',
    'EXCSRESNW','Excess Reserves of Depository Institutions',
    'TOTCI','Commercial and Industrial Loans, All Commercial Banks'
  )
)

# file <- "roxygen.txt"
# cat(file = file)
# for (i in seq_along(fred_lookup)) {
#   for (j in 1:nrow(fred_lookup[[i]])) {
#
#     if (j == 1) {
#       cat(
#         glue::glue(
#         "@section <<<names(fred_lookup)[i]>>>:",
#         "\\tabular{ll}{",
#         .sep = "\n",
#         .open = "<<<",
#         .close = ">>>"
#         ),
#         file = file,
#         append = TRUE,
#         sep = "\n"
#       )
#
#     }
#
#     cat(glue::glue("  <<<fred_lookup[[i]]$CODE[j]>>> \\tab <<<fred_lookup[[i]]$INDICATOR[j]>>> \\cr",
#                    .sep = "\n",
#                    .open = "<<<",
#                    .close = ">>>"),
#         sep = "\n",
#         file = file,
#         append = TRUE)
#
#     if (j == nrow(fred_lookup[[i]])) {
#
#       cat("}\n",
#           file = file,
#           sep = "\n",
#           append = TRUE)
#     }
#   }
# }

get_fred_dff <-
  function() {

    file <- tempfile(fileext = ".json")
    on.exit(unlink(file))

Sys.sleep(3)
httr::GET(
  glue::glue(
    'https://data.nasdaq.com/api/v3/datasets/FRED/DFF?api_key={(Sys.getenv("NASDAQ_API_KEY"))}'
  )) %>%
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

