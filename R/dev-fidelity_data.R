library(tidyverse)



get_fidelity_orders <-
  function() {
cli::cli_text("Reading table in {.url https://eresearch.fidelity.com/eresearch/gotoBL/fidelityTopOrders.jhtml}...")
Sys.sleep(3)
raw_input <-
  rvest::read_html(x = "https://eresearch.fidelity.com/eresearch/gotoBL/fidelityTopOrders.jhtml")

input <-
  raw_input %>%
  rvest::html_node("#topOrdersTable") %>%
  rvest::html_table()

cli::cli_text("Source table:")
print(input)
# > input
# # A tibble: 30 × 8
# `Rank ‡` Symbol Company                      `Price Change`       `# Buy Orders` `Buy / Sell Ra…` `# Sell Orders` `Recent News`
# <dbl> <chr>  <chr>                        <chr>                <chr>          <lgl>            <chr>           <chr>
#  4       44 AAPL   APPLE INC                    1.50+1.50 (+1.1533%) 35383,538      NA               28112,811       Warren
#  5       55 REV    REVLON INC                   1.78+1.78 (+91.2821… 25942,594      NA               28822,882       Dow Jumps
#  6       66 AMD    ADVANCED MICRO DEVICES INC   -0.48-0.48 (-0.585%) 29262,926      NA               21052,105       Form  4

# Cleaning up column names
cli::cli_text("Column names are standardized and {.var rowid} identifier added...")
colnames(input) <-
  c(
    "rank",
    "symbol",
    "company",
    "price_change",
    "buy_orders",
    "buy_to_sell_ratio",
    "sell_orders",
    "recent_news"
  )

input <-
  input %>%
  rowid_to_column()

print(input)
# > input
# # A tibble: 30 × 8
# rank symbol company                      price_change             buy_orders buy_to_sell_ratio sell_orders recent_news
# <dbl> <chr>  <chr>                        <chr>                    <chr>      <lgl>             <chr>       <chr>
#   1    11 TSLA   TESLA INC                    10.98+10.98 (+1.7175%)   54445,444  NA                61896,189   Elon Musk Acknowle…
# 2    22 TQQQ   PROSHARES ULTRAPRO QQQ       0.81+0.81 (+3.7054%)     42354,235  NA                52205,220   --
#   3    33 SQQQ   PROSHARES ULTRAPRO SHORT QQQ -2.13-2.13 (-3.2229%)    39473,947  NA                38753,875   Matt Maley's Inner…
#  4    44 AAPL   APPLE INC                    1.50+1.50 (+1.1533%)     35383,538  NA                28112,811   Warren Buffett cha…

cli::cli_bullets(
  c(
    "Processing data...",
    "*" = "{.var price_change} is processed into numeric {.var price_change_dollar}, {.var price_change_percent}, and {.var price_change_direction}.",
    "*" = "{.var estimated_share_price} is calculated using the {.var price_change_dollar} and {.var price_change_percent} values.",
    "*" = "{.var rank}, {.var buy_orders}, and {.var sell_orders} strings are deduped.",
    "*" = "{.var buy_to_sell_ratio} is updated by calculating {.var buy_orders}/{.var sell_orders}.",
    "*" = "{.var buy_sell_trend} is added based on the {.var buy_to_sell_ratio}."
  )
)
# Cleaning up numeric values (repeating pattern for numbers
output_a <-
input %>%
  transmute(rowid,
         source_price_change = price_change) %>%
  extract(col = source_price_change,
          into = c("price_change_dollar",
                   "price_change_percent"),
          regex = "(^.*?) [(]{1}(.*?)[)]{1}",
          remove = FALSE) %>%
  mutate(price_change_direction =
           case_when(grepl(pattern = "[+]{1}",
                           source_price_change) ~ "+",
                     grepl(pattern = "[-]{1}",
                           source_price_change) ~ "-",
                     TRUE ~ NA_character_)) %>%
  mutate(across(c(price_change_dollar, price_change_percent),
                ~str_remove_all(., pattern = "[%+-]"))) %>%
  mutate(across(c(price_change_dollar, price_change_percent),
                function(x) substr(x, start = 1, stop = nchar(x)/2)
                )) %>%
  mutate(estimated_share_price =
           as.character(as.double(price_change_dollar)/(as.double(price_change_percent)/100)))


output_b <-
  input %>%
  transmute(rowid,
         source_rank = rank,
         source_buy_orders = buy_orders,
         source_sell_orders = sell_orders,
         rank,
         buy_orders,
         sell_orders) %>%
  mutate(
    across(
      c(rank, buy_orders, sell_orders),
        function(x) str_remove_all(as.character(x), pattern = "[^0-9]"))) %>%
  mutate(
    across(
      c(rank, buy_orders, sell_orders),
      function(x) substr(x, 1, (nchar(x)/2)))) %>%
  mutate(
    across(
      c(rank, buy_orders, sell_orders),
      as.integer
      )
  ) %>%
  mutate(buy_to_sell_ratio =
           buy_orders/sell_orders) %>%
  mutate(buy_sell_trend =
           case_when(buy_to_sell_ratio > 1 ~ "Buying",
                     buy_to_sell_ratio < 1 ~ "Selling",
                     TRUE ~ "Neutral"))


output_c <-
  input %>%
  distinct(rowid,
           symbol,
           company)

output2 <-
  list(output_c,
       output_a,
       output_b) %>%
  reduce(left_join,
         by = "rowid") %>%
  distinct() %>%
  select(
    rowid,
    rank,
    symbol,
    company,
    estimated_share_price,
    price_change_dollar,
    price_change_percent,
    price_change_direction,
    buy_orders,
    sell_orders,
    buy_to_sell_ratio,
    buy_sell_trend,
    starts_with("source")
  )

test_output <-
list(
  'Date' = raw_input %>% rvest::html_node(".source") %>% rvest::html_text(),
  'Raw Data' = input,
  'Selling (+)' =
list(
definition = "Stock that is predominantly selling with an increase in price.",
df =
output2 %>%
  dplyr::filter(
    buy_sell_trend == 'Selling',
    price_change_direction == '+')  %>%
  arrange(buy_to_sell_ratio)
),
'Buying (+)' =
list(
  definition = "Stock that is predominantly being bought with an increase in price.",
  df =
output2 %>%
  dplyr::filter(
    buy_sell_trend == 'Buying',
    price_change_direction == '+')  %>%
  arrange(desc(buy_to_sell_ratio))
),
'Selling (-)' =
list(
  definition = "Stock that is predominantly selling with a decrease in price.",
  df =
output2 %>%
  dplyr::filter(
    buy_sell_trend == 'Selling',
    price_change_direction == '-') %>%
  arrange(buy_to_sell_ratio)
),
'Buying (-)' =
list(
  definition = "Stock that is predominantly being bought with a decrease in price.",
  df =
    output2 %>%
    dplyr::filter(
      buy_sell_trend == 'Buying',
      price_change_direction == '-') %>%
    arrange(desc(buy_to_sell_ratio))
))

output_dir <- "inst/fidelity_orders"
if (!dir.exists(output_dir)) {


  dir.create(output_dir)

}


# If data was pulled midday it includes
# the time ie "as of 02:58 PM ET 06/23/22"
# otherwise it is "as of 06/23/22"


# I am converting this value into valid filenames
# that can also be used to derive the correct timestamp
# in the reverse direction when I want to retrieve the
# data
filename <-
  stringr::str_replace_all( test_output$Date,
                            pattern = "(^.*?)([0-9]{1,}.*$)",
                            replacement = "\\2"
  )

# If it is date only, lubridate
# will parse it

normalized_filename <- lubridate::mdy(filename,
                                      quiet = TRUE)

if (!is.na(normalized_filename)) {


  normalized_filename <-
    xfun::with_ext(x = normalized_filename,
                   ext = "xlsx")

} else {

  normalized_filename_date <-
    stringr::str_replace_all(filename,
                             pattern = "^(.*[ ]{1})([0-9]{1,}.*$)",
                             replacement = "\\2")


  normalized_filename_time <-
    stringr::str_replace_all(filename,
                             pattern = "^(.*[ ]{1})([0-9]{1,}.*$)",
                             replacement = "\\1")

  normalized_filename_time <-
    trimws(
      normalized_filename_time
    )
  normalized_filename_time <-
    stringr::str_replace_all(normalized_filename_time,
                             pattern = "(^.*[PA]{1}M)(.*$)",
                             replacement = "\\1")

  normalized_filename_str <-
    glue::glue("{normalized_filename_date} {normalized_filename_time}")


  normalized_filename <-
    as.character(
      strptime(
        normalized_filename_str,
        format = "%m/%d/%y %I:%M %p"
      )
    )

  normalized_filename <-
    xfun::with_ext(x = normalized_filename,
                   ext = "xlsx")


}

if (is.na(normalized_filename)) {

  cli::cli_abort("Normalized filename could not be derived from {.var Date} value '{test_output$Date}'",
                 call = NULL)

}

output_file_path <-
  file.path(output_dir,
            normalized_filename)

# Writing test_output to
# filename

Data <-
  list(
    `Selling (+)` = test_output$`Selling (+)`$df,
    `Buying (+)`  = test_output$`Buying (+)`$df,
    `Selling (-)` = test_output$`Selling (-)`$df,
    `Buying (-)`  = test_output$`Buying (-)`$df) %>%
  bind_rows(.id = "category") %>%
  arrange(rowid) # Arrange rowid based on original order from Fidelity website

`Raw Data` <- test_output$`Raw Data`


final_output <-
  list(
    Data       = Data,
    `Raw Data` = `Raw Data`
  )

openxlsx::write.xlsx(
  x = final_output,
  file = output_file_path
)

return(test_output)
}
