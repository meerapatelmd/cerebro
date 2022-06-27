# .lmqrnY
library(tidyverse)
website_title <- "The 15 Best Waterproof Mascaras for Fluttery, Smudge-Free Lashes"
website_publication_date <- "2021-06-25"
url <- "https://www.allure.com/gallery/best-waterproof-mascara"
file <- "inst/beauty_product_database/product.csv"

current_product_data <-
  readr::read_csv(file = file)

html_data <- rvest::read_html(x = url)

html_text <-
html_data %>%
  rvest::html_node("#main-content") %>%
  rvest::html_text()

html_text2 <-
html_text %>%
  str_replace(pattern = "(^.*?)([/]{1}15May.*?)(Written by Allure.*$)",
              replacement = "\\2")

output <-
strsplit(html_text2,
         split = "[/]{1}[1]{1}[5]{1}")[[1]] %>%
  map(function(x) trimws(unlist(strsplit(x, split = "Shop Now[$]{1}[0-9]{1,2}")))) %>%
  keep(~length(.)==2) %>%
  map(~set_names(., nm = c("product_name", "product_description"))) %>%
  map(~as_tibble_row(.)) %>%
  bind_rows() %>%
  mutate(product_description =
           str_remove_all(product_description,
                          pattern = "^[$]{1}[0-9]{1,}"))

final_output <-
output %>%
  transmute(
    website_title = website_title,
    website_publication_date = website_publication_date,
    website_url = url,
    product_name,
    product_description) %>%
  distinct()

write_csv(
  x = final_output,
  file = file,
  append = TRUE
)
