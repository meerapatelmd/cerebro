# I want to save the fidelity orders data
# to pull trend lines long term

test_output
test_output$Date


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

