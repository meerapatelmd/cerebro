#' @title
#' Write CSV to Import Into Google Calendar
#'
#' @importFrom readr write_csv
#' @rdname write_gcal_csv
#' @export



write_gcal_csv <-
  function(subject = "New Event",
           start_date_day_number,
           start_date_year,
           start_date_offset = c("none", "previous_business_day", "next_business_day"),
           gcal_csv,
           verbose = TRUE) {


    stopifnot(!file.exists(gcal_csv))

    gcal_df <-
    gcal(
      subject = subject,
      start_date_day_number = start_date_day_number,
      start_date_year = start_date_year,
      start_date_offset = start_date_offset,
      verbose = verbose
    )


    readr::write_csv(
      x = gcal_df,
      file = gcal_csv,
      na = ""
    )




  }
