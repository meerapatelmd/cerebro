#' @title
#' Google Calendar Import Dataframe
#'
#' @description
#' Create a dataframe that can be imported
#' into a Google Calendar in CSV format.
#'
#' @details
#' The returned dataframe has the following
#' structure:
#' \tabular{ll}{
#'  \strong{Field} \tab \strong{Definition} \cr
#'  Subject \tab (Required) The name of the event. Example: 'Final exam' \cr
#'  Start Date \tab (Required) The start date in 'm/d/y' format. \cr
#'  Start Time \tab Time the event begins. Example: 10:00 AM. \cr
#'  End Date \tab The date the event ends. \cr
#'  End Time \tab Time the event ends. Example: 1:00 PM \cr
#'  All Day Event \tab Boolean indicating whether event occurs all day. Example: True \cr
#'  Description \tab Notes about the event. \cr
#'  Location \tab Location of event. Example: "Columbia, Schermerhorn 614" \cr
#'  Private \tab Boolean indicating whether event is private or not. Example: False \cr
#' }
#'
#' @rdname gcal
#'
#' @export

gcal <-
  function(subject = "New Event",
           start_date_day_number,
           start_date_year,
           start_date_offset = c("none", "previous_business_day", "next_business_day"),
           verbose = TRUE) {


   start_date <-
     format(
     lubridate::ymd(
      get_monthly_dates(year = start_date_year,
                        day_number = start_date_day_number,
                        offset = start_date_offset,
                        verbose = verbose)),
      "%m/%d/%Y")


   tibble(
     `Subject`       = subject,
     `Start Date`    = start_date,
     `Start Time`    = "",
     `End Date`      = "",
     `End Time`      = "",
     `All Day Event` = "",
     `Description`   = "",
     `Location`      = "",
     `Private`       = ""
   )




  }





gcal_skeleton <-
  function() {

    tribble(
      ~`Subject`,
      ~`Start Date`,
      ~`Start Time`,
      ~`End Date`,
      ~`End Time`,
      ~`All Day Event`,
      ~`Description`,
      ~`Location`,
      ~`Private`
    )


  }
