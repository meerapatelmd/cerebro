
#' @importFrom cli combine_ansi_styles

bold_red <-
  cli::combine_ansi_styles("bold", "red4")

#' @importFrom cli combine_ansi_styles

bold_green <-
  cli::combine_ansi_styles("bold", "green")

#' @title
#' Calculate the Previous Business Day
#' @rdname previous_business_day
#' @export
#' @import lubridate
#' @importFrom cli cli_inform

previous_business_day <-
  function(input_date,
           verbose = TRUE) {
    ymd_date <-
      ymd(input_date)

    input_weekday <-
      weekdays(
        x = ymd_date,
        abbreviate = TRUE
      )


    if (input_weekday == "Sat") {
      new_date <-
        ymd_date - 1

      if (verbose) {
        cli::cli_inform(
          "{bold_red(symbol$cross)} {cli::style_italic(input_date)} occurs on a {weekdays(x = ymd_date, abbreviate = FALSE)}."
        )

        cli::cli_inform(
          "--> {cli::style_bold(new_date)} is the most previous business day ({weekdays(x = new_date, abbreviate = FALSE)})."
        )

        invisible(as.character(new_date))
      } else {
        as.character(new_date)
      }
    } else if (input_weekday == "Sun") {
      new_date <-
        ymd_date - 2

      if (verbose) {
        cli::cli_inform(
          "{bold_red(symbol$cross)} {cli::style_italic(input_date)} occurs on a {weekdays(x = ymd_date, abbreviate = FALSE)}."
        )

        cli::cli_inform(
          "  --> {cli::style_bold(new_date)} is the most previous business day ({weekdays(x = new_date, abbreviate = FALSE)})."
        )

        invisible(as.character(new_date))
      } else {
        as.character(new_date)
      }
    } else {
      if (verbose) {
        cli::cli_inform(
          "{bold_green(symbol$tick)} {cli::style_italic(input_date)} occurs on a business day ({weekdays(x = ymd_date, abbreviate = FALSE)})."
        )

        invisible(as.character(ymd_date))
      } else {
        as.character(ymd_date)
      }
    }
  }


#' @title
#' Calculate the Next Business Day
#' @rdname next_business_day
#' @export
#' @import lubridate
#' @importFrom cli cli_inform

next_business_day <-
  function(input_date,
           verbose = TRUE) {
    ymd_date <-
      ymd(input_date)

    input_weekday <-
      weekdays(
        x = ymd_date,
        abbreviate = TRUE
      )


    if (input_weekday == "Sat") {
      new_date <-
        ymd_date + 2

      if (verbose) {
        cli::cli_inform(
          "{bold_red(symbol$cross)} {cli::style_italic(input_date)} occurs on a {weekdays(x = ymd_date, abbreviate = FALSE)}."
        )

        cli::cli_inform(
          "  --> {cli::style_bold(new_date)} is the next business day ({weekdays(x = new_date, abbreviate = FALSE)})."
        )

        invisible(as.character(new_date))
      } else {
        as.character(new_date)
      }
    } else if (input_weekday == "Sun") {
      new_date <-
        ymd_date + 1


      if (verbose) {
        cli::cli_inform(
          "{bold_red(symbol$cross)} {cli::style_italic(input_date)} occurs on a {weekdays(x = ymd_date, abbreviate = FALSE)}."
        )

        cli::cli_inform(
          "  --> {cli::style_bold(new_date)} is the next business day ({weekdays(x = new_date, abbreviate = FALSE)})."
        )

        invisible(as.character(new_date))
      } else {
        as.character(new_date)
      }
    } else {
      if (verbose) {
        cli::cli_inform(
          "{bold_green(symbol$tick)} {cli::style_italic(input_date)} occurs on a business day ({weekdays(x = ymd_date, abbreviate = FALSE)})."
        )
        invisible(as.character(ymd_date))
      } else {
        as.character(ymd_date)
      }
    }
  }
