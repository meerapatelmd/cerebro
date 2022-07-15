#' @title
#' Calculate 0% Interest Credit Card Payment
#'
#' @description
#' Calculate a payment for a 0% interest credit card.
#'
#' @rdname calculate_cc_payment
#' @export
#' @importFrom lubridate day as_date
#' @importFrom tibble tibble

calculate_cc_payment <-
  function(current_balance,
           first_payment_due,
           final_payment_due,
           credit_limit,
           final_balance) {

    if (missing(current_balance)) {

      cli::cli_alert_warning(
        text = "{.var current_balance} not provided. Defaulting to {.var credit_limit} {.emph {fmt_currency(credit_limit)}} as {.var current_balance}.",
        wrap = TRUE
      )
      cat("\n")

      current_balance <- credit_limit

    }

    # Getting day number for last due date
    day_number <- as.character(lubridate::day(lubridate::as_date(final_payment_due)))

    # Getting all dates between first due date and last due date
    all_dates <-
      as.character(lubridate::as_date(lubridate::as_date(first_payment_due):lubridate::as_date(final_payment_due)))

    payment_due_date <-
      lubridate::as_date(grep(x = all_dates, pattern = sprintf("%s$", day_number), value = TRUE))
    grace_period_end_date <-
      payment_due_date-1

    grace_period_start_date <-
      payment_due_date %m-% months(1)

    schedule_lookup <-
      tibble::tibble(
        payments_left = "",
        grace_period_start_date =
          grace_period_start_date,
        grace_period_end_date =
          grace_period_end_date,
        payment_due_date)

    schedule_lookup$payments_left <-
      nrow(schedule_lookup):1

    for (i in 1:nrow(schedule_lookup)) {
      start_date <-
        schedule_lookup$grace_period_start_date[i]

      end_date <-
        schedule_lookup$grace_period_end_date[i]

      if (Sys.Date() >= start_date) {
        if (Sys.Date() <= end_date) {
          payments_left <-
            schedule_lookup$payments_left[i]


          out <-
            MonthlyPayment(
              value =
                (current_balance - final_balance) / payments_left,
              payments_left = payments_left,
              credit_limit = credit_limit,
              final_balance = final_balance,
              last_payment_date = final_payment_due
            )


          return(out)
        }
      }
    }
  }

#' @title Calculate WF Payment
#' @rdname calculate_wf_payment
#' @export


calculate_wf_payment <-
  function(
    current_balance,
    first_payment_due = "2021-12-21",
    final_payment_due  = "2023-08-21",
    credit_limit   = 18000,
    final_balance = 5400) {


    calculate_cc_payment(
      current_balance = current_balance,
      first_payment_due = first_payment_due,
      final_payment_due = final_payment_due,
      credit_limit = credit_limit,
      final_balance = final_balance
    )

  }


#' @title Calculate US Bank Payment
#' @rdname calculate_usbank_payment
#' @export

calculate_usbank_payment <-
  function(
    current_balance,
    first_payment_due = "2021-09-16",
    final_payment_due  = "2023-04-16",
    credit_limit   = 13000,
    final_balance = 3000) {


    calculate_cc_payment(
      current_balance = current_balance,
      first_payment_due = first_payment_due,
      final_payment_due = final_payment_due,
      credit_limit = credit_limit,
      final_balance = final_balance
    )

  }

#' @title Calculate Citizens Payment
#' @rdname calculate_citizens_payment
#' @export

calculate_citizens_payment <-
  function(
    current_balance,
    first_payment_due = "2022-04-29",
    final_payment_due  = "2023-10-29",
    credit_limit   = 10000,
    final_balance = 3000) {


    calculate_cc_payment(
      current_balance = current_balance,
      first_payment_due = first_payment_due,
      final_payment_due = final_payment_due,
      credit_limit = credit_limit,
      final_balance = final_balance
    )

  }



#' @title Calculate Southwest Payment
#' @rdname calculate_southwest_payment
#' @export

calculate_southwest_payment <-
  function(
    current_balance = 416,
    first_payment_due = "2022-04-17",
    final_payment_due  = "2023-05-17",
    credit_limit   = 10000,
    final_balance = 0) {


    calculate_cc_payment(
      current_balance = current_balance,
      first_payment_due = first_payment_due,
      final_payment_due = final_payment_due,
      credit_limit = credit_limit,
      final_balance = final_balance
    )

  }


#' @title Calculate Slate Payment
#' @rdname calculate_slate_payment
#' @export

calculate_slate_payment <-
  function(
    current_balance = 364,
    first_payment_due = "2022-04-20",
    final_payment_due  = "2023-05-20",
    credit_limit   = 10000,
    final_balance = 0) {


    calculate_cc_payment(
      current_balance = current_balance,
      first_payment_due = first_payment_due,
      final_payment_due = final_payment_due,
      credit_limit = credit_limit,
      final_balance = final_balance
    )

  }


#' @title Calculate Freedom Payment
#' @rdname calculate_freedom_payment
#' @export

calculate_freedom_payment <-
  function(
    current_balance = 364,
    first_payment_due = "2022-04-20",
    final_payment_due  = "2023-06-20",
    credit_limit   = 10000,
    final_balance = 0) {


    calculate_cc_payment(
      current_balance = current_balance,
      first_payment_due = first_payment_due,
      final_payment_due = final_payment_due,
      credit_limit = credit_limit,
      final_balance = final_balance
    )

  }

#' @title Calculate Capital One Payment
#' @rdname calculate_capitalone_payment
#' @export

calculate_capitalone_payment <-
  function(
    current_balance = 2773,
    first_payment_due = "2022-06-17",
    final_payment_due  = "2023-06-17",
    credit_limit   = 15000,
    final_balance = 0) {


    calculate_cc_payment(
      current_balance = current_balance,
      first_payment_due = first_payment_due,
      final_payment_due = final_payment_due,
      credit_limit = credit_limit,
      final_balance = final_balance
    )

  }


#' @title Calculate Simplicity Payment
#' @rdname calculate_simplicity_payment
#' @export

calculate_simplicity_payment <-
  function(
    current_balance = 291,
    first_payment_due = "2022-06-21",
    final_payment_due  = "2023-05-21",
    credit_limit   = 10000,
    final_balance = 0) {


    calculate_cc_payment(
      current_balance = current_balance,
      first_payment_due = first_payment_due,
      final_payment_due = final_payment_due,
      credit_limit = credit_limit,
      final_balance = final_balance
    )

  }
