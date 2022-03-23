
MonthlyPayment <-
  setClass(
    "MonthlyPayment",
    slots =
      c(
        value = "numeric",
        payments_left = "integer",
        credit_limit = "numeric",
        final_balance = "numeric",
        last_payment_date = "character"
      )
  )

setMethod(
  f = "show",
  signature = "MonthlyPayment",
  definition = function(object) {
    monthly_payment <- object@value
    payments_left <- object@payments_left
    credit_limit <- object@credit_limit
    final_balance <- object@final_balance
    last_due_date <- object@last_payment_date
    cli::cli_text(
      "{fmt_currency(monthly_payment)} at {payments_left} payments left."
    )
    cli::cli_text(
      "Total Paid Off: {fmt_currency(payments_left*monthly_payment)}"
    )
    cli::cli_text(
      "Ending Balance: {fmt_currency(final_balance)}"
    )
    cli::cli_text(
      "Limit: {fmt_currency(credit_limit)}"
    )

    cli::cli_text(
      "Last Due Date: {last_due_date}"
    )
  }
)



#' @title
#' Wells Fargo Reflect Credit Card Payment
#' @rdname wf_reflect_payment
#' @export
#' @importFrom tibble tibble

wf_reflect_payment <-
  function(balance,
           first_due_date = "2021-12-21",
           last_due_date = "2023-08-21",
           credit_limit = 18000,
           ending_balance = (18000/3)) {

    # Getting day number for last due date
    day_number <- as.character(lubridate::day(lubridate::as_date(last_due_date)))

    # Getting all dates between first due date and last due date
    all_dates <-
      as.character(lubridate::as_date(lubridate::as_date(first_due_date):lubridate::as_date(last_due_date)))

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
                (balance - ending_balance) / payments_left,
              payments_left = payments_left,
              credit_limit = credit_limit,
              ending_balance = ending_balance,
              last_payment_date = last_due_date
            )


          return(out)
        }
      }
    }
  }


#' @title
#' US Bank Payment
#' @rdname us_bank_payment
#' @export
#' @importFrom tibble tibble

us_bank_payment <-
  function(balance,
           ending_balance = (10000*.3)) {
    payment_due_dates <-
      c(
        "2021-09-16",
        "2021-10-16",
        "2021-11-16",
        "2021-12-16",
        "2022-01-16",
        "2022-02-16",
        "2022-03-16",
        "2022-04-16",
        "2022-05-16",
        "2022-06-16",
        "2022-07-16",
        "2022-08-16",
        "2022-09-16",
        "2022-10-16",
        "2022-11-16",
        "2022-12-16",
        "2023-01-16",
        "2023-02-16",
        "2023-03-16",
        "2023-04-16"
      )
    payment_due_dates <-
      as.Date(payment_due_dates)

    grace_period_start_date <-
      c(
        "2021-08-15",
        "2021-09-15",
        "2021-10-15",
        "2021-11-15",
        "2021-12-15",
        "2022-01-15",
        "2022-02-15",
        "2022-03-15",
        "2022-04-15",
        "2022-05-15",
        "2022-06-15",
        "2022-07-15",
        "2022-08-15",
        "2022-09-15",
        "2022-10-15",
        "2022-11-15",
        "2022-12-15",
        "2023-01-15",
        "2023-02-15",
        "2023-03-15"
      )

    grace_period_start_date <-
      as.Date(grace_period_start_date)

    grace_period_end_date <-
      c(
        "2021-09-16",
        "2021-10-16",
        "2021-11-16",
        "2021-12-16",
        "2022-01-16",
        "2022-02-16",
        "2022-03-16",
        "2022-04-16",
        "2022-05-16",
        "2022-06-16",
        "2022-07-16",
        "2022-08-16",
        "2022-09-16",
        "2022-10-16",
        "2022-11-16",
        "2022-12-16",
        "2023-01-16",
        "2023-02-16",
        "2023-03-16",
        "2023-04-16"
      )

    grace_period_end_date <-
      as.Date(grace_period_end_date)


    schedule_lookup <-
      tibble::tibble(
        payments_left = 20:1,
        grace_period_start_date =
          grace_period_start_date,
        grace_period_end_date =
          grace_period_end_date,
        payment_due_dates
      )

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
                (balance - ending_balance) / payments_left,
              payments_left = payments_left
            )


          return(out)
        }
      }
    }
  }
