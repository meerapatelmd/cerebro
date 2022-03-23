#' @title
#' Calculate CC Payoff
#'
#' @description
#' Calculate how much a credit card will be paid off
#' with a given monthly payment.
#' @rdname calculate_cc_payoff
#' @export
#' @importFrom lubridate day as_date
#' @importFrom tibble tibble

calculate_cc_payoff <-
  function(monthly_payment,
           balance,
           first_due_date,
           last_due_date,
           credit_limit) {

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

          ending_balance <-
            balance-(monthly_payment*payments_left)

          out <-
            MonthlyPayment(
              value = monthly_payment,
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
