
MonthlyPayment <-
  setClass(
    "MonthlyPayment",
    slots =
      c(
        value = "numeric",
        payments_left = "integer"
      )
  )

setMethod(
  f = "show",
  signature = "MonthlyPayment",
  definition = function(object) {
    monthly_payment <- object@value
    payments_left <- object@payments_left
    cli::cli_text(
      "{monthly_payment} at {payments_left} payments left."
    )
  }
)



#' @title
#' Wells Fargo Reflect Credit Card Payment
#' @rdname wf_reflect_payment
#' @export
#' @importFrom tibble tibble

wf_reflect_payment <-
  function(balance) {
    payment_due_date <-
      c(
        as.Date("2021-12-21"),
        as.Date("2022-01-21"),
        as.Date("2022-02-21"),
        as.Date("2022-03-21"),
        as.Date("2022-04-21"),
        as.Date("2022-05-21"),
        as.Date("2022-06-21"),
        as.Date("2022-07-21"),
        as.Date("2022-08-21"),
        as.Date("2022-09-21"),
        as.Date("2022-10-21"),
        as.Date("2022-11-21"),
        as.Date("2022-12-21"),
        as.Date("2023-01-21"),
        as.Date("2023-02-21"),
        as.Date("2023-03-21"),
        as.Date("2023-04-21"),
        as.Date("2023-05-21"),
        as.Date("2023-06-21"),
        as.Date("2023-07-21"),
        as.Date("2023-08-21")
      )


    grace_period_start_date <-
      c(
        as.Date("2021-11-21"),
        as.Date("2021-12-21"),
        as.Date("2022-01-21"),
        as.Date("2022-02-21"),
        as.Date("2022-03-21"),
        as.Date("2022-04-21"),
        as.Date("2022-05-21"),
        as.Date("2022-06-21"),
        as.Date("2022-07-21"),
        as.Date("2022-08-21"),
        as.Date("2022-09-21"),
        as.Date("2022-10-21"),
        as.Date("2022-11-21"),
        as.Date("2022-12-21"),
        as.Date("2023-01-21"),
        as.Date("2023-02-21"),
        as.Date("2023-03-21"),
        as.Date("2023-04-21"),
        as.Date("2023-05-21"),
        as.Date("2023-06-21"),
        as.Date("2023-07-21")
      )

    grace_period_end_date <-
      c(
        as.Date("2021-12-20"),
        as.Date("2022-01-20"),
        as.Date("2022-02-20"),
        as.Date("2022-03-20"),
        as.Date("2022-04-20"),
        as.Date("2022-05-20"),
        as.Date("2022-06-20"),
        as.Date("2022-07-20"),
        as.Date("2022-08-20"),
        as.Date("2022-09-20"),
        as.Date("2022-10-20"),
        as.Date("2022-11-20"),
        as.Date("2022-12-20"),
        as.Date("2023-01-20"),
        as.Date("2023-02-20"),
        as.Date("2023-03-20"),
        as.Date("2023-04-20"),
        as.Date("2023-05-20"),
        as.Date("2023-06-20"),
        as.Date("2023-07-20"),
        as.Date("2023-08-20")
      )

    iteration_lookup <-
      tibble::tibble(
        payments_left = 21:1,
        grace_period_start_date =
          grace_period_start_date,
        grace_period_end_date =
          grace_period_end_date,
        payment_due_date
      )


    for (i in 1:nrow(iteration_lookup)) {
      start_date <-
        iteration_lookup$grace_period_start_date[i]

      end_date <-
        iteration_lookup$grace_period_end_date[i]

      if (Sys.Date() >= start_date) {
        if (Sys.Date() <= end_date) {
          payments_left <-
            iteration_lookup$payments_left[i]


          out <-
            MonthlyPayment(
              value =
                balance / payments_left,
              payments_left = payments_left
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
  function(balance) {
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


    iteration_lookup <-
      tibble::tibble(
        payments_left = 20:1,
        grace_period_start_date =
          grace_period_start_date,
        grace_period_end_date =
          grace_period_end_date,
        payment_due_dates
      )

    for (i in 1:nrow(iteration_lookup)) {
      start_date <-
        iteration_lookup$grace_period_start_date[i]

      end_date <-
        iteration_lookup$grace_period_end_date[i]

      if (Sys.Date() >= start_date) {
        if (Sys.Date() <= end_date) {
          payments_left <-
            iteration_lookup$payments_left[i]


          out <-
            MonthlyPayment(
              value =
                balance / payments_left,
              payments_left = payments_left
            )


          return(out)
        }
      }
    }
  }
