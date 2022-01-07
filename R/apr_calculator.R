
AprMultiplier <-
  setClass(
    "AprMultiplier",
    slots =
      c(
        value = "numeric",
        apr = "numeric",
        compound_period = "character",
        n_compound_period = "numeric"
      )
  )

setMethod(
  f = "show",
  signature = "AprMultiplier",
  definition = function(object) {
    multiplier <- object@value
    multiplier <- cli::style_bold(multiplier)

    apr <- object@apr
    apr <- cli::style_italic(apr)

    n_compound_period <- object@n_compound_period
    n_compound_period <-
      cli::style_italic(n_compound_period)

    compound_period <- object@compound_period
    compound_period <-
      cli::style_italic(compound_period)

    cli::cli_text(
      "{multiplier} at {apr}% APR for a {n_compound_period} {compound_period} period"
    )
  }
)


EstimatedAccountBalance <-
  setClass(
    "EstimatedAccountBalance",
    slots =
      c(
        value = "numeric",
        principle = "numeric",
        total_deposit = "numeric",
        apr = "numeric",
        total_interest_earned = "numeric",
        interest_earned_on_principle = "numeric",
        interest_earned_on_principle_per_compound_period = "numeric",
        interest_earned_on_deposit = "numeric",
        interest_earned_on_deposit_per_compound_period = "numeric",
        each_deposit = "list"
      ),
    contains = c("AprMultiplier")
  )



fmt_currency <-
  function(x) {
    sprintf(
      "$%s",
      prettyNum(
        round(x,
          digits = 0
        ),
        big.mark = ",",
        scientific = FALSE
      )
    )
  }

setMethod(
  f = "show",
  signature = "EstimatedAccountBalance",
  definition = function(object) {
    value <- object@value
    principle <- object@principle
    total_deposit <- object@total_deposit
    apr <- object@apr
    total_interest_earned <- object@total_interest_earned
    interest_earned_on_principle <- object@interest_earned_on_principle
    interest_earned_on_principle_per_compound_period <- object@interest_earned_on_principle_per_compound_period
    interest_earned_on_deposit <- object@interest_earned_on_deposit
    interest_earned_on_deposit_per_compound_period <- object@interest_earned_on_deposit_per_compound_period

    value <- fmt_currency(value)
    principle <- fmt_currency(principle)
    total_deposit <- fmt_currency(total_deposit)
    apr <- paste0(formatC(round(apr, 2),
      format = "f",
      digits = 2
    ), "%")
    total_interest_earned <- fmt_currency(total_interest_earned)
    interest_earned_on_principle <- fmt_currency(interest_earned_on_principle)
    interest_earned_on_principle_per_compound_period <- fmt_currency(interest_earned_on_principle_per_compound_period)
    interest_earned_on_deposit <- fmt_currency(interest_earned_on_deposit)
    interest_earned_on_deposit_per_compound_period <- fmt_currency(interest_earned_on_deposit_per_compound_period)


    compound_period <- object@compound_period
    n_compound_period <- object@n_compound_period


    cat(
      glue::glue(
        cli::style_bold(cli::style_underline("{value} in a {n_compound_period} {compound_period} period")),
        "principle: {principle}",
        "total_deposit: {total_deposit}",
        "apr: {apr}",
        "total_interest_earned: {total_interest_earned}",
        "interest_earned_on_principle: {interest_earned_on_principle}",
        "interest_earned_on_principle_per_{compound_period}: {interest_earned_on_principle_per_compound_period}",
        "interest_earned_on_deposit: {interest_earned_on_deposit}",
        "interest_earned_on_deposit_per_{compound_period}: {interest_earned_on_deposit_per_compound_period}",
        .sep = "\n"
      )
    )
  }
)


EstimatedDepositBalance <-
  setClass(
    "EstimatedDepositBalance",
    slots =
      c(
        value = "numeric",
        deposit = "numeric",
        interest_earned = "numeric",
        interest_earned_per_compound_period = "numeric"
      ),
    contains = c("AprMultiplier")
  )


TotalEstimatedDepositBalance <-
  setClass(
    "TotalEstimatedDepositBalance",
    slots =
      c(
        value = "numeric",
        total_deposit = "numeric",
        interest_earned = "numeric",
        interest_earned_per_compound_period = "numeric",
        compound_period = "character",
        n_compound_period = "numeric",
        each_deposit = "list"
      )
  )


#' @title
#' Calculate Multiplier
#'
#' @rdname calculate_multiplier

calculate_multiplier <-
  function(apr = 0.5,
           compound_period = "month",
           n_compound_period = 12) {
    compound_period <-
      match.arg(compound_period,
        choices = c("day", "week", "month", "year"),
        several.ok = FALSE
      )

    if (identical(compound_period, "day")) {
      scaled_interest <-
        (apr / 365)
    } else if (identical(compound_period, "week")) {
      scaled_interest <-
        apr / 52
    } else if (identical(compound_period, "month")) {
      scaled_interest <-
        apr / 12
    } else if (identical(compound_period, "year")) {
      scaled_interest <-
        apr
    }


    out <- (1 + (scaled_interest / 100))^n_compound_period

    AprMultiplier(
      value = out,
      apr = apr,
      compound_period = compound_period,
      n_compound_period = n_compound_period
    )
  }

#' @title
#' Estimate Account Balance
#'
#' @export
#' @rdname estimate_account_balance
#' @import tidyverse

estimate_account_balance <-
  function(principle = 30000,
           apr = 0.5,
           compound_period = "month",
           deposit_per_compound_period = 0,
           n_compound_period = 12) {
    apr_multiplier <-
      calculate_multiplier(
        apr = apr,
        compound_period = compound_period,
        n_compound_period = n_compound_period
      )


    # Deposits
    deposit_apr_multipliers <- list()
    deposit_compound_period <- n_compound_period
    while (deposit_compound_period > 0) {
      deposit_apr_multipliers[[length(deposit_apr_multipliers) + 1]] <-
        calculate_multiplier(
          apr = apr,
          compound_period = compound_period,
          n_compound_period = deposit_compound_period
        )


      deposit_compound_period <- deposit_compound_period - 1
    }


    deposit_balances <- list()
    for (i in seq_along(deposit_apr_multipliers)) {
      deposit_balances[[i]] <-
        EstimatedDepositBalance(
          value = deposit_per_compound_period * deposit_apr_multipliers[[i]]@value,
          deposit = deposit_per_compound_period,
          interest_earned = (deposit_apr_multipliers[[i]]@value * deposit_per_compound_period) - deposit_per_compound_period,
          interest_earned_per_compound_period = (((deposit_apr_multipliers[[i]]@value * deposit_per_compound_period) - deposit_per_compound_period) / deposit_apr_multipliers[[i]]@n_compound_period),
          compound_period = deposit_apr_multipliers[[i]]@compound_period,
          n_compound_period = deposit_apr_multipliers[[i]]@n_compound_period
        )
    }

    all_deposits <-
      TotalEstimatedDepositBalance(
        value =
          deposit_balances %>%
            purrr::map(function(x) slot(x, "value")) %>%
            unlist() %>%
            sum(),
        total_deposit =
          deposit_balances %>%
            purrr::map(function(x) slot(x, "deposit")) %>%
            unlist() %>%
            sum(),
        interest_earned =
          deposit_balances %>%
            purrr::map(function(x) slot(x, "interest_earned")) %>%
            unlist() %>%
            sum(),
        interest_earned_per_compound_period =
          deposit_balances %>%
            purrr::map(function(x) slot(x, "interest_earned_per_compound_period")) %>%
            unlist() %>%
            sum(),
        compound_period = compound_period,
        n_compound_period = n_compound_period,
        each_deposit = deposit_balances
      )

    EstimatedAccountBalance(
      value = (apr_multiplier@value * principle) + all_deposits@value,
      principle = principle,
      total_deposit = all_deposits@total_deposit,
      apr = apr_multiplier@apr,
      total_interest_earned = ((apr_multiplier@value * principle) - principle) + all_deposits@interest_earned,
      interest_earned_on_principle = (apr_multiplier@value * principle) - principle,
      interest_earned_on_principle_per_compound_period = ((apr_multiplier@value * principle) - principle) / apr_multiplier@n_compound_period,
      interest_earned_on_deposit = all_deposits@interest_earned,
      interest_earned_on_deposit_per_compound_period = all_deposits@interest_earned_per_compound_period,
      compound_period = apr_multiplier@compound_period,
      n_compound_period = apr_multiplier@n_compound_period,
      each_deposit = all_deposits@each_deposit
    )
  }
