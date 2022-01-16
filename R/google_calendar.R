



get_monthly_dates <-
  function(year = "2022",
           day_number = "15",
           offset = c("none", "previous_business_day", "next_business_day"),
           verbose = TRUE) {

    offset <-
    match.arg(
      arg = offset,
      choices = c("none", "previous_business_day", "next_business_day"),
      several.ok = FALSE
    )

    output <-
    sprintf("%s-%s-%s",
            year,
            1:12,
            day_number)



    if (offset == "none") {

      return(output)

    } else if (offset == "previous_business_day") {

      previous_business_days(input_dates = output,
                             verbose = verbose)


    } else if (offset == "next_business_day") {

      next_business_days(input_dates = output,
                         verbose = verbose)


    }


  }
