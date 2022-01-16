weekday_index_map <-
  tibble::tribble(
    ~week_day_index, ~day_of_week,
    1, "Sun",
    2, "Mon",
    3, "Tue",
    4, "Wed",
    5, "Thu",
    6, "Fri",
    7, "Sat"
  )

month_index_map <-
  tibble::tribble(
    ~`month_number`, ~`month_name`,
    "01", "Jan",
    "02", "Feb",
    "03", "Mar",
    "04", "Apr",
    "05", "May",
    "06", "Jun",
    "07", "Jul",
    "08", "Aug",
    "09", "Sep",
    "10", "Oct",
    "11", "Nov",
    "12", "Dec"
  )

# Basic Columns for Ledger
core_cols <-
  c(
    "day_slot_index",
    "month_number",
    "month_name",
    "week_number",
    "day_of_week",
    "month_day_number",
    "year_date_str",
    "year_date"
  )


# Generate a long table that
# can be pivoted into a pretty year calendar
# Create a financial calendar

generate_annual_ledger <-
  function(year = "2022") {
    year_dates <-
      as.Date(
        ymd(sprintf("%s-01-01", year)):ymd(sprintf("%s-12-31", year)),
        origin = ymd("1970-01-01")
      )


    day_of_week <-
      weekdays(
        x = year_dates,
        abbreviate = TRUE
      )

    month_day_number <-
      day(year_dates)
    month_name <-
      month(year_dates,
        label = TRUE
      )
    month_number <-
      month(year_dates,
        label = FALSE
      )
    month_number <-
      stringr::str_pad(month_number,
        width = 2,
        side = "left",
        pad = "0"
      )

    ymd_date_str <-
      sprintf("%s-%s-%s", year, month_number, stringr::str_pad(month_day_number,
        width = 2,
        side = "left",
        pad = "0"
      ))
    week_number <-
      epiweek(year_dates)

    raw_calendar_tbl <-
      tibble(
        year_dates,
        ymd_date_str,
        day_of_week,
        week_number,
        month_name,
        month_day_number
      )

    raw_calendar_tbl_2 <-
      left_join(raw_calendar_tbl,
        weekday_index_map,
        by = "day_of_week"
      )


    raw_calendar_tbl_3 <-
      split(
        raw_calendar_tbl_2,
        raw_calendar_tbl_2$month_name
      )

    raw_calendar_tbl_4 <-
      raw_calendar_tbl_3 %>%
      map(fill_in_partial_1st_week) %>%
      map(function(x) {
        tidyr::pivot_wider(x,
          id_cols = c(week_number),
          names_from = day_of_week,
          values_from = month_day_number
        ) %>%
          mutate_all(as.character) %>%
          mutate_all(~ ifelse(is.na(.), "", .))
      }) %>%
      bind_rows(.id = "month_name")

    out <-
      raw_calendar_tbl_4 %>%
      pivot_longer(
        cols = Sun:Sat,
        names_to = "day_of_week",
        values_to = "month_day_number"
      ) %>%
      rowid_to_column(var = "day_slot_index") %>%
      dplyr::left_join(month_index_map,
        by = "month_name"
      ) %>%
      mutate(
        year_date_str =
          case_when(
            month_day_number != "" ~ sprintf("%s-%s-%s", year, month_number, month_day_number),
            TRUE ~ ""
          )
      ) %>%
      mutate(year_date = ymd(year_date_str))

    class(out) <-
      c(
        "ledger",
        class(out)
      )

    out
  }


# Fill in the partially complete weeks at the beginning
# of each month
fill_in_partial_1st_week <-
  function(tibble) {


    # If week_day_index is not 1, then filling in is needed
    # for the first week number
    first_day_of_month_index <-
      tibble$week_day_index[1]
    if (first_day_of_month_index == 1) {
      return(tibble %>%
        mutate_all(as.character))
    } else {

      # Days to add to the partial first week
      days_to_add <-
        weekday_index_map %>%
        dplyr::slice(1:(first_day_of_month_index - 1)) %>%
        # No numbers (blank)
        mutate(month_day_number = "") %>%
        # Add week number
        mutate(week_number = tibble$week_number[1])

      bind_rows(
        days_to_add,
        tibble %>%
          mutate(month_day_number = as.character(month_day_number))
      ) %>%
        mutate_all(as.character)
    }
  }


ledger_to_cal <-
  function(ledger_obj) {
    if (!("ledger" %in% class(ledger_obj))) {
      stop("`ledger_obj` must be of 'ledger' class.")
    }

    extra_cols <-
      ledger_obj %>%
      select(!any_of(core_cols)) %>%
      colnames()

    if (length(extra_cols) > 0) {
      value_df <-
        ledger_obj %>%
        select(
          day_slot_index,
          month_day_number,
          all_of(extra_cols)
        ) %>%
        mutate_at(
          vars(
            month_day_number,
            all_of(extra_cols)
          ),
          as.character
        ) %>%
        mutate_at(
          vars(
            month_day_number,
            all_of(extra_cols)
          ),
          ~ ifelse(is.na(.), " ", .)
        ) %>%
        unite(
          col = value,
          month_day_number,
          sep = "\n",
          dplyr::all_of(extra_cols),
          na.rm = FALSE
        )

      ledger_obj2 <-
        ledger_obj %>%
        left_join(value_df,
          by = "day_slot_index"
        )
    } else {
      ledger_obj2 <-
        ledger_obj %>%
        mutate(value = month_day_number)
    }

    out <-
      pivot_wider(
        data = ledger_obj2,
        id_cols = c(month_name, week_number),
        names_from = day_of_week,
        values_from = value
      )

    out <-
      left_join(
        month_index_map,
        out,
        by = "month_name"
      )

    class(out) <-
      c(
        "year_cal",
        class(out)
      )

    out
  }


pretty_year_calendar <-
  function(year_calendar) {
    if (!("year_cal" %in% class(year_calendar))) {
      stop("`year_calendar` must be of 'year_cal' class.")
    }

    huxtable::hux(year_calendar)
  }


cal_to_tbl <-
  function(year_calendar) {
    if (!("year_cal" %in% class(year_calendar))) {
      stop("`year_calendar` must be of 'year_cal' class.")
    }

    out <-
      tidyr::pivot_longer(
        year_calendar,
        cols = Sun:Sat,
        names_to = "day_of_week",
        values_to = "month_day_number",
        values_drop_na = FALSE
      ) %>%
      tibble::rowid_to_column(var = "day_slot_index") %>%
      mutate(
        year_date_str =
          case_when(
            month_day_number != "" ~ sprintf("%s-%s-%s", year, month_number, month_day_number),
            TRUE ~ ""
          )
      ) %>%
      mutate(year_date = ymd(year_date_str))

    class(out) <-
      c(
        "year_tbl",
        class(out)
      )

    out
  }


get_paycheck_tbl <-
  function(year = "2022",
           default_paycheck = "4000") {
    year_dates <-
      as.Date(
        ymd(sprintf("%s-01-01", year)):ymd(sprintf("%s-12-31", year)),
        origin = ymd("1970-01-01")
      ) %>%
      as.character()



    paycheck_date0 <-
      lubridate::days_in_month(year_dates) %>%
      enframe() %>%
      distinct() %>%
      mutate(
        month_number =
          stringr::str_pad(
            1:12,
            width = 2,
            pad = "0",
            side = "left"
          )
      ) %>%
      transmute(
        paycheck_1 =
          sprintf(
            "%s-%s-15",
            year,
            month_number
          ),
        paycheck_2 =
          sprintf(
            "%s-%s-%s",
            year,
            month_number,
            value
          )
      ) %>%
      unlist() %>%
      unname() %>%
      sort()

    paycheck_date <-
      sapply(
        paycheck_date0,
        previous_business_day
      )
    paycheck_date <-
      unname(paycheck_date)

    tibble(paycheck_date)
  }

# Pay days are on the last days of the month if the last day falls on a business
# day. If not, then pay day is the day before.
add_paycheck <-
  function(year_obj,
           default_paycheck = "4000",
           ...) {
    if ("list" %in% class(year_obj)) {
      year_tibble <-
        year_obj %>%
        reduce(left_join,
          by = "day_slot_index"
        )
    } else if ("year_cal" %in% class(year_obj)) {
      year_tibble <-
        cal_to_tbl(year_calendar = year_obj)
    } else {
      year_tibble <-
        year_obj
    }

    if (!missing(...)) {
      custom_paycheck_map <-
        enframe(rlang::list2(...),
          name = "year_date",
          value = "custom_paycheck"
        ) %>%
        mutate(year_date = ymd(year_date))
      custom_paycheck_map$custom_paycheck <-
        sprintf("+ %s", unlist(custom_paycheck_map$custom_paycheck))
    } else {
      custom_paycheck_map <-
        tibble::tribble(~year_date, ~custom_paycheck)
    }


    if (!("year_tbl" %in% class(year_tibble))) {
      stop("`year_tibble` must be of 'year_tbl' class.")
    }

    raw_calendar_tbl <-
      year_tibble %>%
      mutate(month_day_number = as.integer(month_day_number))

    pay_day_2_map <-
      raw_calendar_tbl %>%
      group_by(month_name) %>%
      mutate(last_day_of_month = max(month_day_number, na.rm = TRUE)) %>%
      ungroup() %>%
      dplyr::filter(month_day_number == last_day_of_month) %>%
      dplyr::mutate(as.integer(last_day_of_month)) %>%
      mutate(
        month_day_number =
          case_when(
            day_of_week == "Sat" ~ as.double(last_day_of_month - 1),
            day_of_week == "Sun" ~ as.double(last_day_of_month - 2),
            TRUE ~ as.double(last_day_of_month)
          )
      ) %>%
      select(month_name, month_day_number)

    pay_day_1_map <-
      raw_calendar_tbl %>%
      dplyr::filter(month_day_number == 15) %>%
      mutate(
        month_day_number =
          case_when(
            day_of_week == "Sat" ~ as.double(month_day_number - 1),
            day_of_week == "Sun" ~ as.double(month_day_number - 2),
            TRUE ~ as.double(month_day_number)
          )
      ) %>%
      select(month_name, month_day_number)


    pay_day_map <-
      bind_rows(
        pay_day_1_map,
        pay_day_2_map
      ) %>%
      mutate_all(as.character) %>%
      arrange(
        month_name,
        month_day_number
      ) %>%
      mutate(pay_day_number = month_day_number)

    pay_day_tibble <-
      year_tibble %>%
      left_join(pay_day_map,
        by = c("month_name", "month_day_number")
      ) %>%
      mutate(year_date = as.character(year_date)) %>%
      left_join(custom_paycheck_map %>%
        mutate(
          year_date =
            as.character(year_date)
        ),
      by = "year_date"
      ) %>%
      mutate(
        default_paycheck =
          case_when(
            !is.na(pay_day_number) ~ sprintf("+ %s", default_paycheck),
            TRUE ~ NA_character_
          )
      ) %>%
      mutate(
        paycheck =
          coalesce(
            custom_paycheck,
            default_paycheck
          )
      )

    pay_day_map <-
      pay_day_tibble %>%
      dplyr::select(
        day_slot_index,
        custom_paycheck,
        default_paycheck,
        paycheck
      ) %>%
      dplyr::filter(!is.na(paycheck)) %>%
      mutate_all(~ ifelse(is.na(.), "", .))

    class(pay_day_map) <-
      c(
        "paycheck_map",
        class(pay_day_map)
      )

    if ("list" %in% class(year_obj)) {
      out <-
        c(
          year_obj,
          pay_day_map =
            pay_day_map
        )
    } else {
      out <-
        list(
          year_tibble =
            pay_day_tibble %>%
              select(
                -custom_paycheck,
                -default_paycheck,
                -paycheck,
                -pay_day_number
              ),
          pay_day_map =
            pay_day_map
        )
    }

    out
  }


subtract_payment <-
  function(year_obj,
           ...) {
    if ("list" %in% class(year_obj)) {
      year_tibble <-
        year_obj %>%
        reduce(left_join,
          by = "day_slot_index"
        )
    } else if ("year_cal" %in% class(year_obj)) {
      year_tibble <-
        cal_to_tbl(year_calendar = year_obj)
    } else {
      year_tibble <-
        year_obj
    }


    payment_map <-
      enframe(unlist(rlang::list2(...)),
        name = "year_date",
        value = "payment"
      ) %>%
      mutate(year_date = as.character(year_date)) %>%
      tidyr::unnest(cols = payment) %>%
      mutate(payment = sprintf("- %s", payment))

    payment_tibble <-
      year_tibble %>%
      mutate(year_date = as.character(year_date)) %>%
      left_join(payment_map,
        by = "year_date"
      )


    if ("list" %in% class(year_obj)) {
      out <-
        list(
          year_tibble =
            payment_tibble %>%
              select(all_of(core_cols)),
          pay_day_map =
            year_obj$pay_day_map,
          payment_map =
            payment_tibble %>%
              select(
                day_slot_index,
                payment
              ) %>%
              dplyr::filter(!is.na(payment))
        )
    } else {
      out <-
        list(
          year_tibble =
            payment_tibble %>%
              select(all_of(core_cols)),
          payment_map =
            payment_tibble %>%
              select(
                day_slot_index,
                payment
              ) %>%
              dplyr::filter(!is.na(payment))
        )
    }
    out
  }
