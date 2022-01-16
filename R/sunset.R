#' @title
#' Sunrise/Sunset Data
#'
#' @description
#' Get the parsed response from https://api.sunrise-sunset.org
#' based on the given date range.
#'
#' @export
#' @import httr
#' @import R.cache
#' @import secretary
#' @rdname get_daylight_times

get_daylight_times <-
  function(latitude = 40.73,
           longitude = -74,
           start_date = "2022-01-10",
           end_date = "2022-12-31") {
    url <- "https://api.sunrise-sunset.org"

    input_dates <-
      as.character(
        as.Date(ymd(start_date):ymd(end_date),
          origin = "1970-01-01"
        )
      )

    output <-
      vector(
        mode = "list",
        length = length(input_dates)
      ) %>%
      set_names(as.character(input_dates))
    i <- 0

    for (input_date in input_dates) {
      i <- i + 1

      key <- list(input_date)

      cache_file_path <-
        R.cache::findCache(
          key = key,
          dirs = "cerebro"
        )

      if (is.null(cache_file_path)) {
        secretary::typewrite(input_date)
        secretary::typewrite_progress(
          iteration = i,
          total = length(output)
        )

        Sys.sleep(3)
        res <-
          GET(
            url = url,
            path = "json",
            query = list(
              lat = latitude,
              lng = longitude,
              date = input_date
            )
          )

        output[[input_date]] <-
          content(res)

        R.cache::saveCache(
          object = output[[input_date]],
          key = key,
          dirs = "cerebro"
        )

        # print(output[[input_date]])
      } else {
        output[[input_date]] <-
          R.cache::loadCache(
            key = key,
            dirs = "cerebro"
          )

        # print(output[[input_date]])
      }
    }

    am_pm_map <-
      tibble::tribble(
        ~`name`, ~`am_pm`,
        "sunrise", "AM",
        "sunset", "PM"
      )

    output %>%
      transpose() %>%
      pluck("results") %>%
      map(enframe) %>%
      bind_rows(.id = "date") %>%
      mutate(value = unlist(value)) %>%
      mutate(value = as.character(as_date((hms(value) - hms("05:00:00"))))) %>%
      mutate(
        value =
          str_replace_all(
            string = value,
            pattern = "1970-01-01 (.*$)",
            replacement = "\\1"
          )
      ) %>%
      inner_join(am_pm_map, by = "name") %>%
      unite(value,
        value, am_pm,
        sep = " "
      ) %>%
      unite(
        col = datetime,
        date, value,
        sep = " ",
        remove = FALSE
      ) %>%
      mutate(datetime = as.character(ymd_hms(datetime))) %>%
      mutate(
        value =
          str_replace_all(
            string = datetime,
            pattern = "(^.*?) (.*$)",
            replacement = "\\2"
          )
      ) %>%
      select(-datetime) %>%
      tidyr::pivot_wider(
        id_cols = date,
        names_from = name,
        values_from = value
      ) %>%
      mutate(day_length = hms(sunset) - hms(sunrise)) %>%
      mutate(day_length_hr = slot(day_length, "hour")) %>%
      mutate(day_length_min = slot(day_length, "minute")) %>%
      mutate(day_length = day_length_hr + (day_length_min / 60))
  }
