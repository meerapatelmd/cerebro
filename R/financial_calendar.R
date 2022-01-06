


# Create a financial calendar
library(tidyverse)
library(lubridate)

# Generate a long table that
# can be pivoted into a pretty year calendar
generate_year_tbl <-
  function(year = "2022") {

year_dates <-
  as.Date(ymd(sprintf("%s-01-01", year)):ymd(sprintf("%s-12-31", year)), origin = ymd("1970-01-01"))


day_of_week <-
  weekdays(x = year_dates,
         abbreviate = TRUE)
month_day_number <-
  day(year_dates)
month_name <-
  month(year_dates,
        label = TRUE)
week_number <-
  epiweek(year_dates)

raw_calendar_tbl <-
tibble(
  year_dates,
  day_of_week,
  week_number,
  month_name,
  month_day_number
)

weekday_index_map <-
  tribble(
    ~week_day_index, ~day_of_week,
    1, "Sun",
    2, "Mon",
    3, "Tue",
    4, "Wed",
    5, "Thu",
    6, "Fri",
    7, "Sat")

raw_calendar_tbl_2 <-
  left_join(raw_calendar_tbl,
            weekday_index_map,
            by = "day_of_week")


raw_calendar_tbl_3 <-
  split(raw_calendar_tbl_2,
        raw_calendar_tbl_2$month_name)

raw_calendar_tbl_4 <-
  raw_calendar_tbl_3 %>%
  map(fill_in_partial_1st_week) %>%
  map(function(x)
    tidyr::pivot_wider(x,
                       id_cols = c(week_number),
                       names_from  = day_of_week,
                       values_from = month_day_number) %>%
      mutate_all(as.character) %>%
      mutate_all(~ifelse(is.na(.), '', .))) %>%
  bind_rows(.id = "month_name")

out <-
raw_calendar_tbl_4 %>%
  pivot_longer(cols = Sun:Sat,
               names_to = "day_of_week",
               values_to = "month_day_number")

class(out) <-
  c(
    "year_tbl",
    class(out))

out

}

# Fill in the partially complete weeks at the beginning
# of each month
fill_in_partial_1st_week <-
  function(tibble) {


    # If week_day_index is not 1, then filling in is needed
    # for the first week number
    first_day_of_month_index <-
      df$week_day_index[1]
    if (first_day_of_month_index == 1) {

      return(df %>%
               mutate_all(as.character))

    } else {

      # Days to add to the partial first week
      days_to_add <-
      weekday_index_map %>%
        dplyr::slice(1:(first_day_of_month_index-1)) %>%
        # No numbers (blank)
        mutate(month_day_number = "") %>%
        # Add week number
        mutate(week_number = df$week_number[1])

      bind_rows(days_to_add,
                df %>%
                  mutate(month_day_number = as.character(month_day_number))) %>%
        mutate_all(as.character)



    }


  }


render_year_calendar <-
  function(year_calendar) {

    pivot_wider(
      data = year_calendar,
      id_cols = c(month_name, week_number),
      names_from = day_of_week,
      values_from = month_day_number
    )

  }


pretty_year_calendar <-
  function(year_calendar) {

    render_year_calendar(
      year_calendar = year_calendar) %>%
      huxtable::hux()


  }



# Pay days are on the last days of the month if the last day falls on a business
# day. If not, then pay day is the day before.
annotate_pay_day <-
  function(year_calendar) {
    raw_calendar_tbl <-
      year_calendar %>%
      mutate(month_day_number = as.integer(month_day_number))

pay_day_2_map <-
raw_calendar_tbl %>%
  group_by(month_name) %>%
  mutate(last_day_of_month = max(month_day_number, na.rm = TRUE)) %>%
  ungroup() %>%
  dplyr::filter(month_day_number == last_day_of_month) %>%
  dplyr::mutate(as.integer(last_day_of_month)) %>%
  mutate(pay_day =
           case_when(day_of_week == "Sat" ~ as.double(last_day_of_month-1),
                     day_of_week == "Sun" ~ as.double(last_day_of_month-2),
                     TRUE ~ as.double(last_day_of_month))) %>%
  select(month_name, pay_day)

pay_day_1_map <-
  raw_calendar_tbl %>%
  dplyr::filter(month_day_number == 15) %>%
  mutate(pay_day =
           case_when(day_of_week == "Sat" ~ as.double(month_day_number-1),
                     day_of_week == "Sun" ~ as.double(month_day_number-2),
                     TRUE ~ as.double(month_day_number))) %>%
  select(month_name, pay_day)


pay_day_map <-
  bind_rows(
    pay_day_1_map,
    pay_day_2_map) %>%
  mutate_all(as.character) %>%
  arrange(month_name,
          pay_day)

pay_day_map <-
raw_calendar_tbl_4_dpvt %>%
  left_join(pay_day_map %>%
               mutate(month_day_number = pay_day),
             by = c("month_name", "month_day_number")) %>%
  mutate(pay_day = ifelse(is.na(pay_day),
                          "",
                          "+4000"))

pay_day_map
  }


