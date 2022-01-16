# df <-
#   broca::read_cb()


# Change from previous day
calculate_delta <-
  function(x) {
    previous_day_value <-
      c(
        x[1],
        x[-length(x)]
      )


    map2(
      x,
      previous_day_value,
      function(x, y) {
        x - y
      }
    ) %>%
      unlist()
  }


df %>%
  mutate(Diff = calculate_delta(Close)) %>%
  mutate(Date_Value = mdy_hms(Date)) %>%
  mutate(Month = month(Date_Value,
    label = TRUE
  )) %>%
  mutate(Year = year(Date_Value)) %>%
  group_by(Month, Year) %>%
  summarize(Month_Diff = sum(Diff)) %>%
  ungroup() %>%
  arrange(Month_Diff)


df %>%
  mutate(Diff = calculate_delta(Close)) %>%
  mutate(Date_Value = mdy_hms(Date)) %>%
  mutate(Month = month(Date_Value,
    label = TRUE
  )) %>%
  mutate(Year = year(Date_Value)) %>%
  dplyr::filter(
    Month == "Dec",
    Year == 2015
  )
