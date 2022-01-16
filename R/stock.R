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
