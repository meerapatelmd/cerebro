# Declination Angle

declination_angle <-
  function(target_day) {
    target_day2 <-
      ymd(target_day,
        tz = "UTC"
      )

    target_year <-
      year(target_day2)


    d <-
      as.double(
        difftime(ymd(target_day),
          ymd(sprintf(
            "%s-01-01",
            target_year
          )),
          tz = "UTC",
          units = "days"
        )
      )

    d <- 61.4167
    D <- d + 10

    f <- 360 / 365
    -23.44 * cos(D * f)
  }


solar_hour_angle <-
  function(time_of_day = "14:00:00") {
    time_of_day2 <-
      hms(time_of_day) - hms("12:00:00")

    time_of_day3_a <-
      slot(
        time_of_day2,
        "hour"
      )

    time_of_day3_b <-
      slot(
        time_of_day2,
        "minute"
      ) / 60


    time_of_day3 <-
      time_of_day3_a + time_of_day3_b

    15 * time_of_day3
  }


solar_elevation_angle <-
  function(latitude = 32.22,
           target_day = "2022-01-12",
           time_of_day = "12:00:00") {
    x <-
      sin(latitude) * sin(-declination_angle(target_day = target_day)) +
      cos(latitude) * cos(declination_angle(target_day = target_day)) * cos(solar_hour_angle(time_of_day = time_of_day))
    x <- sin(x)
    1 / x
  }
