make_project_folders <-
  function(path = "~/Desktop/2022 Fine Jewelry Capsule") {

    if (!dir.exists(path)) {

      dir.create(path)


    }

    screenshot_folder <-
      file.path(path, "screenshots")


    if (!dir.exists(screenshot_folder)) {

      dir.create(screenshot_folder)

    }


    studs_folder <-
      file.path(screenshot_folder, "studs")

    if (!dir.exists(studs_folder)) {

      dir.create(studs_folder)

    }

    studs_csv_file <-
      file.path(path, "studs.csv")

    if (!file.exists(studs_csv_file)) {
      studs_table <-
      tibble::tribble(
        ~record_datetime,
        ~retailer,
        ~url,
        ~listed_cost ,
        ~metal ,
        ~total_carats ,
        ~cost_per_carat ,
        ~color ,
        ~clarity ,
        ~gia_cut_grade ,
        ~fluorescence
      )

      readr::write_csv(
        x = studs_table,
        file = studs_csv_file
      )

    }


  }

