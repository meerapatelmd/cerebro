#' @title
#' Value Gold Jewelry
#'
#' @rdname value_gold
#' @export
#' @family Market Value

value_gold <-
  function(usd_per_ounce,
           ounces = 0.1,
           karats = c("24K", "18K", "14K", "10K")) {
    karats <-
      match.arg(
        arg = karats,
        choices = c("24K", "18K", "14K", "10K"),
        several.ok = FALSE
      )

    pure_gold_value <-
      usd_per_ounce * ounces

    if (identical(karats, "24K")) {
      return(pure_gold_value)
    } else if (identical(karats, "18K")) {
      0.75 * pure_gold_value
    } else if (identical(karats, "14K")) {
      0.58 * pure_gold_value
    } else if (identical(karats, "10K")) {
      0.42 * pure_gold_value
    }
  }



color <-
  c(
    "D",
    "E",
    "F",
    "G",
    "H",
    "I",
    "J",
    "K",
    "L",
    "M"
  )

clarity <-
  c(
    "FL",
    "IF",
    "VVS1",
    "VS1",
    "VS2",
    "SI1",
    "SI2"
  )

gia_cut_grade <-
  c(
    "Excellent",
    "Very Good",
    "Good"
  )

fluorescence <-
  c(
    "None",
    "Faint",
    "Medium",
    "Strong",
    "+Strong"
  )


StudEvaluation <-
  setClass(
    "StudEvaluation",
    slots =
      c(
        retailer = "character",
        url = "character",
        listed_cost = "numeric",
        metal = "character",
        total_carats = "numeric",
        cost_per_carat = "numeric",
        color = "character",
        clarity = "character",
        gia_cut_grade = "character",
        fluorescence = "character"
      )
  )

setMethod(
  f = "show",
  signature = "StudEvaluation",
  definition = function(object) {
    slot_names <-
      slotNames(object)

    for (slot_name in slot_names) {
      cli::cli_text("{slot_name}: {slot(object, slot_name)}\n")
    }
  }
)


#' @title
#' Evaluate Diamond Stud Earrings
#'
#' @description
#' Assumes brilliant cut diamond.
#'
#' @details
#' https://www.diamonds.pro/education/diamond-prices/
#' @rdname evaluate_stud_earrings
#' @family Market Value
#' @export

evaluate_stud_earrings <-
  function(listed_cost,
           total_carats,
           metal,
           retailer,
           url,
           color = c(
             "Not Listed",
             "D",
             "E",
             "F",
             "G",
             "H",
             "I",
             "J",
             "K",
             "L",
             "M"
           ),
           clarity =
             c(
               "Not Listed",
               "FL",
               "IF",
               "VVS1",
               "VS1",
               "VS2",
               "SI1",
               "SI2"
             ),
           gia_cut_grade =
             c(
               "Not Listed",
               "Excellent",
               "Very Good",
               "Good"
             ),
           fluorescence =
             c(
               "Not Listed",
               "None",
               "Faint",
               "Medium",
               "Strong",
               "+Strong"
             )) {
    gia_cut_grade <-
      match.arg(
        arg = gia_cut_grade,
        choices =
          c(
            "Not Listed",
            "Excellent",
            "Very Good",
            "Good"
          ),
        several.ok = FALSE
      )

    fluorescence <-
      match.arg(
        arg = fluorescence,
        choices =
          c(
            "Not Listed",
            "None",
            "Faint",
            "Medium",
            "Strong",
            "+Strong"
          ),
        several.ok = FALSE
      )

    color <-
      match.arg(
        arg = color,
        choices = c(
          "Not Listed",
          "D",
          "E",
          "F",
          "G",
          "H",
          "I",
          "J",
          "K",
          "L",
          "M"
        ),
        several.ok = FALSE
      )


    clarity <-
      match.arg(
        arg = clarity,
        choices =
          c(
            "Not Listed",
            "FL",
            "IF",
            "VVS1",
            "VS1",
            "VS2",
            "SI1",
            "SI2"
          ),
        several.ok = FALSE
      )



    StudEvaluation(
      retailer = retailer,
      url = url,
      listed_cost = listed_cost,
      metal = metal,
      total_carats = total_carats,
      cost_per_carat = listed_cost / total_carats,
      color = color,
      clarity = clarity,
      gia_cut_grade = gia_cut_grade,
      fluorescence = fluorescence
    )
  }






save_stud_evaluation <-
  function(stud_evaluation,
           path = "~/Desktop/2022 Fine Jewelry Capsule") {
    make_project_folders(path = path)

    studs_table <-
      tibble::tibble(
        record_datetime = Sys.time(),
        retailer = stud_evaluation@retailer,
        url = stud_evaluation@url,
        listed_cost = stud_evaluation@listed_cost,
        metal = stud_evaluation@metal,
        total_carats = stud_evaluation@total_carats,
        cost_per_carat = stud_evaluation@cost_per_carat,
        color = stud_evaluation@color,
        clarity = stud_evaluation@clarity,
        gia_cut_grade = stud_evaluation@gia_cut_grade,
        fluorescence = stud_evaluation@fluorescence
      )

    readr::write_csv(
      x = studs_table,
      file = file.path(path, "studs.csv"),
      append = TRUE
    )
  }
