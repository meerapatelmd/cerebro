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
           color,
           clarity,
           metal,
           gia_cut_grade = "",
           fluorescence = "") {
    StudEvaluation(
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
