% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/devel-market_value.R
\name{evaluate_stud_earrings}
\alias{evaluate_stud_earrings}
\title{Evaluate Diamond Stud Earrings}
\usage{
evaluate_stud_earrings(
  listed_cost,
  total_carats,
  metal,
  retailer,
  url,
  color = c("Not Listed", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M"),
  clarity = c("Not Listed", "FL", "IF", "VVS1", "VS1", "VS2", "SI1", "SI2"),
  gia_cut_grade = c("Not Listed", "Excellent", "Very Good", "Good"),
  fluorescence = c("Not Listed", "None", "Faint", "Medium", "Strong", "+Strong")
)
}
\description{
Assumes brilliant cut diamond.
}
\details{
https://www.diamonds.pro/education/diamond-prices/
}
\seealso{
Other Market Value: 
\code{\link{value_gold}()}
}
\concept{Market Value}
