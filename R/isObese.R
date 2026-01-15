#' Classify Obesity (Adults & Children)
#'
#' Adds a binary obesity column to a data frame.
#' Automatically computes BMI if not provided.
#'
#' @param df Data frame.
#' @param bmi Column name for BMI (optional if weight/height provided).
#' @param weight_lb Column name for weight in pounds (optional).
#' @param weight_kg Column name for weight in kilograms (optional).
#' @param feet Column name for height in feet (optional).
#' @param inches Column name for remaining inches (optional).
#' @param height_m Column name for height in meters (optional).
#' @param height_in Column name for height in inches (optional).
#' @param sex Column name for sex (1 = male, 0 = female).
#' @param age_years Column name for age in years (optional if age_months provided).
#' @param age_months Column name for age in months (optional if age_years provided).
#' @param out_name Name of the binary obesity column (default = "obese").
#' @return Data frame with BMI (if calculated) and binary obesity column.
#' @export
#' @examples
#' # Example 1: Adults only, weight in pounds, height in inches
#' df1 <- data.frame(
#'   sex = c(1, 0, 1),
#'   age_years = c(25, 30, 40),
#'   weight_lb = c(180, 160, 220),
#'   height_in = c(70, 64, 68)
#' )
#' isObese(df1, sex = "sex", age_years = "age_years",
#'         weight_lb = "weight_lb", height_in = "height_in")
#'
#' # Example 2: Children only, weight in kg, height in meters
#' df2 <- data.frame(
#'   sex = c(1, 0),
#'   age_years = c(10, 12),
#'   weight_kg = c(35, 40),
#'   height_m = c(1.40, 1.45)
#' )
#' isObese(df2, sex = "sex", age_years = "age_years",
#'         weight_kg = "weight_kg", height_m = "height_m")
#'
#' # Example 3: Mixed adults and children
#' df3 <- data.frame(
#'   sex = c(1, 0, 1),
#'   age_years = c(25, 12, 8),
#'   weight_lb = c(180, 70, 50),
#'   height_in = c(70, 55, 50)
#' )
#' isObese(df3, sex = "sex", age_years = "age_years",
#'         weight_lb = "weight_lb", height_in = "height_in")
isObese <- function(df,
                    bmi = NULL,
                    weight_lb = NULL,
                    weight_kg = NULL,
                    feet = NULL,
                    inches = NULL,
                    height_m = NULL,
                    height_in = NULL,
                    sex,
                    age_years = NULL,
                    age_months = NULL,
                    out_name = "obese") {
  
  df2 <- df
  
  # ---- Compute BMI if not provided ----
  if (is.null(bmi)) {
    if (is.null(weight_lb) & is.null(weight_kg)) {
      stop("Provide either a BMI column or weight + height to compute BMI")
    }
    df2 <- computeBMI(
      df2,
      weight_lb = weight_lb,
      weight_kg = weight_kg,
      feet = feet,
      inches = inches,
      height_m = height_m,
      height_in = height_in,
      out_name = "bmi"
    )
    bmi <- "bmi"
  }
  
  # ---- Compute age in years and months ----
  age_y <- if (!is.null(age_years)) df2[[age_years]] else rep(NA_real_, nrow(df2))
  age_m <- if (!is.null(age_months)) df2[[age_months]] else rep(NA_real_, nrow(df2))
  
  age_years_calc <- ifelse(is.na(age_y) & !is.na(age_m), age_m / 12, age_y)
  age_months_calc <- ifelse(is.na(age_m) & !is.na(age_y), age_y * 12, age_m)
  
  # ---- Prepare sex ----
  male <- as.numeric(df2[[sex]])
  
  # ---- CDC eligibility for children ----
  cdc_ok <- !is.na(df2[[bmi]]) & !is.na(age_months_calc) & !is.na(male) &
    age_months_calc >= 24 & age_months_calc <= 239
  
  # ---- Initialize obesity column ----
  df2[[out_name]] <- NA_real_
  
  # ---- Compute CDC percentile where eligible ----
  cdc_pct <- rep(NA_real_, nrow(df2))
  eligible <- which(cdc_ok)
  if (length(eligible) > 0) {
    cdc_pct[eligible] <- 100 * pedbp::p_bmi_for_age(
      q = df2[[bmi]][eligible],
      male = male[eligible],
      age = age_months_calc[eligible],
      source = "CDC"
    )
  }
  
  # ---- Adult vs child classification ----
  adult_idx <- which(!is.na(df2[[bmi]]) & !is.na(age_years_calc) & age_years_calc >= 20)
  child_idx <- which(cdc_ok & !is.na(df2[[bmi]]))
  
  df2[[out_name]][adult_idx] <- ifelse(df2[[bmi]][adult_idx] >= 30, 1, 0)
  df2[[out_name]][child_idx] <- ifelse(cdc_pct[child_idx] >= 95, 1, 0)
  
  return(df2)
}
