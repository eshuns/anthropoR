#' Compute Body Mass Index (BMI)
#'
#' Adds a BMI column to the data frame.
#' Accepts height in feet+inches, inches only, or meters.
#' Weight can be in pounds ("lb") or kilograms ("kg").
#'
#' @param df Data frame.
#' @param weight_lb Column name for weight in pounds (optional).
#' @param weight_kg Column name for weight in kilograms (optional).
#' @param feet Column name for height in feet (optional).
#' @param inches Column name for remaining inches (optional).
#' @param height_m Column name for height in meters (optional).
#' @param height_in Column name for height in inches (optional).
#' @param out_name Name of BMI column (default = "bmi").
#' @return Data frame with BMI column added.
#' @export
#' @examples
#' # Example 1: Weight in pounds, height in inches
#' df1 <- data.frame(
#'   weight_lb = c(180, 150),
#'   height_in = c(70, 65)
#' )
#' computeBMI(df1, weight_lb = "weight_lb", height_in = "height_in")
#'
#' # Example 2: Weight in kilograms, height in meters
#' df2 <- data.frame(
#'   weight_kg = c(70, 80),
#'   height_m = c(1.75, 1.80)
#' )
#' computeBMI(df2, weight_kg = "weight_kg", height_m = "height_m")
#'
#' # Example 3: Height in feet + inches, weight in pounds
#' df3 <- data.frame(
#'   weight_lb = c(180, 160),
#'   feet = c(5, 5),
#'   inches = c(10, 4)
#' )
#' computeBMI(df3, weight_lb = "weight_lb", feet = "feet", inches = "inches")
#'
#' # Example 4: Mixed data: some rows with height in meters, some in inches
#' df4 <- data.frame(
#'   weight_kg = c(70, 80),
#'   height_m = c(1.75, NA),
#'   height_in = c(NA, 70)
#' )
#' computeBMI(df4, weight_kg = "weight_kg", height_m = "height_m", height_in = "height_in")
computeBMI <- function(df,
                       weight_lb = NULL,
                       weight_kg = NULL,
                       height_m = NULL,
                       height_in = NULL,
                       feet = NULL,
                       inches = NULL,
                       out_name = "bmi") {
  
  n <- nrow(df)
  
  # ----------------------------
  # Compute weight in kg
  # ----------------------------
  weight_kg_calc <- rep(NA_real_, n)
  
  if (!is.null(weight_lb)) {
    idx <- !is.na(df[[weight_lb]])
    weight_kg_calc[idx] <- df[[weight_lb]][idx] * 0.453592
  }
  
  if (!is.null(weight_kg)) {
    idx <- !is.na(df[[weight_kg]])
    weight_kg_calc[idx] <- df[[weight_kg]][idx]  # overwrite only non-NA
  }
  
  # ----------------------------
  # Compute height in meters
  # ----------------------------
  height_m_calc <- rep(NA_real_, n)
  
  if (!is.null(height_m)) {
    idx <- !is.na(df[[height_m]])
    height_m_calc[idx] <- df[[height_m]][idx]
  }
  
  if (!is.null(height_in)) {
    idx <- !is.na(df[[height_in]]) & is.na(height_m_calc)
    height_m_calc[idx] <- df[[height_in]][idx] * 0.0254
  }
  
  if (!is.null(feet)) {
    inches_val <- if (!is.null(inches)) df[[inches]] else 0
    total_in <- df[[feet]] * 12 + inches_val
    idx <- !is.na(total_in) & is.na(height_m_calc)
    height_m_calc[idx] <- total_in[idx] * 0.0254
  }
  
  # ----------------------------
  # Compute BMI
  # ----------------------------
  df[[out_name]] <- ifelse(
    !is.na(weight_kg_calc) & !is.na(height_m_calc) & height_m_calc > 0,
    weight_kg_calc / (height_m_calc^2),
    NA_real_
  )
  
  return(df)
}
