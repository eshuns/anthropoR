library(testthat)
library(anthropoR)

# ----------------------------
# computeBMI tests
# ----------------------------

test_that("computeBMI calculates correctly for weight in kg and height in meters", {
  df <- data.frame(
    weight_kg = c(70, 80),
    height_m = c(1.75, 1.80)
  )
  df2 <- computeBMI(df, weight_kg = "weight_kg", height_m = "height_m")
  expect_equal(round(df2$bmi[1], 2), 22.86)
  expect_equal(round(df2$bmi[2], 2), 24.69)
})

test_that("computeBMI calculates correctly for weight in lb and height in inches", {
  df <- data.frame(
    weight_lb = c(150, 180),
    height_in = c(65, 70)
  )
  df2 <- computeBMI(df, weight_lb = "weight_lb", height_in = "height_in")
  expect_equal(round(df2$bmi[1], 2), 24.96)
  expect_equal(round(df2$bmi[2], 2), 25.83)
})

test_that("computeBMI calculates correctly for feet + inches", {
  df <- data.frame(
    weight_lb = c(180, 160),
    feet = c(5, 5),
    inches = c(10, 4)
  )
  df2 <- computeBMI(df, weight_lb = "weight_lb", feet = "feet", inches = "inches")
  expect_equal(round(df2$bmi[1], 2), 25.83)
  expect_equal(round(df2$bmi[2], 2), 27.46)
})

test_that("computeBMI handles missing values gracefully", {
  df <- data.frame(
    weight_lb = c(180, NA),
    height_in = c(70, 65)
  )
  df2 <- computeBMI(df, weight_lb = "weight_lb", height_in = "height_in")
  expect_equal(round(df2$bmi[1], 2), 25.83)
  expect_true(is.na(df2$bmi[2]))
})

# ----------------------------
# isObese tests
# ----------------------------

test_that("isObese correctly classifies adults", {
  df <- data.frame(
    sex = c(1, 0, 1),
    age_years = c(25, 30, 40),
    weight_lb = c(180, 160, 220),
    height_in = c(70, 64, 68)
  )
  df2 <- isObese(df,
                 weight_lb = "weight_lb",
                 height_in = "height_in",
                 sex = "sex",
                 age_years = "age_years")
  expect_equal(df2$obese, c(0, 0, 1))
})

test_that("isObese computes BMI if missing and classifies children/adults", {
  df <- data.frame(
    sex = c(1, 0, 1),
    age_years = c(10, 12, 25),
    weight_lb = c(70, 90, 180),
    height_in = c(55, 55, 70)
  )
  df2 <- isObese(df,
                 weight_lb = "weight_lb",
                 height_in = "height_in",
                 sex = "sex",
                 age_years = "age_years")
  
  # Adults: last row BMI >= 30? Expect 0
  expect_equal(df2$obese[3], 0)
  
  # Children: percentile-based, expect 0/1
  expect_true(all(df2$obese[1:2] %in% c(0,1)))
})

test_that("isObese handles missing age or sex gracefully", {
  df <- data.frame(
    sex = c(NA, 1),
    age_years = c(10, NA),
    weight_lb = c(70, 180),
    height_in = c(55, 70)
  )
  df2 <- isObese(df,
                 weight_lb = "weight_lb",
                 height_in = "height_in",
                 sex = "sex",
                 age_years = "age_years")
  expect_true(all(is.na(df2$obese[1:2])))
})
