library(testthat)
library(anthropoR)

# --- computeBMI tests ---

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