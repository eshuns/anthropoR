library(testthat)
library(anthropoR)

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