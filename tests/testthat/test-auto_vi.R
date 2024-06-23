test_that("plot drawing works", {
  skip_on_cran()
  myvi <- auto_vi(lm(dist ~ speed, data = cars))
  expect_no_error(myvi$plot_resid())
})

test_that("prediction works", {
  skip_on_cran()
  myvi <- auto_vi(lm(dist ~ speed, data = cars), get_keras_model("vss_phn_32"))
  expect_equal(myvi$vss()$vss, 3.161841, tolerance = 0.01)
})

test_that("check works", {
  skip_on_cran()
  myvi <- auto_vi(lm(dist ~ speed, data = cars), get_keras_model("vss_phn_32"))
  myvi$check()
  expect_equal(myvi$check_result$observed$vss, 3.161841, tolerance = 0.01)
  expect_equal(nrow(myvi$check_result$null), 100)
  expect_equal(nrow(myvi$check_result$boot), 100)
})
