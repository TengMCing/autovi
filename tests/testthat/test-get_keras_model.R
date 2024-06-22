test_that("model listing works", {
  skip_on_cran()
  expect_no_error(list_keras_model())
})


test_that("model download works", {
  skip_on_cran()
  expect_no_error(get_keras_model("vss_phn_32"))
})
