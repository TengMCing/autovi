test_that("image conversion works", {
  skip_on_cran()
  kw <- keras_wrapper(get_keras_model("vss_phn_32"))
  path <- save_plot(ggplot2::ggplot(cars) +
                      ggplot2::geom_point(ggplot2::aes(dist, speed)))
  expect_equal(kw$get_input_height(), 32L)
  expect_equal(kw$get_input_width(), 32L)
  expect_true("python.builtin.object" %in% class(kw$image_to_array(path)))
})

test_that("image prediction works", {
  skip_on_cran()
  kw <- keras_wrapper(get_keras_model("vss_phn_32"))
  path <- save_plot(ggplot2::ggplot(cars) +
                      ggplot2::geom_point(ggplot2::aes(dist, speed)))
  expect_equal(nrow(kw$predict(kw$image_to_array(path), matrix(1, ncol = 5))), 1)
})
