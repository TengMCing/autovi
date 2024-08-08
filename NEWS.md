# autovi 0.4.1

* Introduce `AUTO_VI$save_plot()` which is the default method of saving a plot by calling `save_plot()`. This allows user to override the plot saving method if needed.
* Fix a bug in `AUTO_VI$vss()` that arguments will be passed incorrectly to `KERAS_WRAPPER$image_to_array()` when a `data.frame` or a `tibble` is provided by the user to predict visual signal strength.
* Remove the `type` parameter and `p_value_type` parameter from `AUTO_VI$p_value()` and `AUTO_VI$check()`, respectively, and unify the p-value formula. Now the p-value is always calculated as `mean(c(null_dist, vss) >= vss)`, where `null_dist` is a vector of visual signal strength for null residual plots, and `vss` is the visual signal strength for the true residual plot.

# autovi 0.4.0

* First CRAN release
