# autovi 0.4.1

## New Features
* Introduce `AUTO_VI$save_plot()` which is the default method of saving a plot by calling `save_plot()`. This allows user to override the plot saving method if needed.
* Introduce a method `AUTO_VI$summary()` which allows user to get computed statistics provided in `AUTO_VI$..str..()`.
* Introduce a method `AUTO_VI$plot_pair()` which allows user to put the true residual plot and a null plot side-by-side.
* Introduce `AUTO_VI$boot_method()` which is the default method of generating bootstrapped residuals. This allows user to override the bootstrapping scheme if needed.

## Changes
* Integrate the `AUTO_VI$select_feature()` method into `AUTO_VI$feature_pca()` for clarity. Now the `AUTO_VI$feature_pca()` method has one more parameter `pattern` for specifying feature name pattern.
* Remove the `type` parameter and `p_value_type` parameter from `AUTO_VI$p_value()` and `AUTO_VI$check()`, respectively, and unify the p-value formula. Now the p-value is always calculated as `mean(c(null_dist, vss) >= vss)`, where `null_dist` is a vector of visual signal strength for null residual plots, and `vss` is the visual signal strength for the true residual plot.

## Bug Fix
* Fix a bug in `AUTO_VI$vss()` that arguments will be passed incorrectly to `KERAS_WRAPPER$image_to_array()` when a `data.frame` or a `tibble` is provided by the user to predict visual signal strength.

# autovi 0.4.0

* First CRAN release
