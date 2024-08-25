# autovi 0.4.1

## New Features
* Introduce `AUTO_VI$save_plot()` which is the default method of saving a plot by calling `save_plot()`. This allows user to override the plot saving method if needed.
* Introduce a method `AUTO_VI$summary()` which allows user to get computed statistics provided in `AUTO_VI$..str..()`.
* Introduce a method `AUTO_VI$plot_pair()` which allows user to put the true residual plot and a null plot side-by-side.
* Introduce a method `AUTO_VI$plot_lineup()` which allows user to generate a lineup for manual inspection.
* Introduce `AUTO_VI$boot_method()` which is the default method of generating bootstrapped residuals. This allows user to override the bootstrapping scheme if needed.
* Introduce `residual_checker()` as a new class constructor of `AUTO_VI`. It has an argument `keras_model_name` that will be passed to `get_keras_model()`.

## Changes
* Integrate the `AUTO_VI$select_feature()` method into `AUTO_VI$feature_pca()` for clarity. Now the `AUTO_VI$feature_pca()` method has one more parameter `pattern` for specifying feature name pattern.
* Remove the `type` parameter and `p_value_type` parameter from `AUTO_VI$p_value()` and `AUTO_VI$check()`, respectively, and unify the p-value formula. Now the p-value is always calculated as `mean(c(null_dist, vss) >= vss)`, where `null_dist` is a vector of visual signal strength for null residual plots, and `vss` is the visual signal strength for the true residual plot.
* Improve `AUTO_VI$feature_pca_plot()`. Now the observed point is always displayed on top of other groups.
* `AUTO_VI$check()` and `AUTO_VI$lineup_check()` now returns `self` instead of `invisible(self)` to provide a visible summary of the check result.
* `get_keras_model()` now downloads models in the `TensorFlow` "SavedModel" format by default, rather than the `.keras` format. While this change may slow down the model loading process due to the larger file size, it ensures better backward compatibility with various Python and `TensorFlow` versions. User can still choose to download the `.keras` format by using the `format` argument.

## Bug Fix
* Fix a bug in `AUTO_VI$vss()` that arguments will be passed incorrectly to `KERAS_WRAPPER$image_to_array()` when a `data.frame` or a `tibble` is provided by the user to predict visual signal strength.
* Fix a bug in `save_plot()` where the `path` argument was not functioning as intended.. 

# autovi 0.4.0

* First CRAN release
