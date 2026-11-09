# CHANGES IN TSINTERMITTENT VERSION 2.0 (08 NOV 2026)
- new: Empirical quantiles of the demand over the lead time for all methods, using the input probs. When probs is provided, h is understood as the lead time, and the quantiles are calculated from the cumulative forecast errors over h periods from all in-sample origins. The output element lt contains the quantiles, the safety stock, and the cumulative errors with their origins. The calculation is also exported as lt.dist.
- new: New cost functions msce and mace, on the cumulative errors over h periods, for crost, crost.ma, tsb, and sexsm.
- new: Outputs of crost, crost.ma, tsb, sexsm, and imapa are now objects of class tsint (imapa also of class imapa), with print, summary, plot, coef, fitted, and forecast methods. fitted and forecast return ts objects when the input is a ts object; the list elements frc.in and frc.out remain numeric vectors. idclass returns an object of class idclass with print and plot methods. Existing list elements are unchanged; the information used by the methods is appended in a new element, fit.info.
- new: New forecast method (using the forecast generic of the generics package), which gives point forecasts for any horizon without optimising the model again, optionally applying it to new data. lt.dist accepts a fitted model, to calculate quantiles for any lead time and probabilities without refitting.
- new: New input model in crost, crost.ma, tsb, and sexsm to apply a fitted model to new data without optimisation. With init.opt=TRUE, only the initial values are optimised. imapa accepts a fitted model in model.fit.
- new: Plots moved to plot methods, which accept graphical arguments, e.g. main and ylim. New lead time view of the forecasts. outplot also accepts the name of a view, e.g. outplot="leadtime".
- new: imapa: new input cl for parallel processing with an existing cluster. paral=1 still looks for a cluster named cl in the global environment if cl is not provided.
- new: Added a vignette and tests.
- imapa no longer depends on the MAPA package. Requires R >= 4.1.0.
- imapa: the default maximumAL is the larger of the maximum inter-demand interval of the series and its sampling frequency (for ts objects), instead of half the length of the series. Results of imapa without maximumAL differ from earlier versions.
- imapa: points in the model selection plot have a black outline.
- imapa: parallel processing checks that the workers can load the same version of the package, and gives an informative error otherwise, e.g., when the package is loaded with devtools::load_all().
- Documentation and NAMESPACE are now generated with roxygen2 from the R files, with more examples. Added package level help page (?tsintermittent).
- Input checks on type, cost, init, and outplot, with informative errors. type="crost" is accepted as an alias for "croston".
- opt.on is deprecated and will be removed in a future version.
- bugfix: crost, tsb, sexsm: the length of a numeric init was checked incorrectly. crost and tsb now give a clear error if init has fewer than two values.
- bugfix: tsb: failed when a single smoothing parameter was given and init.opt=TRUE. tsb now gives a clear error.
- bugfix: crost.ma: when the grid search had tied minima, the orders of different minima were combined.
- bugfix: data.frc: leftover browser() call and wrong type with method="auto". Series that cannot be classified are now forecast with SES.
- bugfix: data.frc: failed when h=1.
- bugfix: idclass: failed for a single ts series. Series with fewer than two non-zero values are now left out of all classes with a warning.
- bugfix: imapa: returned matrices when a single aggregation level was used, and failed when h=0.

# CHANGES IN TSINTERMITTENT VERSION 1.10 (15 JUL 2022)
- Updated links.
- Updated DESCRIPTION.
- bugfix: Fixed checks on classes.
- bugfix: crost, crost.ma: fixed bug when checking number of parameters to optimise. Bug did not cause issues before.

# CHANGES IN TSINTERMITTENT VERSION 1.9 (10 MAR 2016)
- new: crost, crost.ma: provide vectors of non-zero demand and intervals.
- crost.ma: changed the variable name of demand rate to "cc".

# CHANGES IN TSINTERMITTENT VERSION 1.8 (23 SEP 2015)
- new: Added "auto" method in data.frc. This selects and uses Croston, SBA and SES automatically for each time series, based on PKa classification (idclass function).
- new: Added check on crost.decomp and idclass when input is a data frame.
- new: Added warning on functions that handle single time series when data frames with more than a single column are used as inputs.
- imapa: changed classification to PKa instead of PK with prespecified alpha. Note that data.frc with method="auto" and imapa with maximumAL=1 give the same results, but data.frc is faster. Use imapa when temporal aggregation is desirable.

# CHANGES IN TSINTERMITTENT VERSION 1.7 (17 SEP 2015)
- new: Added function data.frc. This is a wrapper to forecast data frames with a single command.
- new: Added option to remove NA values in all forecasting methods.
- simID now outputs different series as columns in a data frame.

# CHANGES IN TSINTERMITTENT VERSION 1.6 (17 FEB 2015)
- new: Added separate function crost.decomp for Croston decomposition.

# CHANGES IN TSINTERMITTENT VERSION 1.5 (20 NOV 2014)
- bugfix: crost.ma: corrected optimisation error.

# CHANGES IN TSINTERMITTENT VERSION 1.4 (03 OCT 2014)
- new: Added more time series checks for crost and imapa functions.
- imapa: revised default maximumAL to be floor(number of observations/2).
- simID: changed default CV^2 to 0.5.

# CHANGES IN TSINTERMITTENT VERSION 1.3 (04 AUG 2014)
- new: Added option to optimise only initial values without parameters in crost, tsb and sexsm functions.
- new: Added init.opt option for imapa in the same way as it is for crost and tsb functions. This does not affect SES optimisation.
- new: Added option to use pre-fitted imapa models.
- bugfix: Corrected major optimisation bug - issue with opt.on in all crost, tsb and sexsm functions.
- bugfix: imapa: corrected bug when maximumAL was very high.
- bugfix: imapa: corrected bugs in the legend of plots.

# CHANGES IN TSINTERMITTENT VERSION 1.2 (16 JUL 2014)
- new: Introduced imapa function for intermittent demand series.
- Added MAPA package dependency for temporal aggregation functions.
- Added parallel package dependency for imapa parallelisation.

# CHANGES IN TSINTERMITTENT VERSION 1.1 (03 JUL 2014)
- new: Added SES function (sexsm) with control over parameter and initialisation.
- Added URL in description.
- Renamed data and data2 to ts.data1 and ts.data2.
- Forced type input to lowercase.

# CHANGES IN TSINTERMITTENT VERSION 1.0 (23 JUN 2014)
- First release.
