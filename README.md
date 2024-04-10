
<!-- README.md is generated from README.Rmd. Please edit that file -->

# eyeQuality

<!-- badges: start -->
<!-- badges: end -->

The goal of *eyeQuality* is to provide functions for basic processing of
eye tracking data. The package is designed to support a variety of eye
tracking data sources (but was primarily tested using data exports from
Tobii Studio and Tobii Pro).

## Installation

You can install the eyeQuality from
[GitHub](https://github.com/elab-umn/eyeQuality/) with:

``` r
# install.packages("devtools")
devtools::install_github("elab-umn/eyeQuality")
```

Alternatively, you can install from source by downloading the package
via github. Follow instructions outlined
[here](https://www.dataquest.io/blog/install-package-r/)

NOTE: Coming soon, we hope *eyeQuality* will be available to download
via CRAN. We will update documentation here once available!

## Package Documentation

To access package documentation and a list of all functions run

``` r
help(package = "eyeQuality")
```

## Contributing

1.  Clone github repo
2.  Open the eyeQuality.Rproj file to launch the project in RStudio
3.  Create a new branch (use the button on the “Git” pane in RStudio or
    use git commands in the Terminal)
4.  Write code
5.  Add tests using testthat package (see
    [tests/README](/tests/README.md))
6.  Add documentation using roxygen (see [R package roxygen
    instructions](https://r-pkgs.org/man.html))
7.  Once your changes are ready to be merged, submit a [pull
    request](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/creating-a-pull-request)
    on github

If you find any bugs or have feature requests, [open an issue on
github](https://github.com/elab-umn/eyetrackingELabR/issues). Please be
as detailed as possible in your issue.

## Output Variable Data Dictionary

| Recording Information Variables | Definitions                             |
|:--------------------------------|:----------------------------------------|
| event                           | Stimulus event label                    |
| eventValue                      | Stimulus event value                    |
| recordingDuration_ms            | Total recording duration (ms)           |
| resolutionHeight                | Recording screen resolution height (px) |
| resolutionWidth                 | Recording screen resolution width (px)  |
| eyeTrackerTimestamp             | Internal eye tracker timestamp (unit?)  |
| recordingTimestamp_ms           | Recording timecourse timestamp (ms)     |

| Gaze Position Variables | Definitions                                                                 |
|:------------------------|:----------------------------------------------------------------------------|
| gazeLeftX               | Raw left eye X gaze position in pixels                                      |
| gazeLeftY               | Raw left eye Y gaze position in pixels                                      |
| gazeRightX              | Raw right eye X gaze position in pixels                                     |
| gazeRightY              | Raw right eye Y gaze position in pixels                                     |
| gazeLeftX.int           | Left eye X gaze position in pixels after interpolation                      |
| gazeLeftY.int           | Left eye Y gaze position in pixels after interpolation                      |
| gazeRightX.int          | Right eye X gaze position in pixels after interpolation                     |
| gazeRightY.int          | Right eye Y gaze position in pixels after interpolation                     |
| gazeX.eyeSelect         | Composite X gaze position after interpolation and eye selection             |
| gazeY.eyeSelect         | Composite Y gaze position after interpolation and eye selection             |
| gazeX.smooth            | Composite X gaze position after interpolation, eye selection, and smoothing |
| gazeY.smooth            | Composite Y gaze position after interpolation, eye selection, and smoothing |
| gazeX.va                | X gaze position in visual angles                                            |
| gazeY.va                | Y gaze position in visual angles                                            |
| gazeX.preprocessed_px   | FINAL PREPROCESSED GAZE X POSITION IN PIXEL SPACE                           |
| gazeY.preprocessed_px   | FINAL PREPROCESSED GAZE Y POSITION IN PIXEL SPACE                           |
| gazeX.preprocessed_va   | FINAL PREPROCESSED GAZE X POSITION IN VISUAL ANGLE SPACE                    |
| gazeY.preprocessed_va   | FINAL PREPROCESSED GAZE Y POSITIONN IN VISUAL ANGLE SPACE                   |

| Z Distance Variables      | Definitions                                                                                       |
|:--------------------------|:--------------------------------------------------------------------------------------------------|
| distanceLeftZ             | Raw left eye Z position (distance from screen in mm)                                              |
| distanceRightZ            | Raw right eye Z position (distance from screen in mm)                                             |
| distanceLeftZ.int         | Left eye Z position (distance from screen in mm) after interpolation                              |
| distanceRightZ.int        | Right eye Z position (distance from screen in mm) after interpolation                             |
| distanceZ.eyeSelect       | Composite eye Z position (distance from screen) after interpolation and eye selection             |
| distanceZ.smooth          | Composite eye Z position (distance from screen) after interpolation, eye selection, and smoothing |
| distanceZ.preprocessed_mm | FINAL PREPROCESSED Z DISTANCE POSITION IN MM                                                      |

| Velocity Variables                   | Definitions                                                |
|:-------------------------------------|:-----------------------------------------------------------|
| velocityX_va_ms                      | X gaze velocity in VA/second (lagged by two gazepoints)    |
| velocityY_va_ms                      | Y gaze velocity in VA/second (lagged by two gazepoints)    |
| velocityEuclidean_va_ms              | Euclidean velocity in VA/second (lagged by two gazepoints) |
| velocityX.smooth_va_ms               | X gaze velocity in VA/second after smoothing               |
| velocityY.smooth_va_ms               | Y gaze velocity in VA/second after smoothing               |
| velocityEuclidean.smooth_va_ms       | Euclidean velocity in VA / second after smoothing          |
| velocityX.preprocessed_va_ms         | FINAL PREPROCESSED X GAZE VELOCITY IN VA/SECOND            |
| velocityY.preprocessed_va_ms         | FINAL PREPROCESSED Y GAZE VELOCITY IN VA/SECOND            |
| velocityEuclidean.preprocessed_va_ms | FINAL PREPROCESSED EUCIDEAN VELOCITY IN VA/SECOND          |

| IVT Variables           | Definitions                     |
|:------------------------|:--------------------------------|
| IVT.classification      | FINAL IVT FILTER CLASSIFICATION |
| IVT.fixationIndex       | Index number of fixation        |
| IVT.saccadeIndex        | Index number of saccade         |
| IVT.fixationDuration_ms | Total duration of fixation (ms) |

| Validity Variables       | Definitions                                         |
|:-------------------------|:----------------------------------------------------|
| validLeft                | Input for classifying left eye validity             |
| validRight               | Input for classigying right eye validity            |
| gazeLeft.offscreen       | Classification of left eye offscreen gaze validity  |
| gazeRight.offscreen      | Classification of right eye offscreen gaze validity |
| offscreen.classification | FINAL CLASSIFICATION OF OVERALL OFFSCREEN VALIDITY  |

| Pupil and Blink Variables | Definitions                                                       |
|:--------------------------|:------------------------------------------------------------------|
| pupilLeft                 | Raw left pupil data                                               |
| pupilRight                | Raw right pupil data                                              |
| pupilLeft.int             | Interpolated left pupil data                                      |
| pupilRight.int            | Interpolated right pupil data                                     |
| pupil.eyeSelect           | Pupil data after interpolation and eye selection                  |
| pupil.smooth              | Pupil data after interpolation, eye selection, and smoothing      |
| pupil.preprocessed        | FINAL PREPROCESSED PUPIL DATA                                     |
| pupilLeft.blink           | Flags blinks in left eye (where 1 = blink)                        |
| pupilRight.blink          | Flags blinks in right eye (where 1 = blink)                       |
| bothEyes.blink            | Flags where blinks occur concurrently in both left and right eyes |
| blink.classification      | FINAL BLINK CLASSIFICATION (based on eyeSelection method)         |
