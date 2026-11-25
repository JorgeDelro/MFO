# Run the MFO Shiny app

Opens the app of the package in the browser: load the data of one
participant, describe the protocol, fit the curve and download the
results (SPEC 10). The app only calls the functions of the package, so
its results are those of \[read_cart()\], \[MFO()\], \[MFO_kinetics()\],
\[MFO_zones()\] and \[MFO_reliability()\]. The data stay on your
computer: the app runs locally.

## Usage

``` r
run_app(...)
```

## Arguments

- ...:

  further arguments to \[shiny::runApp()\], e.g. \`port\` or
  \`launch.browser\`.

## Value

Called for its side effect (runs the app); returns what
\[shiny::runApp()\] returns.

## Details

The app needs the packages shiny and bslib (in Suggests).

## Examples

``` r
if (interactive()) run_app()
```
