# Write simulated sessions as folders for MFOs()

Writes each participant and session of a \[simulate_MFO_sessions()\]
result as a folder \`\<id\>\_\<session\>\` (e.g. \`P01_S1\`) with three
xlsx files, the layout that \`MFOs(from = "folder")\` reads. The
participant ID in the \`MFOs()\` summary is then the folder name. A file
\`sessions.csv\` in \`path\` maps each folder name (\`id\`) to its
\`participant\` and \`session\`; \`MFOs()\` ignores it, as it reads only
folders.

## Usage

``` r
write_MFO_sessions(
  sim,
  path,
  db_basal_name = "basal_df",
  db_MFO_name = "MFO_df",
  db_graded_name = "VO2max_df",
  overwrite = FALSE
)
```

## Arguments

- sim:

  result of \[simulate_MFO_sessions()\].

- path:

  folder where the participant folders are written (created if needed).

- db_basal_name, db_MFO_name, db_graded_name:

  file names without the extension (defaults as the example data).

- overwrite:

  logical; if \`FALSE\` (default), existing participant folders stop the
  function.

## Value

The folders written, invisibly.

## See also

\[simulate_MFO_sessions()\], \[MFOs()\]

## Examples

``` r
sim <- simulate_MFO_sessions(2, 2, population = list(VO2max = c(3000, 0)),
                             seed = 1)
out <- file.path(tempdir(), "sim")
write_MFO_sessions(sim, out)
list.files(out)
#> [1] "P01_S1"       "P01_S2"       "P02_S1"       "P02_S2"       "sessions.csv"
```
