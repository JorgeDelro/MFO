# Read databases for MFO package

Read databases for MFO package

## Usage

``` r
read_MFO_databases(
  from = c("folder", "files"),
  path,
  db_basal_name,
  db_MFO_name,
  db_graded_name,
  col_name_VO2 = "VO2",
  col_name_VCO2 = "VCO2",
  col_name_RER = "RER",
  col_name_HR = "HR",
  remove_rows = NULL
)
```

## Arguments

- from:

  select either from "folder" or "files"

- path:

  path to the databases

- db_basal_name:

  name of the database with the basal metabolic rate test

- db_MFO_name:

  name of the database of MFO test

- db_graded_name:

  name of the database of the graded exercise test

- col_name_VO2:

  name given to the variable VO2 in the databases. Default "VO2"

- col_name_VCO2:

  name given to the variable VCO2 in the databases. Default "VCO2"

- col_name_RER:

  name given to the variable RER in the databases. Default "RER"

- col_name_HR:

  name given to the variable HR in the databases. Default "HR"

- remove_rows:

  An integer (or a vector of integers) representing the position of the
  rows to delete

## Value

Returns 3 databases:

- participant_db_basal: database with basal metabolism.

- participant_db_MFO: database with MFO test.

- participant_db_graded: graded exercise test.
