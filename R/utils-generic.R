# Generic csv/xlsx reader of read_cart(device = "generic") (SPEC 16, #22):
# reads the mapped columns as text, parses numbers and Time, and hands the
# records to the shared pipeline in R/utils-cart.R. Only the mapped columns
# are kept, so the metadata lines of the export (where the participant's name
# usually is) never leave this function.

# Contract column -> name in the file; defaults are the contract names
.generic_columns <- function(columns) {
  allowed <- c("Time", "VO2", "VCO2", "RER", "HR", "Load", "VE")
  map <- stats::setNames(allowed, allowed)
  if (!is.null(columns)) {
    if (!is.character(columns) || is.null(names(columns)) || any(names(columns) == "") ||
        anyDuplicated(names(columns)) || anyNA(columns)) {
      stop("columns must be a named character vector, e.g. ",
           "c(Time = \"t\", VO2 = \"VO2 STPD\").", call. = FALSE)
    }
    unknown <- setdiff(names(columns), allowed)
    if (length(unknown) > 0) {
      stop("Unknown name(s) in columns: ", paste(unknown, collapse = ", "),
           ". Use ", paste(allowed, collapse = ", "), ".", call. = FALSE)
    }
    map[names(columns)] <- columns
  }
  map
}


# One table (csv file or xlsx sheet) as text, header in line skip + 1 and,
# with units_row, the units in the next line. Returns the mapped columns
# renamed to the contract and the units of VO2 and VCO2 ("" if none)
.read_generic_table <- function(file, sheet, columns, skip, units_row, decimal) {
  raw <- .read_generic_raw(file, sheet, skip, decimal)
  if (nrow(raw) < 1 + units_row) stop("The file has no data rows.", call. = FALSE)
  header <- trimws(as.character(unlist(raw[1, ])))
  units <- if (units_row) trimws(as.character(unlist(raw[2, ]))) else rep("", length(header))
  units[is.na(units)] <- ""
  body <- raw[-seq_len(1 + units_row), , drop = FALSE]

  missing_cols <- setdiff(columns[c("Time", "VO2", "VCO2")], header)
  if (length(missing_cols) > 0) {
    stop("Column(s) ", paste0("\"", missing_cols, "\"", collapse = ", "),
         " not found", if (!is.null(sheet)) paste0(" in sheet \"", sheet, "\""),
         ". Columns in the file: ", paste0("\"", header[header != ""], "\"", collapse = ", "),
         ". Map them with columns = c(Time = , VO2 = , VCO2 = ).", call. = FALSE)
  }
  present <- columns[columns %in% header]
  out <- list()
  for (k in names(present)) {
    j <- match(present[[k]], header)
    v <- body[[j]]
    out[[k]] <- if (k == "Time") .parse_time(v, decimal) else .parse_number(v, decimal, k)
  }
  out <- as.data.frame(out)
  list(data = out,
       vo2_unit = units[match(columns[["VO2"]], header)],
       vco2_unit = units[match(columns[["VCO2"]], header)])
}


# The whole table as text, without header: csv (or txt/tsv) or an xlsx sheet
.read_generic_raw <- function(file, sheet, skip, decimal, n_max = Inf) {
  ext <- tolower(sub(".*\\.([^.]+)$", "\\1", basename(file)))
  if (ext %in% c("xlsx", "xls")) {
    raw <- readxl::read_excel(file, sheet = if (is.null(sheet)) 1 else sheet, skip = skip,
                              col_names = FALSE, col_types = "text", n_max = n_max,
                              .name_repair = "minimal")
    return(as.data.frame(raw, stringsAsFactors = FALSE))
  }
  if (!ext %in% c("csv", "txt", "tsv")) {
    stop("device = \"generic\" reads csv (or txt/tsv) and xlsx files, not .", ext, ".",
         call. = FALSE)
  }
  if (!is.null(sheet)) stop("sheets are only for xlsx files.", call. = FALSE)
  lines <- readLines(file, warn = FALSE, encoding = "UTF-8",
                     n = if (is.finite(n_max)) skip + n_max else -1L)
  if (skip > 0) lines <- lines[-seq_len(min(skip, length(lines)))]
  if (length(lines) == 0) stop("The file has no data rows.", call. = FALSE)
  sep <- .detect_sep(lines[1], decimal)
  utils::read.table(text = lines, sep = sep, header = FALSE, quote = "\"",
                    colClasses = "character", na.strings = c("", "NA"),
                    fill = TRUE, comment.char = "", strip.white = TRUE,
                    check.names = FALSE)
}


# Column names in the header line of a generic file (for the mapping of the
# app, #24)
.generic_header <- function(file, sheet = NULL, skip = 0, decimal = ".") {
  raw <- .read_generic_raw(file, sheet, skip, decimal, n_max = 1)
  h <- trimws(as.character(unlist(raw[1, ])))
  h[!is.na(h) & h != ""]
}


# Field separator of a csv from its header line: tab, ";" or ","; with a
# decimal comma the separator cannot be ","
.detect_sep <- function(line, decimal) {
  counts <- c("\t" = lengths(regmatches(line, gregexpr("\t", line))),
              ";" = lengths(regmatches(line, gregexpr(";", line))),
              "," = lengths(regmatches(line, gregexpr(",", line))))
  if (decimal == ",") counts[","] <- 0
  if (all(counts == 0)) return(if (decimal == ",") ";" else ",")
  names(counts)[which.max(counts)]
}


# Text -> number with the declared decimal mark. If values fail to parse and
# contain commas, the error proposes decimal = "," (Jorge, 2026-11-19)
.parse_number <- function(v, decimal, name) {
  v <- trimws(as.character(v))
  v[v %in% c("", "-", "NA", "NaN")] <- NA
  # With decimal = "," a dot is not read as a thousands mark: "1.234,5" fails
  x <- if (decimal == ",") gsub(",", ".", v, fixed = TRUE) else v
  if (decimal == ",") x[grepl(".", v, fixed = TRUE)] <- NA
  num <- suppressWarnings(as.numeric(x))
  bad <- !is.na(v) & is.na(num)
  if (any(bad)) {
    hint <- if (decimal == "." && any(grepl("^-?[0-9]+,[0-9]+$", v[bad]))) {
      " The values have decimal commas: use decimal = \",\"."
    } else ""
    stop(sprintf("Column %s has %d value(s) that are not numbers (e.g. \"%s\").%s",
                 name, sum(bad), v[bad][1], hint), call. = FALSE)
  }
  num
}


# Time in seconds from seconds, mm:ss, hh:mm:ss (with decimals) or Excel
# times (fractions of a day, all values <= 1). Elapsed time from the start of
# the recording is expected (Jorge, 2026-11-19)
.parse_time <- function(v, decimal) {
  v <- trimws(as.character(v))
  v[v %in% c("", "NA")] <- NA
  ok <- v[!is.na(v)]
  if (length(ok) == 0) stop("Column Time is empty.", call. = FALSE)
  # date-times written as text (e.g. "1899-12-31 00:01:20"): keep the time
  dt <- "^[0-9]{4}-[0-9]{2}-[0-9]{2}[ T]"
  if (all(grepl(dt, ok))) v <- sub(dt, "", v)
  ok <- v[!is.na(v)]
  if (decimal == ",") v <- gsub(",", ".", v, fixed = TRUE)
  ok <- v[!is.na(v)]
  num <- suppressWarnings(as.numeric(ok))
  if (!anyNA(num)) {
    if (max(num) <= 1) {
      .mfo_message("excel_time", "Time read as Excel times (fractions of a day).")
      # rounded to the millisecond: day fractions carry binary error
      return(round(as.numeric(v) * 86400, 3))
    }
    return(as.numeric(v))
  }
  clock <- "^[0-9]+(:[0-5]?[0-9]){1,2}(\\.[0-9]+)?$"
  if (all(grepl(clock, ok))) {
    out <- vapply(strsplit(v, ":", fixed = TRUE), function(p) {
      if (length(p) == 1 && is.na(p)) return(NA_real_)
      p <- as.numeric(p)
      sum(p * 60^rev(seq_along(p) - 1))
    }, numeric(1))
    return(out)
  }
  bad <- ok[is.na(num) & !grepl(clock, ok)]
  stop(sprintf(paste0("Time not recognised (e.g. \"%s\"). Use seconds (95.5), mm:ss ",
                      "(01:35), hh:mm:ss (00:01:35) or Excel times."), bad[1]),
       call. = FALSE)
}


# Unit of VO2 from a units row: "mL/min", "L/min" or "mL/kg/min"; NA if the
# label is empty or not recognised
.unit_from_label <- function(x) {
  if (is.null(x) || is.na(x) || x == "") return(NA_character_)
  k <- tolower(gsub("[][()[:space:]]", "", x))
  k <- gsub("\u00b7", "/", k)
  switch(k,
         "ml/min" = "mL/min",
         "l/min" = "L/min",
         "ml/kg/min" = "mL/kg/min",
         NA_character_)
}


# VO2 unit: explicit vo2_unit wins; otherwise a recognised units row is a
# declaration of the file, used with a message (Jorge, 2026-11-19)
.generic_unit <- function(vo2_unit, vo2_label, vco2_label) {
  if (vo2_unit != "auto") return(vo2_unit)
  u <- .unit_from_label(vo2_label)
  if (is.na(u)) {
    if (!is.null(vo2_label) && !is.na(vo2_label) && vo2_label != "") {
      .mfo_message("units_row_unknown",
                   paste0("VO2 unit \"", vo2_label, "\" in the units row not recognised; ",
                          "using vo2_unit = \"auto\"."), unit = vo2_label)
    }
    return("auto")
  }
  u2 <- .unit_from_label(vco2_label)
  if (!is.na(u2) && u2 != u) {
    stop("The units row gives VO2 in ", u, " and VCO2 in ", u2,
         "; both must be in the same unit.", call. = FALSE)
  }
  .mfo_message("units_row", paste0("VO2 and VCO2 unit taken from the units row: ", u, "."),
               unit = u)
  u
}
