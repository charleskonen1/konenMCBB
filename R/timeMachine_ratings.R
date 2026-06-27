#' Get historical Torvik Time Machine ratings
#'
#' Retrieves archived team ratings for a specific date.
#'
#' @param date Date in YYYYMMDD format (must be >= 20141113)
#'
#' @return A data.frame of team ratings
#' @export
timeMachine_ratings <- function(date) {

  if (missing(date) || length(date) != 1) {
    stop("`date` must be a single value in YYYYMMDD format.")
  }

  if (!grepl("^[0-9]{8}$", as.character(date))) {
    stop("`date` must be in YYYYMMDD format.")
  }

  dateC <- as.integer(date)
  cur_date <- as.integer(format(Sys.Date(), "%Y%m%d"))

  if (dateC < 20141113 || dateC >= cur_date) {
    stop("`date` must be >= 20141113 and before today.")
  }

  dateChar <- as.character(dateC)

  url <- paste0(
    "https://barttorvik.com/timemachine/team_results/",
    dateChar,
    "_team_results.json.gz"
  )

  gz_path <- tempfile(fileext = ".json.gz")
  json_path <- tempfile(fileext = ".json")

  utils::download.file(url, gz_path, mode = "wb", quiet = TRUE)

  R.utils::gunzip(
    gz_path,
    destname = json_path,
    overwrite = TRUE,
    remove = FALSE
  )

  x <- jsonlite::fromJSON(json_path)

  df <- as.data.frame(
    x,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  hdr <- strsplit(
    paste(
      "rank","team","conf","record","adjoe","oe Rank","adjde","de Rank",
      "barthag","rank","proj. W","Proj. L","Pro Con W","Pro Con L",
      "Con Rec.","sos","ncsos","consos","Proj. SOS","Proj. Noncon SOS",
      "Proj. Con SOS","elite SOS","elite noncon SOS","Opp OE","Opp DE",
      "Opp Proj. OE","Opp Proj DE","Con Adj OE","Con Adj DE",
      "Qual O","Qual D","Qual Barthag","Qual Games","FUN",
      "ConPF","ConPA","ConPoss","ConOE","ConDE","ConSOSRemain",
      "Conf Win%","WAB","WAB Rk","Fun Rk","adjt",
      sep = "\t"
    ),
    "\t",
    fixed = TRUE
  )[[1]]

  if (ncol(df) != length(hdr)) {
    stop(
      "Column mismatch: df has ", ncol(df),
      " columns but expected ", length(hdr), "."
    )
  }

  colnames(df) <- make.unique(hdr, sep = "_")

  return(df)
}
