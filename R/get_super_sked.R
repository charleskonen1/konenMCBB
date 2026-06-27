#' Get Bart Torvik super schedule
#'
#' Retrieves the full Torvik super schedule table for a given season.
#'
#' @param year Season year (>= 2008)
#'
#' @return A data.frame containing super schedule data
#' @export
get_super_sked <- function(
    year = as.integer(format(Sys.Date(), "%Y"))
) {

  if (!is.numeric(year) || year < 2008) {
    stop("`year` must be numeric and >= 2008.")
  }

  year_str <- as.character(year)

  csv_url <- paste0(
    "https://barttorvik.com/",
    year_str,
    "_super_sked.csv"
  )

  resp <- tryCatch(
    httr2::req_perform(.torvik_req(csv_url, timeout = 30)),
    error = function(e) stop("Failed to reach barttorvik.com: ", conditionMessage(e))
  )
  httr2::resp_check_status(resp)
  txt <- httr2::resp_body_string(resp)
  data <- utils::read.csv(text = txt, stringsAsFactors = FALSE, check.names = FALSE)

  cols <- c(
    "muid","Date","confmatch","matchup","prediction",
    "ttq","conf","venue",
    "team1","t1oe","t1de","t1py","t1wp","t1propt",
    "team2","t2oe","t2de","t2py","t2wp","t2propt",
    "tpro","t1qual","t2qual","gp","result","tempo",
    "possessions","t1pts","t2pts","winner","loser",
    "t1adjt","t2adjt","t1adjo","t1adjd","t2adjo","t2adjd",
    "gamevalue","mistmach","blowout","t1elite","t2elite",
    "ord_date","t1ppp","t2ppp","gameppp",
    "t1rk","t2rk","t1gs","t2gs","gamestats",
    "overtimes","t1fun","t2fun","results"
  )

  if (ncol(data) != length(cols)) {
    stop(
      "Column mismatch: expected ", length(cols),
      " columns but received ", ncol(data), "."
    )
  }

  colnames(data) <- cols


  splitPred <- stringr::str_split(
    data$prediction,
    ", ",
    simplify = TRUE
  )

  colDF1 <- stringr::str_split(splitPred[,1], "-", simplify = TRUE)
  colDF2 <- stringr::str_split(splitPred[,2], " ", simplify = TRUE)

  data$predicted_score <- colDF2[,1]
  data$pctChanceWin    <- colDF2[,2]
  data$favored_team    <- colDF1[,1]
  data$line            <- colDF1[,2]

  return(data)
}
