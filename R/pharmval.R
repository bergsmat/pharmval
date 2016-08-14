
#' Dispatch pharmevent method.
#'
#' Dispatches pharmevent method.
#' @param x object of dispatch
#' @param ... arguments to methods
#' @export

as.pharmevent <- function(x = NULL, ...)UseMethod('as.pharmevent')




#' Create an empty pharmevent table.
#'
#' Creates an empty pharmevent table. Essentially, a constructor function.
#' @param x ignored
#' @param ... ignored
#' @export
#' @return pharmevent
#' @examples
#' as.pharmvevent()

as.pharmevent.NULL <- function(x,...){
  x <- data.frame(
    stringsAsFactors = FALSE,
    USUBJID = character(0),
    STUDY = numeric(0),
    CENTER = numeric(0),
    SUBJECT = numeric(0),
    PART = numeric(0),
    INDICATION = numeric(0),
    INDICATION_NAME = character(0),
    TRT = numeric(0),
    TRT_NAME = character(0),
    VISIT = numeric(0),
    VISIT_NAME = character(0),
    BASE = numeric(0),
    SCREEN = numeric(0),
    DATE_DAY = character(0),
    DATE_TIME = character(0),
    NOMINAL_TIME = numeric(0),
    TIME = numeric(0),
    TIME_UNIT = character(0),
    TYPE = numeric(0),
    TYPE_NAME = character(0),
    SUBTYPE = numeric(0),
    VALUE = numeric(0),
    VALUE_TEXT = numeric(0),
    UNIT = character(0),
    NAME = character(0),
    DURATION = numeric(0),
    ULOQ = numeric(0),
    LLOQ = numeric(0),
    ROUTE = character(0),
    INTERVAL = numeric(0),
    NR_DOSES = integer(0)
    # REPEATED = numeric(0),
    # EXCLUSION = character(0),
    # IMPUTATION = character(0)
  )
  class(x) <- union('pharmevent',class(x))
  x
}
#' Create a pharmevent table from a data.frame.
#'
#' Creates a pharmevent table from a data.frame.
#' @param x data.frame
#' @param ... ignored
#' @export
#' @return pharmevent
#' @examples
#' # data(pharmevents)
#' head(as.pharmevent(pharmevents))

# pharmevents <- read.csv('../pmxdat/10928_2014_9370_MOESM1_ESM/Supplementary/Original Data/DataSet_Example.csv',as.is=TRUE, na.strings=c('.','NaN','','NA'))
# names(pharmevents)[[1]] <- 'USUBJID'
# save(pharmevents,file='data/pharmevents.rda')

as.pharmevent.data.frame <- function(x,...){
  p <- as.pharmevent()
  need <- setdiff(names(p), names(x))
  if(length(need))stop('need e.g. column ',need[[1]], ' to create pharmevent object')
  extra <- setdiff(names(x), names(p))
  if(length(extra)){
    message('dropping ',length(extra),' unrecognized columns e.g. ',extra[[1]])
    x <- x[, ! names(x) %in% extra,drop=FALSE]
  }
  x <- x[,names(p)]
  x <- rbind(p,x)
  class(x) <- union('pharmevent',class(x))
  x
}

#' Dispatch pharmval method.
#'
#' Dispatches pharmval method.
#' @param x object of dispatch
#' @param ... arguments to methods
#' @export

as.pharmval <- function(x = NULL, ...)UseMethod('as.pharmval')
#' Create an empty pharmval table.
#'
#' Creates an empty pharmval table.
#' @param x ignored
#' @param ... ignored
#' @export
#' @return pharmval
#' @examples
#' as.pharmval()

as.pharmval.NULL <- function(x,...){
  x <- data.frame(
    stringsAsFactors = FALSE,
    USUBJID = character(0),
    STUDY = numeric(0),
    CENTER = numeric(0),
    SUBJECT = numeric(0),
    PART = numeric(0),
    INDICAT = numeric(0),
    INDICATC = character(0),
    TRT = numeric(0),
    TRTC = character(0),
    VISIT = numeric(0),
    VISITC = character(0),
    BASE = numeric(0),
    SCREEN = numeric(0),
    DATE = character(0),
    CLOCK = character(0),
    NOMTIME = numeric(0),
    TIME = numeric(0),
    TIMEUNIT = character(0),
    TYPE = numeric(0),
    TYPEC = character(0),
    SUBTYPE = numeric(0),
    VALUE = numeric(0),
    VALUEC = numeric(0),
    UNIT = character(0),
    NAME = character(0),
    DURATION = numeric(0),
    ULOQ = numeric(0),
    LLOQ = numeric(0),
    ROUTE = character(0),
    INTERVAL = numeric(0),
    ADDLDOSE = integer(0),
    REPEATED = numeric(0),
    EXCLUDED = character(0),
    IMPUTED = character(0)
  )
  class(x) <- union('pharmval',class(x))
  x
}

#' Convert pharmval to pharmevent.
#'
#' Converts pharmval to pharmevent.
#' @param x pharmval object
#' @param ... ignored
#' @export
#' @import dplyr
#' @importFrom tidyr spread
#' @importFrom tidyr gather
#' @import magrittr
#' @return pharmevent
#' @examples
#' as.pharmevent(as.pharmval())

as.pharmevent.pharmval <- function(x,...){
  x %<>% select(-REPEATED, -EXCLUDED, -IMPUTED)
  x %<>% rename(
    # USUBJID = character(0),
    # STUDY = numeric(0),
    # CENTER = numeric(0),
    # SUBJECT = numeric(0),
    # PART = numeric(0),
    INDICATION = INDICAT,
    INDICATION_NAME = INDICATC,
    # TRT = numeric(0),
    TRT_NAME = TRTC,
    # VISIT = numeric(0),
    VISIT_NAME = VISITC,
    # BASE = numeric(0),
    # SCREEN = numeric(0),
    DATE_DAY = DATE,
    DATE_TIME = CLOCK,
    NOMINAL_TIME = NOMTIME,
    # TIME = numeric(0),
    TIME_UNIT = TIMEUNIT,
    # TYPE = numeric(0),
    TYPE_NAME = TYPEC,
    # SUBTYPE = numeric(0),
    # VALUE = numeric(0),
    VALUE_TEXT = VALUEC,
    # UNIT = character(0),
    NAME = VARIABLE,
    # DURATION = numeric(0),
    # ULOQ = numeric(0),
    # LLOQ = numeric(0),
    # ROUTE = character(0),
    # INTERVAL = numeric(0),
    NR_DOSES = ADDLDOSE
    # REPEATED = numeric(0),
    # EXCLUSION = character(0),
    # IMPUTATION = character(0)
  )
  class(x) <- c('pharmevent','data.frame')
  x
}

#' Convert pharmevent to pharmval.
#'
#' Converts pharmevent to pharmval.
#' @param x pharmevent object
#' @param ... ignored
#' @export
#' @return pharmval
#' @examples
#' head(as.pharmval(as.pharmevent(pharmevents)))

as.pharmval.pharmevent <- function(x,...){
  x %<>% rename(
    # USUBJID = character(0),
    # STUDY = numeric(0),
    # CENTER = numeric(0),
    # SUBJECT = numeric(0),
    # PART = numeric(0),
    INDICAT = INDICATION,
    INDICATC = INDICATION_NAME,
    # TRT = numeric(0),
    TRTC = TRT_NAME,
    # VISIT = numeric(0),
    VISITC = VISIT_NAME,
    # BASE = numeric(0),
    # SCREEN = numeric(0),
    DATE = DATE_DAY,
    CLOCK = DATE_TIME,
    NOMTIME = NOMINAL_TIME,
    # TIME = numeric(0),
    TIMEUNIT = TIME_UNIT,
    # TYPE = numeric(0),
    TYPEC = TYPE_NAME,
    # SUBTYPE = numeric(0),
    # VALUE = numeric(0),
    VALUEC = VALUE_TEXT,
    # UNIT = character(0),
    VARIABLE = NAME,
    # DURATION = numeric(0),
    # ULOQ = numeric(0),
    # LLOQ = numeric(0),
    # ROUTE = character(0),
    # INTERVAL = numeric(0),
    ADDLDOSE = NR_DOSES
    # REPEATED = numeric(0),
    # EXCLUSION = character(0),
    # IMPUTATION = character(0)
  )
  x$REPEATED = rep(NA, length=nrow(x)) %>% as.numeric
  x$EXCLUDED = rep(NA, length=nrow(x)) %>% as.character
  x$IMPUTED =  rep(NA, length=nrow(x)) %>% as.character
  class(x) <- c('pharmval','data.frame')
  x
}

