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
#' as.pharmevent()

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
    VARIABLE = character(0),
    USUBJID = character(0),
    TIME = numeric(0),
    REPEATED = numeric(0),
    TYPE = numeric(0),
    TYPEC = character(0),
    SUBTYPE = numeric(0),
    SUBTYPEC = character(0),
    VALUE = numeric(0),
    VALUEC = numeric(0),
    UNIT = character(0),
    DATE = character(0),
    CLOCK = character(0),
    NOMTIME = numeric(0),
    TIMEUNIT = character(0),
    EXCLUDED = character(0),
    IMPUTED = character(0),
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
    ULOQ = numeric(0),
    LLOQ = numeric(0),
    ROUTE = character(0),
    DURATION = numeric(0),
    INTERVAL = numeric(0),
    ADDLDOSE = integer(0)
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
  x %<>% select(-REPEATED, -EXCLUDED, -IMPUTED,-SUBTYPEC)
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
  x <- x[,names(as.pharmevent())]
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
  x$SUBTYPEC =  rep(NA, length=nrow(x)) %>% as.character
  x <- x[,names(as.pharmval())]
  class(x) <- c('pharmval','data.frame')
  x
}

as.declaration <- function(x,...)UseMethod('as.declaration')
as.declaration.pharmval <- function(x, item = 'VARIABLE'){
  y <- data.frame(
    stringsAsFactors = FALSE,
    VARIABLE = 'META',
    TYPEC = x$TYPEC,
    VALUEC = item
  )
  y %<>% distinct
  y %<>% as.pharmval
  y
}
declarations <- function(x,...)UseMethod('declarations')
declarations.pharmval <- function(x,...){
  x %<>% filter(VARIABLE == 'META') %>% select(ITEM = VALUEC, PROPERTY = TYPEC)
  class(x) <- union('declarations',class(x))
  x
}
meta <- function(x,...)UseMethod('meta')
meta.pharmval <- function(x,...){
  d <- declarations(x)
  x %<>% filter(VARIABLE != 'META') # clear declarations
  t <- x %>% semi_join(d %>% select(TYPEC = PROPERTY), by = 'TYPEC') # pull metatable records
  t %<>% select(INSTANCE = VARIABLE,PROPERTY = TYPEC, ENTRY = VALUEC)
  t %<>% left_join(d ,by='PROPERTY')
  t %<>% select(ITEM,INSTANCE,PROPERTY,ENTRY)
  class(t) <- c('meta','data.frame')
  t
}

nonmeta <- function(x,...)UseMethod('nonmeta')
nonmeta.pharmval <- function(x,...){
  class <- class(x)
  d <- declarations(x)
  x %<>% filter(VARIABLE != 'META') # clear declarations
  x %<>% anti_join(d %>% select(TYPEC = PROPERTY), by = 'TYPEC') # clear meta
  class(x) <- class
  class <- union('nonmeta',class)
  x
}
is.sas5 <- function(x,...)UseMethod('is.sas5')
is.sas5.character <- function(x,...){
  if(any(is.na(x)))return(FALSE)
  if(max(nchar(x)) > 8)return(FALSE)
  if(any(x %contains% '[^A-Z0-9_]'))return(FALSE)
  return(TRUE)
}
flat <- function(x,...)UseMethod('flat')
flat.pharmval <- function(x,...){
  m <- x %>% meta
  x %<>% nonmeta
  m %<>% split(m$ITEM)
  for(nm in names(m)) x %<>% weld(m[[nm]])
  x %<>% as.data.frame(stringsAsFactors = FALSE)
  x
}

weld <- function(x,...)UseMethod('weld')
weld.pharmval <- function(x, table){
  table %<>%
    mutate(row_number = row_number()) %>%
    # to avoid intransitivity when two items have same property
    spread(ITEM,INSTANCE) %>%
    select(-row_number)
  table %<>% spread(PROPERTY,ENTRY)
  x %<>% left_join(table, by = intersect(names(table),names(x)))
  x
}
as.superset <- function(x,...)UseMethod('as.superset')
as.superset.character <- function(x,project = '../model',...){ # x is run name
  filename <- paste0(x,'.csv')
  filedir <- file.path(project,x)
  filepath <- file.path(filedir,filename)
  if(!file.exists(filepath)) filepath <- sub('\\.csv$','.sup',filepath)
  stopifnot(file.exists(filepath))
  y <- filepath %>% read.csv(as.is=T,na='.')
  names(y)[names(y) == x] <- 'VISIBLE'
  as.superset(y)
}
as.superset.data.frame <- function(x,...){
  class(x) <- union('superset',class(x))
  x
}

as.pharmval.character <- function(
  x, # a model name
  project='../model',
  dir = file.path(project, x),
  file = paste0(x,'.csv'),
  spec = paste0(x,'.spec'),
  pvl = paste0(x,'.pvl'),
  filepath = file.path(dir, file),
  specpath = file.path(dir, spec),
  pvlpath = file.path(dir, pvl),
  use.existing = FALSE, # vs. derive from superset
  ...
){
  stopifnot(length(x)==1)
  if(use.existing){
    if(!file.exists(pvlpath))stop('use.existing is TRUE but could not find ',pvlpath)
    y <- read.csv(na='.',as.is=TRUE,pvlpath)
    y %<>% as.pharmval
    return(y)
  }
  y <-  as.superset.character(x,model=model,...)
  z <-  read.spec(specpath)
  as.pharmval(y,spec=z,...) # i.e. as.pharmval.superset
}

as.pharmval.data.frame <- function(x,calculate = list(), attribute = list(), ...){
  # calculate should ensure (as appropriate)
  # * VARIABLE
  # * USUBJID
  # * TIME
  # * REPEATED
  # * VALUE(C)
  # * TYPE(C)
  # * SUBTYPE(C)
  # calculate has form ITEM ~ expression
  # attribute has form ITEM ~ PROPERTY e.g. VARIABLE ~ LABEL
  # * where LABEL is unique within VARIABLE (but may be reused across VARIABLE)

  declaration <- function(item, property){
    stopifnot(length(item) == length(property))
    data.frame(stringsAsFactors=FALSE,ITEM = item, PROPERTY = property)
  }

  d <- declarations(as.pharmval())
  m <- meta(as.pharmval())


  # calculate
  if(length(calculate))
    for(i in 1:length(calculate)){
      nugget <- as.list(calculate[[i]])
      target <- as.character(nugget[[2]])
      bucket <- eval(nugget[[3]],envir=x)
      x[[target]] <- bucket
    }

  # attribute
  if(length(attribute))
    for(i in 1:length(attribute)){
      nugget <- as.list(attribute[[i]])
      item <- as.character(nugget[[2]])
      property <- as.character(nugget[[3]])
      stopifnot(item %in% names(x))
      stopifnot(property %in% names(x))
      m2 <- unique(x[,c(item,property)])
      names(m2) <- c('INSTANCE','ENTRY')
      m2$ITEM <- item
      m2$PROPERTY <- property
      m2 %<>% select(ITEM,INSTANCE,PROPERTY,ENTRY)
      test <- with(m2, split(ENTRY,INSTANCE))
      matches <- lapply(test, length)
      stopifnot(all(matches == 1))
      d2 <- declaration(item,property)
      d <- rbind(d,d2)
      m <- rbind(m,m2)
    }

  y <- merge.data.frame(as.pharmval(),x,all = TRUE)
  if(nrow(d)) y <- merge.data.frame(
    all=TRUE,
    y,
    d %>%       # reverse declarations.pharmval
      mutate(VARIABLE = 'META') %>%
      rename(VALUEC = ITEM, TYPEC = PROPERTY)
  )
  if(nrow(m)) y <- merge.data.frame(
    all=TRUE,
    y,
    m %>%      # reverse meta.pharmval
      select(
        VARIABLE = INSTANCE, TYPEC = PROPERTY, VALUEC = ENTRY  # ITEM preserved in declarations
      )
  )

  y <- y[,names(as.pharmval()),drop=FALSE]
  class(y) <- union('pharmval',class(x))
  y
}

decode <- function(x,spec){
  col <- as.character(substitute(x))
  x[x==0] <- NA
  map(x,from=codes(spec,col),to=decodes(spec,col))
}

bundle <- function(...){
  #browser()
  x <- list(...)
  ans <- rep(NA,length(x[[1]]))
  for(i in 1:length(x)){
    new <- x[[i]]
    i <- is.na(ans) & !is.na(new)
    j <- !is.na(ans) & !is.na(new)
    ans[i] <- new[i]
    ans[j] <- paste(sep='/',ans[j],new[j])
  }
  ans
}

first_non_na <- function(x){
  x <- x[!is.na(x)]
  if(!length(x))return(NA)
  x[[1]]
}

as.number <- function(x,...)suppressWarnings(as.numeric(x))

recalculateTime <- function(x,...){
  x$datetime <- as.mDateTime(as.mDate(as.character(x$DATE)),as.mTime(as.character(x$CLOCK)))
  x %<>% group_by(USUBJID) %>% mutate(reftime = if_else(SUBTYPE == 1 & is.na(EXCLUDED),datetime,NA_real_))
  x %<>% group_by(USUBJID) %>% mutate(reftime = first_non_na(reftime))
  x %<>% group_by(USUBJID) %>% mutate(alttime = first_non_na(datetime))
  x %<>% group_by(USUBJID) %>% mutate(reftime = if_else(is.na(reftime),alttime,reftime))
  x$time <- (x$datetime - x$reftime) %>% as.hour %>% as.number
  x$TIME <- x$time
  x$time <- NULL
  x$reftime <- NULL
  x$datetime <- NULL
  x$alttime <- NULL
  x %<>% data.frame(stringsAsFactors=FALSE)
  #x %>% group_by(USUBJID, TIME, EVID, CMT, REPEATED) %>% status
  x
}

as.pharmval.superset <- function(
  x,
  spec,
  preprocess=function(x,...)x, # intervention
  postprocess=function(x,...)x, # intervention
  calculate = list(), # columns that can be created from other columns (list of functions)
  recalculateTime = TRUE,
  ...
){
  original_names <- names(x)
  cols <- length(original_names)
  visible <- match('VISIBLE',original_names)
  post_visible <- character(0)
  if(cols > visible) post_visible <- original_names[(visible + 1):cols]

  # preprocess
  x %<>% preprocess

  # calculate
  if(length(calculate))
    for(i in 1:length(calculate)){
      nugget <- as.list(calculate[[i]])
      target <- as.character(nugget[[2]])
      bucket <- eval(nugget[[3]],envir=x)
      x[[target]] <- bucket
    }

  # Need to rename spec$column wherever renaming data
  if(length(calculate))
    for(i in 1:length(calculate)){
      nugget <- as.list(calculate[[i]])
      target <- as.character(nugget[[2]])
      bucket <- as.character(nugget[[3]])
      if(class(nugget[[3]]) == 'name')
        spec$column[spec$column == bucket] <- target
    }
  # Need to rename post_visible wherever renaming data
  if(length(calculate))
    for(i in 1:length(calculate)){
      nugget <- as.list(calculate[[i]])
      target <- as.character(nugget[[2]])
      bucket <- as.character(nugget[[3]])
      if(class(nugget[[3]]) == 'name')
        post_visible[post_visible == bucket] <- target
    }

  # recalculate TIME from DATE and CLOCK
  #x %>% group_by(USUBJID, DATE, TIME, REPEATED) %>% status

  if(recalculateTime) x %<>% recalculateTime

  # stack nonconstitutive
  constitutive <- c(
    'USUBJID',  'STUDY',    'CENTER',   'SUBJECT',  'PART',
    'INDICAT',  'INDICATC', 'TRT', 'TRTC',     'VISIT',    'VISITC' ,
    'BASE',     'SCREEN',   'DATE',     'CLOCK',    'NOMTIME' ,
    'TIME',     'TIMEUNIT', 'DURATION', 'ROUTE',    'INTERVAL',
    'ADDLDOSE', 'REPEATED', 'EXCLUDED', 'IMPUTED','ULOQ', 'LLOQ',
    'TYPE', 'SUBTYPE','SUBTYPEC', 'DOSEREC'
  )

  # test one-to-one SUBTYPE, SUBTYPEC
  levs <- function(...)length(unique(levels(paste(...))))
  constant <- function(x,within) levs(x,within) == levs(x)
  one2one <- function(x,y)constant(x,y) && constant(y,x)
  stopifnot(one2one(x$SUBTYPE,x$SUBTYPEC))


  # test user has supplied all constitutives except TYPE
  stopifnot(all(setdiff(constitutive,'TYPE') %in% names(x)))

  # x %<>% gather_(key_col='VARIABLE',value_col='VALUE',gather_cols=nonconstitutive)

  # At this point, we wish to stack non-constitutive values intelligently.
  # The 'calculate' routines provided by the user should have supplied any missing consitutive items.
  # As well, AMT and DV should be present.  These are indexed by SUBTYPE(C).
  # During stacking, the following pertain to DV where DOSEREC=0: LLOQ, ULOQ, EXCLUDED, IMPUTED.  (Ignore DURATION, ROUTE, INTERVAL, ADDLDOSE).
  # During stacking, the following pertain to AMT where DOSEREC=1: EXCLUDED, IMPUTED, DURATION, ROUTE, INTERVAL, ADDLDOSE. (Ignore LLOQ, ULOQ).
  # During stacking, all non-constitutives besides AMT and DV are stacked, ignoring LLOQ, ULOQ, EXCLUDED, IMPUTED, DURATION, ROUTE, INTERVAL, ADDLDOSE.
  #  -Those non-constitutives that, ignoring NA, are constant within USUBJID are stacked only in the context of USUBJID and those remaining constitutives that are constant within USUBJID (dropping NA, dropping repeats).
  #  -Those non-constitutives that, ignoring NA, vary with time, are stacked in the context of all remaining constitutives (dropping NA, dropping repeats).
  # After stacking, AMT stack, DV, stack, and ancillary stack are concatenated (merged)
  # After concatenation, units, labels, and decodes are added.

  # Stack AMT, per above
  amtcols <- intersect(constitutive, names(x))
  amtcols <- c(amtcols, 'AMT')
  amtcols <- setdiff(amtcols, c('DOSEREC','LLOQ','ULOQ'))
  amt <- x %>% filter(DOSEREC == 1) %>% select_(.dots=amtcols)
  amt %<>% rename(VALUE = AMT)
  amt %<>% mutate(VARIABLE = 'AMT')


  # Stack DV, per above
  dvcols <- intersect(constitutive,names(x))
  dvcols <- c(dvcols, 'DV')
  dvcols <- setdiff(dvcols, c('DURATION','ROUTE','INTERVAL','ADDLDOSE'))
  dv <- x %>% filter(DOSEREC == 0) %>% select_(.dots=dvcols)
  dv %<>% rename(VALUE = DV)
  dv %<>% mutate(VARIABLE = 'DV')

  # Stack nons, per above
  nonconstitutive <- setdiff(names(x),constitutive)
  nonconstitutive <- setdiff(nonconstitutive, c('AMT','DV'))
  nonconstitutive <- setdiff(nonconstitutive, c('DOSEREC','LLOQ','ULOQ'))
  nonconstitutive <- setdiff(nonconstitutive, c('DURATION','ROUTE','INTERVAL','ADDLDOSE'))
  nonconstitutive <- intersect(nonconstitutive, names(x))

  # Which constitutive items are constant by subject?
  descriptors <- intersect(constitutive, names(x))
  descriptors <- setdiff(descriptors, c('AMT','DV','LLOQ','ULOQ', 'DURATION','ROUTE','DOSEREC','INTERVAL','ADDLDOSE','EXCLUDED','IMPUTED'))
  subject_level_descriptors <- x %>%
    select_(.dots=descriptors) %>%
    gather_(key_col = 'VARS', value_col = 'VALS', gather_cols=setdiff(descriptors,'USUBJID')) %>%
    group_by(VARS,USUBJID) %>%
    summarise(subjConstant=length(unique(VALS)) == 1) %>% # NA is acceptable, but must be constant
    summarise(dataConstant = all(subjConstant)) %>%
    filter(dataConstant) %$% VARS
  subject_level_descriptors <- c('USUBJID',subject_level_descriptors)
  subject_level_descriptors <- intersect(names(as.pharmval()),subject_level_descriptors)

  # Which nonconstitutive items are constant by subject?
  subject_level_endpoints <- x %>%
    select_(.dots = c('USUBJID',nonconstitutive)) %>%
    gather_(key_col = 'VARS', value_col = 'VALS', gather_cols = nonconstitutive) %>%
    group_by(VARS, USUBJID) %>%
    summarise(subjConstant=length(unique(VALS[!is.na(VALS)])) <= 1) %>% # all NA or 1 non-NA value
    summarise(dataConstant = all(subjConstant)) %>%
    filter(dataConstant) %$% VARS

  # Which nonconstitutive items are variable by subject
  time_variant_endpoints <- setdiff(nonconstitutive, subject_level_endpoints)

  # Stack subject-level endpoints in the context of subject-level descriptors, dropping NA and removing duplicates.
  subj <- x %>%
    select_(.dots=c(subject_level_descriptors,subject_level_endpoints)) %>%
    gather_(
      key_col = 'VARIABLE',
      value_col = 'VALUE',
      gather_cols = subject_level_endpoints,
      na.rm = TRUE
    )

  subj %<>% ungroup %>% distinct

  # Stack time-varying endpoits in the context of all descriptors, dropping NA and removing duplicates.
  # Experimentally, also drop records with NA TIME.
  covs <- x %>%
    filter(!is.na(TIME)) %>%
    select_(.dots=c(descriptors,time_variant_endpoints)) %>%
    gather_(
      key_col = 'VARIABLE',
      value_col = 'VALUE',
      gather_cols = time_variant_endpoints,
      na.rm = TRUE
    )

  # If time-varying, but constant across subtypec within time, knock out subtypec.
  not_subtyped <- covs %>%
    group_by(VARIABLE, USUBJID, TIME) %>%
    summarize(VALS = length(unique(VALUE))) %>%
    summarize(constant_subject = all(VALS == 1)) %>%
    summarize(constant_variable = all(constant_subject)) %>%
    filter(constant_variable) %$% VARIABLE
  covs %<>% mutate(SUBTYPEC = if_else(VARIABLE %in% not_subtyped,NA_character_,SUBTYPEC))
  covs %<>% mutate(SUBTYPE  = if_else(VARIABLE %in% not_subtyped,NA_real_,SUBTYPE))
  covs %<>% ungroup %>% distinct

  amt  %<>% as.pharmval
  dv   %<>% as.pharmval
  subj %<>% as.pharmval
  covs %<>% as.pharmval

  x <- rbind(amt, dv, subj, covs)


  # supply decodes from spec
  # supply VALUEC from spec decodes

  valuec <- data.frame(
    stringsAsFactors=FALSE,
    VARIABLE = rep(spec$column, times=sapply(codes(spec),length)),
    VALUE = spec %>% codes %>% unlist %>% as.character %>% as.number,
    VALUEC = spec %>% decodes %>% unlist
  )
  valuec %<>% filter(!is.na(VALUE))
  x %<>% select(-VALUEC) # na placeholder
  x %<>% left_join(valuec, by = c('VARIABLE','VALUE'))

  # any VALUE not facultatively numeric must be ported to VALUEC,
  # presumably not in conflict with a level
  x$valuec <- if_else(is.na(as.number(x$VALUE)),as.character(x$VALUE),NA_character_)

  # enforce value numeric
  x$VALUE <- as.number(as.character(x$VALUE))

  # coalesce VALUEC
  x$VALUEC <- ifelse(is.na(x$VALUEC),x$valuec,x$VALUEC)
  x$valuec <- NULL


  # supply units from spec
  spec$UNIT <- guidetext(spec)
  meta <- spec  %>% select(VARIABLE=column, LABEL=label,UNIT)
  x %<>% select(-UNIT) # placeholder
  x %<>% left_join(meta %>% filter(!is.na(UNIT)) %>% select(VARIABLE,UNIT), by = 'VARIABLE')

  # supply labels from spec
  # pool(x$VARIABLE, meta$VARIABLE)

  labels <- data.frame(
    stringsAsFactors = F,
    VARIABLE = intersect(x$VARIABLE, meta$VARIABLE)
  )
  labels$LABEL <- meta$LABEL[match(labels$VARIABLE,meta$VARIABLE)]
  labels %<>% rename(VALUEC = LABEL)
  labels %<>% mutate(TYPEC = 'LABEL')
  labels %<>% as.pharmval
  d <- as.declaration(labels)


  # subtype, subtypec supplied externally to signal stacking, e.g. of DV, PRED
  # VARIABLE and LABEL distinguished by subtypec should be blended with subtypec, and VARIABLE promoted to TYPEC

  # The VALUE for some VARIABLE may not differ by SUBTYPEC, in which case
  # SUBTYPEC should be revoked, to be reassigned.
  # For those VARIABLE, such as DV, that DO differ by SUBTYPE(C),
  #  - TYPEC should be assigned VARIABLE
  #  - VARIABLE should be combined with SUBTYPE(C) in a SAS compliant manner.
  #  - matching VARIABLE in labels should be transformed correspondingly.

  # Where is VALUE unique by SUBTYPEC?
  # x %>% group_by(VARIABLE, USUBJID, TIME, REPEATED) %>% dup %>% head %>% data.frame
  # x %>% group_by(VARIABLE, USUBJID, TIME, REPEATED, SUBTYPEC) %>% dup %>% head %>% data.frame
  # x %>% group_by(VARIABLE, USUBJID, TIME, REPEATED, SUBTYPEC) %>% status
  # x %>% itemize(VARIABLE, TYPE, TYPEC, SUBTYPE, SUBTYPEC) %>% data.frame
  #
  # Ensure population of SUBTYPE, SUBTYPEC, TYPE, TYPEC

  x %<>% mutate(SUBTYPE = if_else(SUBTYPE %>% is.na,1,SUBTYPE))
  x %<>% mutate(SUBTYPEC = if_else(SUBTYPEC %>% is.na,'subject',SUBTYPEC))
  x %<>% mutate(TYPEC = if_else(SUBTYPEC == 'subject',NA_character_,VARIABLE))
  x %<>% mutate(VARIABLE = if_else(SUBTYPEC == 'subject',VARIABLE,paste0(VARIABLE,'_',SUBTYPE)))
  too_long <- unique(x$VARIABLE)
  too_long <- too_long[nchar(too_long) > 8]
  if(length(too_long)) warning('longer than 8 characters: ',paste(too_long,collapse=', '))
  x$TYPEC[is.na(x$TYPEC) & x$VARIABLE %contains% '^ET[A]?[0-9]+$'] <- 'IIV'
  x$TYPEC[is.na(x$TYPEC) & x$VARIABLE %in% post_visible] <- 'PARAM'
  x$TYPEC[is.na(x$TYPEC) & x$VARIABLE %in% not_subtyped] <- 'TIMEVAR'
  x$TYPEC[is.na(x$TYPEC)] <- 'DEMOG'

  x$TYPE <- recode(
    x$TYPEC,
    AMT = 0,
    DV = 1,
    DEMOG = 2,
    PARAM = 3,
    IIV = 4,
    .default = NA_real_
  )

  x$TYPE[is.na(x$TYPE)] <- as.number(factor(x$TYPEC[is.na(x$TYPE)])) + max(x$TYPE,na.rm=T)

  x %<>% group_by(TYPEC) %>% mutate(SUBTYPE = if_else(SUBTYPEC == 'subject',as.number(factor(VARIABLE)),SUBTYPE))

  x %<>% ungroup
  x %<>% mutate(SUBTYPEC = if_else(SUBTYPEC == 'subject',VARIABLE,SUBTYPEC))


  # fix labels for AMT_1 etc
  subtype <- x %>% filter(VARIABLE != SUBTYPEC) %>% select(VARIABLE=TYPEC,NEW=VARIABLE,PREFIX = SUBTYPEC) %>% ungroup %>% distinct

  labels %<>% left_join(subtype, by = 'VARIABLE')
  labels %<>% mutate(VARIABLE = if_else(is.na(NEW),VARIABLE,NEW))
  labels %<>% mutate(VALUEC = if_else(is.na(PREFIX),VALUEC,paste(PREFIX,VALUEC)))
  labels %<>% select(-NEW,-PREFIX)

  # maybe trap the case where something varies by time, but not really by SUBTYPEC within time.  Can recover more covs.

  # why does CL pick up a subtyping? Probably because it is time-varying, though not truly subtype-varying.

  x <- rbind(d,labels,x)

  # If REPEATED is defined at all for a VARIABLE, there must be no NA REPEATED (e.g. for other subjects) at least where TIME is defined
  repeated <- x %>% filter(!is.na(REPEATED)) %$% VARIABLE %>% unique
  x %<>% mutate( REPEATED = if_else(
    is.na(REPEATED) & VARIABLE %in% repeated & !is.na(TIME),
    0,
    REPEATED
  ))

  x %<>% ungroup %>% distinct

  # classify
  x %<>% as.pharmval.data.frame

  # postprocess
  x %>% postprocess
  x
}

is.valid <- function(x,...)UseMethod('is.valid')

is.valid.pharmval <- function(x,...){
  valid <- TRUE
  p <- as.pharmval()

  # 1. The key items are VARIABLE, USUBJID, TIME, REPEATED, nested in that order.
  if(length(names(x)) != length(names(p))){
    valid <- FALSE
    warning('incorrect number of columns')
    need <- setdiff(names(p),names(x))
    extra <- setdiff(names(x),names(p))
    if(length(need))warning('need ', paste(need, collapse=', '))
    if(length(extra))warning('drop ', paste(extra, collapse=', '))
  }else{
    if(!identical(names(x),names(p))){
      valid <- FALSE
      warning('check column order')
    }
  }

  # 2. Key values may be missing, except for VARIABLE.
  if(any(is.na(x$VARIABLE))){
    valid <- FALSE
    warning('missing values in VARIABLE')
  }

  # 3. If a key value is missing, all nested key values should be missing.
  k <- x %>% itemize(
    USUBJID = is.na(USUBJID),
    TIME = is.na(TIME),
    REPEATED = is.na(REPEATED)
  )
  if(!all(k$REPEATED[k$TIME])){
    valid <- FALSE
    warning('REPEATED is defined where TIME is not')
  }
  if(!all(k$TIME[k$USUBJID])){
    valid <- FALSE
    warning('TIME is defined where USUBJID is not')
  }

  # 4. Key missingness should be consistent within value of VARIABLE (where not excluded)
  d <- x %>%
    nonmeta %>%
    filter(is.na(EXCLUDED)) %>%
    itemize(
      VARIABLE,
      USUBJID = is.na(USUBJID),
      TIME = is.na(TIME),
      REPEATED = is.na(REPEATED)
    ) %$% VARIABLE
  d <- unique(d[duplicated(d)])
  if(length(d)){
    valid <- FALSE
    warning('key missingness varies for some VARIABLE, e.g. ', d[[1]])
  }

  # 5. Combinations of key values should be unique.
  u <- x %>% nonmeta %>% group_by(VARIABLE,USUBJID,TIME,REPEATED) %>% dup
  if(nrow(u)){
    valid <- FALSE
    warning('combinations of VARIABLE, USUBJID, TIME, and REPEATED are not all unique, e.g. ',u$VARIABLE[[1]],', ',u$USUBJID[[1]],', ',u$TIME[[1]],', ',u$REPEATED[[1]])
  }

  # 6. TYPEC and SUBTYPEC may not be missing.
  m <- x %>% nonmeta
  if(any(is.na(m$TYPEC)) ||
     any(is.na(m$TYPE)) ||
     any(is.na(m$SUBTYPEC)) ||
     any(is.na(m$SUBTYPE))
  ){
    valid <- FALSE
    warning('missing values in TYPEC, TYPE, SUBTYPEC, and/or SUBTYPE')
  }

  # 7. TYPEC and SUBTYPEC must be consistent within value of VARIABLE, and vice versa.
  c <- x %>%
    nonmeta %>%
    itemize(VARIABLE, TYPEC, SUBTYPEC) %$% VARIABLE
  c <- unique(c[duplicated(c)])
  if(length(c)){
    valid <- FALSE
    warning('TYPEC, SUBTYPEC not consistent within VARIABLE')
  }

  ## NEED TO TEST VICE VERSA

  # 8. TYPE is numeric, and has a one-to-one correspondence with TYPEC. SUBTYPE one-to-one with SUBTYPEC within TYPE(C).
  #oto <- with(m, one2one(TYPE,TYPEC) && constant(TYPEC,TYPE))
  if(!one2one(m$TYPE,m$TYPEC)){
    valid <- FALSE
    warning('TYPE and TYPEC not one-to-one')
  }

  oto <- with(
    m,
    constant(SUBTYPE,within=list(TYPE,SUBTYPEC)) &&
      constant(SUBTYPEC,within=list(TYPE,SUBTYPE))
  )
  if(!oto){
    valid <- FALSE
    warning('SUBTYPE and SUBTYPEC not one-to-one (within TYPE)')
  }

  # 9. VALUE and VALUEC may be missing. If VALUEC is defined, VALUE should also be defined.
  if(any(is.na(x$VALUE[!is.na(x$VALUE)])))warning('VALUE missing where VALUEC defined')

  # 10. Values of TYPEC and VARIABLE should conform to SAS variable conventions.
  if(!all(is.sas5(x$TYPEC))){
    valid <- FALSE
    warning('not all TYPEC are 8 char uppercase alphanumeric (or underscore)')
  }
  if(!all(is.sas5(x$VARIABLE))){
    valid <- FALSE
    eg <- x %>% filter(!is.sas5(VARIABLE)) %$% VARIABLE %>% `[[`(1)
    warning('not all VARIABLE are 8 char uppercase alphanumeric (or underscore) e.g. ',eg)
  }

  # 11. 'META' is a reserved value of VARIABLE.

  # 12. If VARIABLE is 'META', VALUEC is a pharmval item name, such as VARIABLE.

  # 13. Where VARIABLE is 'META', the effective key is TYPEC. This record is a declaration. All other records with the same value of TYPEC constitute a metadata table.
  m <- x %>% filter(VARIABLE == 'META')
  if(nrow(m))if(any(duplicated(m$TYPEC))){
    valid <- FALSE
    warning('repeated TYPEC where VARIABLE is META')
  }

  # 14. For records of a metadata table, VARIABLE identifies a (possible) value ... the target ... from the column indicated in the declaration; TYPEC identifies an attribute of the target; VALUE and (optionally) VALUEC give the value of the attribute of the target.

  m <- x %>% meta
  if(nrow(m)){
    if(any(is.na(m$ENTRY))){
      warning('metatable record with no entry (VALUEC)')
    }
  }

  # 15. A valid pharmval object may have zero records, or only metadata records, or only data records. Declarations may be present without corresponding metadata tables, but not vice versa.


  return(valid)
}

assets <- function(x,...)UseMethod('assets')
assets.pharmval <- function(x,...){
  x %>% nonmeta %>% itemize(VARIABLE,TYPEC, SUBTYPEC, TYPE, SUBTYPE) %>%
    arrange(TYPE,SUBTYPE) %>%
    ungroup %>%
    select(-TYPE,-SUBTYPE) %>% as.data.frame(stringsAsFactors=FALSE)
}

filter_.pharmval <- function(.data, ...){
  x <- as.data.frame(.data,stringsAsFactors = FALSE)
  x <- filter_(x, ...)
  class(x) <- class(.data)
  x
}

merge.pharmval <- function(x,y,...){
  stopifnot(inherits(y,'pharmval'))
  z <- rbind(x,y)
  z %<>% ungroup %>% distinct
  z
}

read.pharmval <- function(x,na.strings='.',as.is=TRUE,...)read.csv(x,na.strings=na.strings,as.is=as.is,...) %>% as.pharmval

write.pharmval <- function(x, file, na='.',quote=FALSE,...)write.csv(x,file=file,na=na,quote=quote,...)


