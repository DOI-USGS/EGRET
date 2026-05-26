#' Populate Daily data frame
#'
#' Using a data frame that has at least Date, Q, Qualifier, populates the rest of the basic Daily data frame used in EGRET analysis.
#'
#' @param rawData dataframe contains at least Date, Q, Qualifier columns.
#' @param qConvert numeric conversion to cubic meters per second.
#' @param verbose logical specifying whether or not to display messages.
#' @param adjust logical specifying whether or not to add a constant to zero values
#' to allow log transformation. Defaults to TRUE.
#' @param fill logical specifying whether to fill NA values by linear interpolation.
#' Defaults to FALSE.
#' @param maxgap Maximum number of NA days allowed for interpolating gaps.
#' Default is 21. Only used if fill is set to TRUE.
#' @param fill_type character to define what process to fill missing data. Options are
#' "interpolation" - linear interpolation from the
#' `zoo::na.approx`, or "log_interp" - linear interpolation in the log space.
#' Only used if fill is set to TRUE.
#' @keywords WRTDS flow
#' @author Robert M. Hirsch \email{rhirsch@@usgs.gov}
#' @return A data frame 'Daily' with the following columns:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' Q \tab numeric \tab Discharge in m^3/s\cr
#' Julian \tab integer \tab Number of days since Jan. 1, 1850\cr
#' Month \tab integer \tab Month of the year [1-12] \cr
#' Day \tab integer \tab Day of the year [1-366] \cr
#' DecYear \tab numeric \tab Decimal year \cr
#' MonthSeq \tab integer \tab Number of months since January 1, 1850 \cr
#' Qualifier \tab character \tab Qualifying code \cr
#' i \tab integer \tab Index of days, starting with 1 \cr
#' LogQ \tab numeric \tab Natural logarithm of Q  \cr
#' Q7 \tab numeric \tab 7 day running average of Q \cr
#' Q30 \tab numeric \tab 30 day running average of Q \cr
#' }
#' @seealso \code{\link{readNWISDaily}}, \code{\link{readUserDaily}}
#' @export
#' @examples
#' Date <- as.character(seq(from = as.Date("2001/1/1"),
#'                          to = as.Date("2002/1/2"),
#'                          by = "day"))
#' Q <- c(-1:365)
#' Qualifier <- rep("",367)
#' dataInput_complete <- data.frame(Date, Q, Qualifier)
#' dataInput <- dataInput_complete[-4:-5,]
#'
#' # No fill, but with 0 and negative:
#' Daily <- populateDaily(dataInput, qConvert = 1)
#'
#' # No negatives/zeros:
#' Q <- 2+sin(seq(from = 0, to = 2*pi, length.out = 367))
#' Q <- jitter(Q, factor = 500)
#' plot(Q, ylim = c(0, 3.2))
#' dataInput_complete <- data.frame(Date, Q, Qualifier)
#' # Remove some rows to test missing:
#' dataInput <- dataInput_complete[-4:-5,]
#' dataInput <- dataInput[-10:-20,]
#'
#' # No fill:
#' Daily <- populateDaily(dataInput, qConvert = 1)
#' plot(Daily$Date[1:30], Daily$Q[1:30], type = "b", ylim = c(0, 3.2))
#'
#' # Linear interpolation:
#' Daily_fill <- populateDaily(dataInput,
#'                             qConvert = 1,
#'                             fill = TRUE,
#'                             fill_type = "interpolation")
#' plot(Daily_fill$Date[1:30],
#'      Daily_fill$Q[1:30],
#'      col = as.factor(Daily_fill$Qualifier[1:30]),
#'      type = "b", pch = 16, ylim = c(0, 3.2),
#'      main = "Linear Interpolation")
#'
#'
#' # Add a gap that is too big do deal with:
#' dataInput <- dataInput_complete[-4:-20,]
#' dataInput <- dataInput[-200:-255,]
#'
#' Daily_interp <- populateDaily(dataInput,
#'                               qConvert = 1,
#'                               fill = TRUE,
#'                               fill_type = "interpolation")
#' plot(Daily_interp$Date, Daily_interp$Q,
#'      col = as.factor(Daily_interp$Qualifier),
#'      main = "Linear Interpolation",
#'      type = "b", pch = 16, ylim = c(0, 3.2))
#' plot(Daily_interp$Date[1:50], Daily_interp$Q[1:50],
#'      col = as.factor(Daily_interp$Qualifier[1:50]),
#'      main = "Linear Interpolation",
#'      type = "b", pch = 16, ylim = c(0, 3.2))
#'
#' Daily_log_interp <- populateDaily(dataInput,
#'                               qConvert = 1,
#'                               fill = TRUE,
#'                               fill_type = "log_interp")
#' plot(Daily_log_interp$Date, Daily_log_interp$Q,
#'      col = as.factor(Daily_log_interp$Qualifier),
#'      main = "Linear Interpolation in Log Scale",
#'      type = "b", pch = 16, ylim = c(0, 3.2))
#' plot(Daily_log_interp$Date[1:50], Daily_log_interp$Q[1:50],
#'      col = as.factor(Daily_log_interp$Qualifier[1:50]),
#'      main = "Linear Interpolation in Log Scale",
#'      type = "b", pch = 16, ylim = c(0, 3.2))
#'
#'
#' # Real data:
#' eList <- Choptank_eList
#' Daily_chop <- eList$Daily
#' df <- Daily_chop[,c("Date", "Q")]
#' df <- df[-2:-5, ]
#' df <- df[-100:-200,]
#' D2 <- populateDaily(df, 1, fill = TRUE)
#' plot(D2$Date[1:20], D2$Q[1:20],
#'      col = as.factor(D2$Qualifier[1:20]),
#'      main = "Linear Interpolation",
#'      type = "b", pch = 16)
#' plot(D2$Date[1:110], D2$Q[1:110],
#'      col = as.factor(D2$Qualifier[1:110]),
#'      main = "Linear Interpolation",
#'      type = "b", pch = 16)
#'
populateDaily <- function(
  rawData,
  qConvert,
  verbose = TRUE,
  adjust = TRUE,
  fill = FALSE,
  maxgap = 21,
  fill_type = "interpolation"
) {
  localDaily <- as.data.frame(matrix(ncol = 3, nrow = nrow(rawData)))
  colnames(localDaily) <- c('Date', 'Q', 'Qualifier')

  if ("code" %in% names(rawData)) {
    localDaily$Qualifier <- rawData$code
  } else if ("qualifier" %in% names(rawData)) {
    localDaily$Qualifier <- rawData$qualifier
  } else {
    localDaily$Qualifier <- ""
  }

  if ("time" %in% names(rawData)) {
    localDaily$Date <- rawData$time
  } else if ("Date" %in% names(rawData)) {
    localDaily$Date <- rawData$Date
  } else {
    localDaily$Date <- rawData[[1]]
  }

  if ("value" %in% names(rawData)) {
    localDaily$Q <- rawData$value / qConvert
  } else if ("Q" %in% names(rawData)) {
    localDaily$Q <- rawData$Q / qConvert
  } else {
    localDaily$Q <- rawData[[2]] / qConvert
  }

  # Make complete daily time series
  all_dates <- data.frame(
    Date = seq.Date(
      from = min(as.Date(localDaily$Date), na.rm = TRUE),
      to = max(as.Date(localDaily$Date), na.rm = TRUE),
      by = "day"
    )
  )
  localDaily <- merge(all_dates, localDaily, all.x = TRUE)

  # Populate date columns
  dateFrame <- populateDateColumns(localDaily$Date)
  localDaily <- merge(localDaily, dateFrame)

  localDaily <- localDaily[, c(
    setdiff(names(localDaily), "Qualifier"),
    "Qualifier"
  )]
  localDaily$i <- 1:nrow(localDaily)

  # Zero and negative discharge
  nz <- length(which(localDaily$Q == 0))
  nn <- length(which(localDaily$Q < 0))

  if (nn > 0) {
    if (verbose) {
      message(paste("There are", as.character(nn), "negative flow days."))
      message("Many EGRET functions will not work with negative values.")
    }
  }

  qshift <- 0.0
  if (adjust) {
    if (nz > 0 & nn == 0) {
      qshift <- 0.001 * mean(localDaily$Q, na.rm = TRUE)
      if (verbose) {
        message(paste(
          "There are",
          as.character(nz),
          "zero flow days and no negative flow days."
        ))
        message(paste(
          "All days had",
          as.character(round(qshift, 4)),
          "cms added to the discharge value."
        ))
      }
    } else if (nn > 0) {
      if (verbose) {
        message(paste(
          "Adjust is TRUE but there are",
          as.character(nn),
          "negative flow days."
        ))
        message("Discharge was not adjusted.")
      }
    }
  }

  localDaily$Q <- localDaily$Q + qshift
  localDaily$LogQ <- suppressWarnings(
    log(localDaily$Q)
  )

  # Rolling mean discharge
  ma <- function(x, n = 7) {
    stats::filter(x, rep(1 / n, n), sides = 1)
  }

  if (length(localDaily$Date) < 30) {
    if (verbose) {
      message(
        "Rolling means not calculated because there are fewer than 30 days."
      )
    }
  } else {
    localDaily$Q7 <- as.numeric(ma(localDaily$Q))
    localDaily$Q30 <- as.numeric(ma(localDaily$Q, 30))
  }

  # NA discharge values
  nd <- localDaily$Q == -999999
  localDaily$Q <- ifelse(nd, NA, localDaily$Q)
  if (verbose) {
    na_ind <- which(is.na(localDaily$Q))
    total_na <- length(na_ind)
    if (total_na > 0) {
      perc_missing <- signif(100 * total_na / nrow(localDaily), digits = 3)
      message(paste0(perc_missing, "% missing data."))
      breaks <- c(0, which(diff(na_ind) > 1), length(na_ind))
      ranges <- mapply(
        function(start, end) {
          ind_start <- na_ind[start + 1]
          ind_end <- na_ind[end]
          n_days <- ind_end - ind_start + 1
          sprintf(
            "  %s to %s (%d day%s)",
            format(localDaily$Date[ind_start], "%Y-%m-%d"),
            format(localDaily$Date[ind_end], "%Y-%m-%d"),
            n_days,
            if (n_days == 1) "" else "s"
          )
        },
        breaks[-length(breaks)],
        breaks[-1],
        SIMPLIFY = TRUE
      )
      message(sprintf(
        "NA ranges in Q (%d total NA value%s across %d run%s):",
        total_na,
        if (total_na == 1) "" else "s",
        length(ranges),
        if (length(ranges) == 1) "" else "s"
      ))
      message(paste(ranges, collapse = "\n"))
    }
  }

  # Fill NA discharge by linear interpolation
  if (fill) {
    localDaily <- fill_missing_daily(
      df = localDaily,
      fill_type = fill_type,
      maxgap = maxgap,
      value_col = "Q",
      qualifier_col = "Qualifier"
    )

    localDaily$LogQ <- suppressWarnings(
      log(localDaily$Q)
    )
    if (length(localDaily$Date) >= 30) {
      localDaily$Q7 <- as.numeric(ma(localDaily$Q))
      localDaily$Q30 <- as.numeric(ma(localDaily$Q, 30))
    }
    if (verbose) {
      message(sprintf(
        "NA values filled by %s when gap range less than %s days.",
        fill_type,
        maxgap
      ))
    }
  } else {
    if (any(is.na(localDaily$Q))) {
      message(paste("Many EGRET functions will not work with missing values."))
    }
  }

  attr(localDaily, "number_of_zero_flow") <- nz
  attr(localDaily, "number_of_negative_flow") <- nn
  attr(localDaily, "qshift") <- qshift

  return(localDaily)
}


#' Fill missing daily data
#'
#' Uses either linear interpolation, linear interpolation in the log
#' scale, or fixed-interval smoothing on time series to fill missing data. This
#' function gets used within the Daily functions if fill=TRUE. As a
#' standalone function, the input can be data directly download from
#' `dataRetrieval::read_waterdata_daily`.
#'
#' @param df Data frame with at least value and qualifier columns. The names
#' of those columns are defined by value_col and qualifier_col. The data
#' frame is expected to be a complete and uniform time series. This could
#' be one row per day, or one row per X interval. The data in the value_col
#' will be filled in with the assumption that the data is uniform and any
#' missing data is set as `NA`.
#' @param maxgap Maximum number of NA days allowed for interpolating gaps.
#' Default is 21. Only used if fill is set to TRUE.
#' @param fill_type character to define what process to fill missing data. Options are
#' "interpolation" - linear interpolation from the
#' `zoo::na.approx`, or "log_interp" - linear interpolation in the log space.
#' Only used if fill is set to TRUE.
#' @param value_col Character, name of value column.
#' @param qualifier_col Character, name of qualifier column.
#' @export
#' @examples
#' Date <- seq(from = as.Date("2001/1/1"),
#'             to = as.Date("2002/1/2"),
#'             by = "day")
#' Qualifier <- rep("",367)
#' Q <- 2+sin(seq(from = 0, to = 2*pi, length.out = 367))
#' Q <- jitter(Q, factor = 500)
#' plot(Q, ylim = c(0, 3.2))
#' dataInput <- data.frame(time = Date,
#'                         value = Q,
#'                         qualifier = Qualifier)
#' # Remove some rows to test missing:
#' dataInput$value[4:5] <- NA
#' dataInput$value[10:20] <- NA
#'
#' # Linear interpolation:
#' interp1 <- fill_missing_daily(df = dataInput,
#'                               fill_type = "interpolation")
#' plot(interp1$time[1:30],
#'      interp1$value[1:30],
#'      col = as.factor(interp1$qualifier[1:30]),
#'      type = "b", pch = 16, ylim = c(0, 3.2),
#'      main = "Linear Interpolation")
#'
#' # Add a gap that is too big do deal with:
#' dataInput$value[200:255] <- NA
#'
#' df_interp <- fill_missing_daily(dataInput,
#'                                 fill_type = "interpolation")
#' plot(df_interp$time, df_interp$value,
#'      col = as.factor(df_interp$qualifier),
#'      main = "Linear Interpolation",
#'      type = "b", pch = 16, ylim = c(0, 3.2))
#' plot(df_interp$time[1:50], df_interp$value[1:50],
#'      col = as.factor(df_interp$qualifier[1:50]),
#'      main = "Linear Interpolation",
#'      type = "b", pch = 16, ylim = c(0, 3.2))
#'
#' df_log_interp <- fill_missing_daily(dataInput,
#'                                     fill_type = "log_interp")
#' plot(df_log_interp$time, df_log_interp$value,
#'      col = as.factor(df_log_interp$qualifier),
#'      main = "Linear Interpolation in Log Scale",
#'      type = "b", pch = 16, ylim = c(0, 3.2))
#' plot(df_log_interp$time[1:50], df_log_interp$value[1:50],
#'      col = as.factor(df_log_interp$qualifier[1:50]),
#'      main = "Linear Interpolation in Log Scale",
#'      type = "b", pch = 16, ylim = c(0, 3.2))
#'
fill_missing_daily <- function(
  df,
  fill_type,
  maxgap = 21,
  value_col = "value",
  qualifier_col = "qualifier"
) {
  match.arg(
    fill_type,
    choices = c(
      "interpolation",
      "log_interp"
    )
  )

  if (fill_type == "interpolation") {
    q = df[[value_col]]
  } else {
    q = log(df[[value_col]])
  }
  
  Q_interp <- zoo::na.approx(q, maxgap = maxgap, na.rm = FALSE)
  df[[qualifier_col]][is.na(df[[value_col]])] <- "INTERPOLATED"
  
  if (fill_type == "log_interp") {
    Q_interp <- exp(Q_interp)
  }
  
  df[[value_col]][is.na(df[[value_col]])] <- Q_interp[is.na(df[[value_col]])]


  # If gaps were too big, remove fit label:
  df[[qualifier_col]][is.na(df[[value_col]])] <- ""

  return(df)
}
