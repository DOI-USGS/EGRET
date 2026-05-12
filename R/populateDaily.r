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
#' "interpolation", "spline", or "tsSmooth". "interpolation" is linear interpolation from the
#' `zoo::na.approx`. "spline" is a spline fit using `zoo::na.spline`. "tsSmooth" uses
#' `stats::tsSmooth` which is fixed-interval smoothing on time series.
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
#' plot(Q)
#' dataInput_complete <- data.frame(Date, Q, Qualifier)
#' # Remove some rows to test missing:
#' dataInput <- dataInput_complete[-4:-5,]
#'
#' # No fill:
#' Daily <- populateDaily(dataInput, qConvert = 1)
#' plot(Daily$Date[1:10], Daily$Q[1:10], type = "b")
#'
#' # Linear interpolation:
#' Daily_fill <- populateDaily(dataInput,
#'                             qConvert = 1,
#'                             fill = TRUE,
#'                             fill_type = "interpolation")
#' plot(Daily_fill$Date[1:10],
#'      Daily_fill$Q[1:10],
#'      col = as.factor(Daily_fill$Qualifier),
#'      type = "b", pch = 16,
#'      main = "Linear Interpolation")
#'
#' # Spline fit:
#' Daily_spline <- populateDaily(dataInput,
#'                               qConvert = 1,
#'                               fill = TRUE,
#'                               fill_type = "spline")
#' plot(Daily_spline$Date[1:10],
#'      Daily_spline$Q[1:10],
#'      col = as.factor(Daily_spline$Qualifier),
#'      main = "Spline Fit",
#'      type = "b", pch = 16 )
#'
#' # Fixed-Interval Smoothing on Time Series:
#' Daily_tsSmooth <- populateDaily(dataInput,
#'                               qConvert = 1,
#'                               fill = TRUE,
#'                               fill_type = "tsSmooth")
#' plot(Daily_tsSmooth$Date[1:10],
#'      Daily_tsSmooth$Q[1:10],
#'      col = as.factor(Daily_tsSmooth$Qualifier),
#'      main = "Fixed-interval smoothing on time series",
#'      type = "b", pch = 16)
#'
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
#'      type = "b", pch = 16)
#' plot(Daily_interp$Date[1:50], Daily_interp$Q[1:50],
#'      col = as.factor(Daily_interp$Qualifier[1:50]),
#'      main = "Linear Interpolation",
#'      type = "b", pch = 16)
#'
#' Daily_spline <- populateDaily(dataInput,
#'                              qConvert = 1,
#'                              fill = TRUE,
#'                              fill_type = "spline")
#' plot(Daily_spline$Date[1:50], Daily_spline$Q[1:50],
#'      col = as.factor(Daily_spline$Qualifier[1:50]),
#'      main = "Spline Fit",
#'      type = "b", pch = 16)
#'
#' Daily_tsSmooth <- populateDaily(dataInput,
#'                                 qConvert = 1,
#'                                 fill = TRUE,
#'                                 fill_type = "tsSmooth")
#' plot(Daily_tsSmooth$Date[1:50], Daily_tsSmooth$Q[1:50],
#'      col = as.factor(Daily_tsSmooth$Qualifier[1:50]),
#'      type = "b", pch = 16)
#'
#' eList <- Choptank_eList
#' Daily_chop <- eList$Daily
#' df <- Daily_chop[,c("Date", "Q")]
#' df <- df[-2:-5, ]
#' df <- df[-100:-200,]
#' D2 <- populateDaily(df, 1, fill = TRUE)
#' plot(D2$Date[1:10], D2$Q[1:10],
#'      col = as.factor(D2$Qualifier[1:10]),
#'      type = "b", pch = 16)
#' plot(D2$Date[1:365], D2$Q[1:365],
#'      col = as.factor(D2$Qualifier[1:365]),
#'      type = "b", pch = 16)
#'
populateDaily <- function(
  rawData,
  qConvert,
  verbose = TRUE,
  adjust = TRUE,
  fill = FALSE,
  maxgap = 21,
  fill_type = c("interpolation")
) {
  if (fill) {
    match.arg(fill_type, choices = c("interpolation", "spline", "tsSmooth"))
  }

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
    if (fill_type == "interpolation") {
      Q_interp <- zoo::na.approx(localDaily$Q, maxgap = maxgap, na.rm = FALSE)
      localDaily$Qualifier[is.na(localDaily$Q)] <- "INTERPOLATED"
      localDaily$Q[is.na(localDaily$Q)] <- Q_interp[is.na(localDaily$Q)]
    } else if (fill_type == "spline") {
      Q_spline <- zoo::na.spline(localDaily$Q, maxgap = maxgap, na.rm = FALSE)
      localDaily$Qualifier[is.na(localDaily$Q)] <- "SPLINE FIT"
      localDaily$Q[is.na(localDaily$Q)] <- Q_spline[is.na(localDaily$Q)]
    } else if (fill_type == "tsSmooth") {
      browser()
      missing <- rle(is.na(localDaily$Q))
      my_series <- stats::window(localDaily$Q)
      my_struct <- stats::StructTS(my_series, type = "trend")
      fit <- stats::tsSmooth(my_struct)
      localDaily$Qualifier[is.na(localDaily$Q)] <- "tsSmooth FIT"
      localDaily$Q[is.na(localDaily$Q)] <- fit[is.na(localDaily$Q), 1]
    }

    # If gaps were too big, remove fit label:
    localDaily$Qualifier[is.na(localDaily$Q)] <- ""
    localDaily$LogQ <- suppressWarnings(
      log(localDaily$Q)
    )
    if (length(localDaily$Date) >= 30) {
      localDaily$Q7 <- as.numeric(ma(localDaily$Q))
      localDaily$Q30 <- as.numeric(ma(localDaily$Q, 30))
    }
    if (verbose) {
      message(sprintf(
        "NA values filled by linear interpolation when gap range less than %s days.",
        maxgap
      ))
    }
  } else {
    if (any(is.na(localDaily$Q))) {
      message(paste("Many EGRET functions will not work with missing values."))
    }
  }

  return(localDaily)
}
