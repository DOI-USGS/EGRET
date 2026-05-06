#' Populate Daily data frame
#'
#' Using a data frame that has at least Date, Q, Qualifier, populates the rest of the basic Daily data frame used in EGRET analysis.
#'
#' @param localDaily dataframe contains at least Date, Q, Qualifier columns.
#' @param qConvert numeric conversion to cubic meters per second.
#' @param verbose logical specifying whether or not to display messages.
#' @param adjust logical specifying whether or not to add a constant to zero values 
#' to allow log transformation. Defaults to TRUE.
#' @param fill logical specifying whether to fill NA values by linear interpolation.
#' Defaults to FALSE.
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
#' Date <- as.character(seq(as.Date("2001/1/1"),
#'          as.Date("2002/1/2"), by = "day"))
#' Q <- -1:365
#' Qualifier <- rep("",367)
#' dataInput <- data.frame(Date, Q, Qualifier, stringsAsFactors=FALSE)
#' Daily <- populateDaily(dataInput, 1)
#' 
populateDaily <- function(localDaily, qConvert, verbose = TRUE, adjust = TRUE, fill = FALSE) {
  
  localDaily$Q <- localDaily$Q / qConvert
  
  # Make complete daily time series
  all_dates <- data.frame(Date = seq.Date(min(localDaily$Date), max(localDaily$Date), by = "day"))
  localDaily <- merge(all_dates, localDaily, all.x = TRUE)
  
  # Populate date columns
  dateFrame <- populateDateColumns(localDaily$Date)
  localDaily <- merge(localDaily, dateFrame)
  localDaily <- localDaily[, c(setdiff(names(localDaily), "Qualifier"), "Qualifier")]
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
        message(paste("There are", as.character(nz), "zero flow days and no negative flow days."))
        message(paste("All days had", as.character(round(qshift, 4)),
                      "cms added to the discharge value."))
      }
    } else if (nn > 0) {
      if (verbose) {
        message(paste("Adjust is TRUE but there are", as.character(nn), "negative flow days."))
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
      message("Rolling means not calculated because there are fewer than 30 days.")
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
      breaks <- c(0, which(diff(na_ind) > 1), length(na_ind))
      ranges <- mapply(function(start, end) {
        ind_start <- na_ind[start + 1]
        ind_end <- na_ind[end]
        n_days <- ind_end - ind_start + 1
        sprintf("  %s to %s (%d day%s)",
                format(localDaily$Date[ind_start], "%Y-%m-%d"),
                format(localDaily$Date[ind_end],   "%Y-%m-%d"),
                n_days, if (n_days == 1) "" else "s")
      }, breaks[-length(breaks)], breaks[-1], SIMPLIFY = TRUE)
      message(sprintf("NA ranges in Q (%d total NA value%s across %d run%s):",
                      total_na, if (total_na == 1) "" else "s",
                      length(ranges), if (length(ranges) == 1) "" else "s"))
      message(paste(ranges, collapse = "\n"))
      message(paste("Many EGRET functions will not work with missing values."))
    }
  }
  
  # Fill NA discharge by linear interpolation
  if (fill) {
    localDaily$Qualifier[is.na(localDaily$Q)] <- "INTERPOLATED"
    localDaily$Q = approx(localDaily$i[!(is.na(localDaily$Q))], localDaily$Q[!(is.na(localDaily$Q))], localDaily$i)$y
    localDaily$LogQ <- suppressWarnings(
      log(localDaily$Q)
    )
    if (length(localDaily$Date) >= 30) {
      localDaily$Q7 <- as.numeric(ma(localDaily$Q))
      localDaily$Q30 <- as.numeric(ma(localDaily$Q, 30))
    }
    if (verbose) {
      message("All NA values filled by linear interpolation.")
    }
  }
  
  return(localDaily)
}


