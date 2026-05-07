## ----collapse = TRUE----------------------------------------------------------
library(EGRET)
Daily <- readNWISDaily(siteNumber = "04191058",
                       parameterCd = "00060",
                       startDate = "2015-04-15",
                       endDate = "2024-10-01",
                       verbose = TRUE,
                       convert = TRUE,
                       adjust = TRUE,
                       fill = FALSE)

## ----collapse = TRUE----------------------------------------------------------
Daily <- readNWISDaily(siteNumber = "04191058",
                       parameterCd = "00060",
                       startDate = "2015-04-15",
                       endDate = "2024-10-01",
                       verbose = TRUE,
                       convert = TRUE,
                       adjust = FALSE,
                       fill = TRUE)

## ----collapse = TRUE----------------------------------------------------------
Daily <- readNWISDaily(siteNumber = "04176063",
                       parameterCd = "00060",
                       startDate = "2020-10-01",
                       endDate = "2024-09-01",
                       verbose = TRUE,
                       convert = TRUE,
                       adjust = TRUE,
                       fill = FALSE)

## ----collapse = TRUE----------------------------------------------------------
Sample <- readNWISSample(siteNumber = "04165710",
                         parameterCd = "00665",
                         verbose = TRUE)

## ----collapse = TRUE----------------------------------------------------------
Sample <- readWQPSample(siteNumber = "USGS-04165710",
                        characteristicName = "00665",
                        verbose = TRUE)

