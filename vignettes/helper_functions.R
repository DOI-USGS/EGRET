
join_qw_uv <- function(qw_data, # data from readWQP
                          uv_flow_qw, # data from readNWISuv
                          hour_threshold = 24, # hours threshold for joining
                          join_by_qw = "ActivityStartDateTime", 
                          join_by_uv = "dateTime",
                          qw_val = "ResultMeasureValue",
                          qw_rmk = "ResultDetectionConditionText",
                          qw_det_val = "DetectionQuantitationLimitMeasure.MeasureValue",
                          qw_val_uv, # water quality value column in uv data
                          qw_rmk_uv, # water quality remark column in uv data
                          flow_val = "X_00060_00000", # uv flow parameter
                          flow_rmk = "X_00060_00000_cd"){ # uv flow parameter cd
  
  library(data.table)
  req_cols <- c(join_by_qw, qw_val, qw_rmk, qw_det_val)
  if(!all(req_cols %in% names(qw_data))){
    stop(paste('qw_data missing columns:', req_cols[!req_cols %in% names(qw_data)]))
  }
  
  req_cols_uv <- c(join_by_uv)
  if(!all(req_cols_uv %in% names(uv_flow_qw))){
    stop(paste('uv_data missing columns:', req_cols_uv[!req_cols_uv %in% names(uv_flow_qw)]))
  }
  
  data.table::setDT(qw_data)[, eval(parse(text = paste("join_date :=", join_by_qw)))]
  
  data.table::setDT(uv_flow_qw)[, eval(parse(text = paste("join_date :=", join_by_uv)))]
  
    # rolling join
  x <- uv_flow_qw[qw_data, on = .(join_date), roll = "nearest"]
  
  setnames(x, c(qw_val, join_by_uv, join_by_qw, qw_rmk, qw_det_val),
           c("val_qw","uv_date", "qw_date", "qw_rmk", "qw_det_val"))
  
  x <- x[order(qw_date)]
  
  x_tib <- as_tibble(x)
  
  if(!is.na(flow_val) | flow_val != ""){
    x_tib$flow_uv <- x_tib[[flow_val]]
  }
  if(!is.na(flow_rmk) | flow_rmk != ""){
    x_tib$flow_rmk_uv <- x_tib[[flow_rmk]]
  }
  
  if(!is.na(qw_val_uv) | qw_val_uv != ""){
    x_tib$qw_val_uv <- x_tib[[qw_val_uv]]
  }
  if(!is.na(qw_rmk_uv) | qw_rmk_uv != ""){
    x_tib$qw_rmk_uv <- x_tib[[qw_rmk_uv]]
  }
  
  toMatch <- c("NON-DETECT", "NON DETECT", "NOT DETECTED",
               "DETECTED NOT QUANTIFIED", "BELOW QUANTIFICATION LIMIT")
  
  x_tib <- x_tib |> 
    mutate(delta_time = difftime(qw_date, uv_date, units = "hours"),
           qw_val_uv = if_else(abs(as.numeric(delta_time)) >= hour_threshold, 
                               NA, qw_val_uv),
           qualifier = if_else(grepl(paste(toMatch,collapse="|"),
                                     toupper(qw_rmk)),
                               "<", ""),
           value = if_else(qualifier == "<", qw_det_val, val_qw),
           date = as.Date(qw_date)) |> 
    select(any_of(c("uv_date", "qw_date", "delta_time", "date",
                    "qw_val_uv", "qw_rmk_uv",
                    "value", "qualifier", 
                    "flow_uv", "flow_rmk_uv"))) |> 
    rename(dateTime = qw_date) 
  
  
  compressedData <- EGRET::compressData(x_tib[, c("date",
                                                  "qualifier",
                                                  "value")],
                                        verbose = FALSE)
  Sample <- EGRET::populateSampleColumns(compressedData)
  Sample <- Sample |>
    left_join(x_tib |>
                select(-qualifier) |> 
                rename(qw_dateTime = dateTime,
                       uv_dateTime = uv_date,
                       Date = date, 
                       ConcHigh = value),
              by = c("Date", "ConcHigh"))
  
  return(Sample)
  
}
