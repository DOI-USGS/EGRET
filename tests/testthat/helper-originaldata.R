# Choptank_eList has model results already, but the model 
# hasn't been run since it was originally saved. This takes  
# the .RData files available in this package and converts 
# them into their original, pre-modeled form. This is 
# especially necessary for testing any modeling function.
eList_Ch <- Choptank_eList
info_stale_Ch <- getInfo(eList_Ch)
daily_stale_Ch <- getDaily(eList_Ch)
sample_stale_Ch <- getSample(eList_Ch)
surfaces_stale_Ch <- getSurfaces(eList_Ch)

info_orig_Ch <- info_stale_Ch[, 1:(which(names(info_stale_Ch) == "bottomLogQ") - 1)]
daily_orig_Ch <- daily_stale_Ch[, 1:(which(names(daily_stale_Ch) == "Q30") - 1)]
sample_orig_Ch <- sample_stale_Ch[, c("Date","ConcLow", "ConcHigh", "Uncen", "ConcAve", 
                                      "Julian","Month","Day","DecYear","MonthSeq",
                                      "SinDY","CosDY")]
surfaces_orig_Ch <- NA
eList_orig_Ch <- mergeReport(info_orig_Ch, daily_orig_Ch, sample_orig_Ch, surfaces_orig_Ch, verbose = FALSE)
sample_orig_Ch <- getSample(eList_orig_Ch)


# Arkansas_eList has model results already, but the model 
# hasn't been run since it was originally saved. This takes  
# the .RData files available in this package and converts 
# them into their original, pre-modeled form. This is 
# especially necessary for testing any modeling function.
eList_Ar <- Arkansas_eList
info_stale_Ar <- getInfo(eList_Ar)
daily_stale_Ar <- getDaily(eList_Ar)
sample_stale_Ar <- getSample(eList_Ar)
surfaces_stale_Ar <- getSurfaces(eList_Ar)

info_orig_Ar <- info_stale_Ar[, 1:(which(names(info_stale_Ar) == "bottomLogQ") - 1)]
daily_orig_Ar <- daily_stale_Ar[, 1:(which(names(daily_stale_Ar) == "Q30") - 1)]
sample_orig_Ar <- sample_stale_Ar[, 1:(which(names(sample_stale_Ar) == "yHat") - 1)]
surfaces_orig_Ar <- NA
eList_orig_Ar <- mergeReport(info_orig_Ar, daily_orig_Ar, sample_orig_Ar, surfaces_orig_Ar, verbose = FALSE)

