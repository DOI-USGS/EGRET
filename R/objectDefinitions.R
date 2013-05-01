# Updating this list will not automatically update the objects available in the package.
# If you update this list, run the code to generate fluxConst or qUnit, then save the arrays:
# save(fluxConst, file="fluxConst.RData")
# save(qConst, file="qConst.RData")
# save(monthInfo, file="monthInfo.RData")  #shouldn't need to add a month, but maybe you want to add an attribute or correct a mistake
# then, move the files (fluxConst.RData, qConst.RData, monthInfo.RData) to packageFolder/data
# Package will have to be re-built to include new data.

fluxConst <- list(poundsDay = new("fluxUnit", 
                           shortName = "   lbs/day  ",   
                           unitFactor = 2.204623,    
                           unitName = "pounds/day",
                           unitExpress = expression("Observed Flux in pounds/day"),
                           unitExpressTiny = expression("Obs. Flux (lbs/d)"),
                           unitEstimate = expression("Estimated Flux in pounds/day"),
                           unitEstimateTiny = expression("Est. Flux (lbs/d)"),
                           shortCode = 1
                           ),
                tonsDay = new("fluxUnit", 
                           shortName = "   tons/day  ",  
                           unitFactor = 0.001102,    
                           unitName = "tons/day",
                           unitExpress = expression("Observed Flux in tons/day"),
                           unitExpressTiny = expression("Obs. Flux (tons/d)"),
                           unitEstimate = expression("Estimated Flux in tons/day"),
                           unitEstimateTiny = expression("Est. Flux (tons/d)"),
                           shortCode = 2
                           ),
                kgDay = new("fluxUnit", 
                           shortName = "    kg/day  ",   
                           unitFactor = 1,
                           unitName = "kg/day",
                           unitExpress = expression("Observed Flux in kg/day"),
                           unitExpressTiny = expression("Obs. Flux (kg/d)"),
                           unitEstimate = expression("Estimated Flux in kg/day"),
                           unitEstimateTiny = expression("Est. Flux (kg/d)"),
                           shortCode = 3
                           ),
                thousandKgDay = new("fluxUnit", 
                           shortName = " 10^3 kg/day",
                           unitFactor = 0.001,
                           unitName = "thousands of kg/day",
                           unitExpress = expression(paste("Observed Flux in ",10^3*kg/day)),
                           unitExpressTiny = expression(paste("Obs. Flux ", "(", 10^3*kg/d, ")")),
                           unitEstimate = expression(paste("Estimated Flux in ",10^3*kg/day)),
                           unitEstimateTiny = expression(paste("Est. Flux ", "(", 10^3*kg/d, ")")),
                           shortCode = 4
                           ),
                tonsYear = new("fluxUnit", 
                           shortName = "   tons/yr  ",
                           unitFactor = 0.402619,
                           unitName = "tons/year",
                           unitExpress = expression("Observed Flux in tons/year"),
                           unitExpressTiny = expression("Obs. Flux (tons/yr)"),
                           unitEstimate = expression("Estimated Flux in tons/year"),
                           unitEstimateTiny = expression("Est. Flux (tons/yr)"),
                           shortCode = 5
                           ),
                thousandTonsYear = new("fluxUnit", 
                           shortName = "  10^3 tons/yr",
                           unitFactor = 0.000402619,
                           unitName = "thousands of tons/year",
                           unitExpress = expression(paste("Observed Flux in ",10^3*tons/yr)),
                           unitExpressTiny = expression(paste("Obs. Flux ", "(", 10^3*tons/yr, ")")),
                           unitEstimate = expression(paste("Estimated Flux in ",10^3*tons/yr)),
                           unitEstimateTiny = expression(paste("Est. Flux ", "(", 10^3*tons/yr, ")")),
                           shortCode = 6
                           ),
                millionTonsYear = new("fluxUnit",
                           shortName = "  10^6 tons/yr",
                           unitFactor = 4.02619e-07,
                           unitName = "millions of tons/year",
                           unitExpress = expression(paste("Observed Flux in ",10^6*tons/yr)),
                           unitExpressTiny = expression(paste("Obs. Flux ", "(", 10^6*tons/yr, ")")),
                           unitEstimate = expression(paste("Estimated Flux in ",10^6*tons/yr)),
                           unitEstimateTiny = expression(paste("Est. Flux ", "(", 10^6*tons/yr, ")")),
                           shortCode = 7
                           ),
                thousandKgYear = new("fluxUnit",
                           shortName = "   10^3 kg/yr",
                           unitFactor = 0.36525,
                           unitName = "thousands of kg/year",
                           unitExpress = expression(paste("Observed Flux in ",10^3*kg/yr)),
                           unitExpressTiny = expression(paste("Obs. Flux ", "(", 10^3*kg/yr, ")")),
                           unitEstimate = expression(paste("Estimated Flux in ",10^3*kg/yr)),
                           unitEstimateTiny = expression(paste("Est. Flux ", "(", 10^3*kg/yr, ")")),
                           shortCode = 8
                           ),
                millionKgYear = new("fluxUnit",
                           shortName = "   10^6 kg/yr",
                           unitFactor = 0.00036525,
                           unitName = "millions of kg/year",
                           unitExpress = expression(paste("Observed Flux in ",10^6*kg/yr)),
                           unitExpressTiny = expression(paste("Obs. Flux ", "(", 10^6*kg/yr, ")")),
                           unitEstimate = expression(paste("Estimated Flux in ",10^6*kg/yr)),
                           unitEstimateTiny = expression(paste("Est. Flux ", "(", 10^6*kg/yr, ")")),
                           shortCode = 9
                           ),
                billionKgYear = new("fluxUnit",
                           shortName = "   10^9 kg/yr",
                           unitFactor = 3.6525e-07,
                           unitName = "billions of kg/year",
                           unitExpress = expression(paste("Observed Flux in",10^9*kg/yr)),
                           unitExpressTiny = expression(paste("Obs. Flux ", "(", 10^9*kg/yr, ")")),
                           unitEstimate = expression(paste("Estimated Flux in",10^9*kg/yr)),
                           unitEstimateTiny = expression(paste("Est. Flux ", "(", 10^9*kg/yr, ")")),
                           shortCode = 10
                           ),
                thousandTonsDay = new("fluxUnit",
                           shortName = " 10^3 tons/day",
                           unitFactor = 1.102e-06,
                           unitName = "thousands of tons/day",
                           unitExpress = expression(paste("Observed Flux in ",10^3*tons/day)),
                           unitExpressTiny = expression(paste("Obs. Flux ", "(", 10^3*tons/d, ")")),
                           unitEstimate = expression(paste("Estimated Flux in ",10^3*tons/day)),
                           unitEstimateTiny = expression(paste("Est. Flux ", "(", 10^3*tons/d, ")")),
                           shortCode = 11
                           ),
                millionKgDay = new("fluxUnit",
                           shortName = "   10^6 kg/day",
                           unitFactor = 1.000e-06,
                           unitName = "millions of kg/day",
                           unitExpress = expression(paste("Observed Flux in ",10^6*kg/day)),
                           unitExpressTiny = expression(paste("Obs. Flux ", "(", 10^6*kg/d, ")")),
                           unitEstimate = expression(paste("Estimated Flux in ",10^6*kg/day)),
                           unitEstimateTiny = expression(paste("Est. Flux ", "(", 10^6*kg/d, ")")),
                           shortCode = 12
                           )
)

qConst <- list(
                cfs = new("qUnit",
                          qShortName = "   cfs  ",
                          qUnitFactor = 35.314667,
                          qUnitName = "Cubic Feet per Second",
                          qUnitExpress = expression(paste("Discharge in ",ft^3/s)),
                          qUnitTiny = expression(paste("Discharge ", "(", ft^3/s, ")")),
                          shortCode = 1
                          ),
                cms = new("qUnit",
                          qShortName = "   cms  ",
                          qUnitFactor = 1,
                          qUnitName = "Cubic Meters per Second",
                          qUnitExpress = expression(paste("Discharge in ",m^3/s)),
                          qUnitTiny = expression(paste("Discharge ", "(", m^3/s, ")")),
                          shortCode = 2
                          ),
                thousandCfs = new("qUnit",
                          qShortName = "10^3 cfs",
                          qUnitFactor = 0.035314667,
                          qUnitName = "Thousand Cubic Feet per Second",
                          qUnitExpress = expression(paste("Discharge in ",10^3*ft^3/s)),
                          qUnitTiny = expression(paste("Discharge ", "(", 10^3*ft^3/s, ")")),
                          shortCode = 3
                          ),
                thousandCms = new("qUnit",
                          qShortName = "10^3 cms",
                          qUnitFactor = 0.001,
                          qUnitName = "Thousand Cubic Meters per Second",
                          qUnitExpress = expression(paste("Discharge in ",10^3*m^3/s)),
                          qUnitTiny = expression(paste("Discharge ", "(", 10^3*m^3/s, ")")),
                          shortCode = 4
                          ),
                mmDay = new("qUnit",
                          qUnitName = "mm per day",
                          shortCode = 5
                          ),
                mmYear = new("qUnit",
                          qUnitName = "mm per year",
                          shortCode = 6
                          )
)

monthInfo = c(new("monthLabel",
                    monthAbbrev = "Jan",
                    monthFull = "January",
                    monthSingle = "J"
                    ),
              new("monthLabel",
                    monthAbbrev = "Feb",
                    monthFull = "February",
                    monthSingle = "F"
                    ),
              new("monthLabel",
                    monthAbbrev = "Mar",
                    monthFull = "March",
                    monthSingle = "M"
                    ),
              new("monthLabel",
                    monthAbbrev = "Apr",
                    monthFull = "April",
                    monthSingle = "A"
                    ),
              new("monthLabel",
                    monthAbbrev = "May",
                    monthFull = "May",
                    monthSingle = "M"
                    ),
              new("monthLabel",
                    monthAbbrev = "Jun",
                    monthFull = "June",
                    monthSingle = "J"
                    ),
              new("monthLabel",
                    monthAbbrev = "Jul",
                    monthFull = "July",
                    monthSingle = "J"
                    ),
              new("monthLabel",
                    monthAbbrev = "Aug",
                    monthFull = "August",
                    monthSingle = "A"
                    ),
              new("monthLabel",
                    monthAbbrev = "Sep",
                    monthFull = "September",
                    monthSingle = "S"
                    ),
              new("monthLabel",
                    monthAbbrev = "Oct",
                    monthFull = "October",
                    monthSingle = "O"
                    ),
              new("monthLabel",
                    monthAbbrev = "Nov",
                    monthFull = "November",
                    monthSingle = "N"
                    ),
              new("monthLabel",
                    monthAbbrev = "Dec",
                    monthFull = "December",
                    monthSingle = "D"
                    )
)
