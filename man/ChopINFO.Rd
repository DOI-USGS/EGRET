\docType{data}
\name{ChopINFO}
\alias{ChopINFO}
\title{Example INFO dataframe}
\format{Data frame with 1 row and 56 columns, some of which are described here:
\tabular{lll}{
ColumnName \tab Type \tab Description \cr
shortName \tab string \tab Name of site, suitable for use in graphical headings \cr
staAbbrev \tab string \tab Abbreviation for station name, used in saveResults \cr
paramShortName \tab string \tab Name of constituent, suitable for use in graphical headings \cr
constitAbbrev \tab string \tab Abbreviation for constituent name, used in saveResults \cr
drainSqKm \tab numeric \tab Drainage area in  km^2 \cr
paStart \tab integer (1-12) \tab Starting month of period of analysis \cr
paLong \tab integer (1-12) \tab Length of period of analysis in months \cr
}}
\source{
Parameter information (Nitrate/nitrite, USGS parameter code
00631) is found from NWISweb
(\url{http://nwis.waterdata.usgs.gov/nwis/pmcodes/}), and
station information is retrieved from NWIS water services
(\url{http://waterservices.usgs.gov/}), both via the
dataRetrieval R package. The USGS site id is 01491000.
}
\description{
Example data representing meta-data from the Choptank River
at Greensboro, MD, USGS data as augmented by the functions
setupYears (for WRTDS) and setPA (for flowHistory).
}
\author{
Robert M. Hirsch \email{rhirsch@usgs.gov}
}
\keyword{data}
\keyword{quality}
\keyword{water}

