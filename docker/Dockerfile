FROM code.chs.usgs.gov:5001/ctek/docker/r-lang/r-base:4.4

# Necessary R libraries
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
  pandoc-citeproc \
  qpdf \
  r-cran-rmarkdown \
  r-cran-knitr \
  r-cran-devtools \
  r-cran-tidyverse \
  r-cran-dataretrieval \
  r-cran-survival \
  r-cran-fields \
  r-cran-foreach \
  r-cran-truncnorm \
  r-cran-extrafont \
  r-cran-egretci \
  r-cran-rkt \
  r-cran-pkgdown \
  r-cran-doparallel \
  r-cran-png \
  r-cran-connectapi \
  r-cran-zyp \
  r-cran-covr \
  r-cran-zoo \
  r-cran-data.table \
  && apt-get install -y pandoc \
  && rm -rf /var/lib/apt/lists/*




