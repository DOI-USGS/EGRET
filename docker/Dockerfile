FROM code.chs.usgs.gov:5001/ctek/docker/r-lang/r-base:4.4

# Necessary R libraries
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
  pandoc-citeproc \
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
  r-cran-parallel \
  r-cran-png \
  r-cran-connectapi \
  && rm -rf /var/lib/apt/lists/*



