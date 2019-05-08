FROM rocker/r-base
MAINTAINER Pharbers Liu <contact@pharbershub>
RUN apt-get update
RUN apt-get install -y libssl-dev libsasl2-dev
RUN R -e 'install.packages("plyr", repos = "http://cran.cnr.berkeley.edu/")'
RUN R -e 'install.packages("dplyr", repos = "http://cran.cnr.berkeley.edu/")'
RUN R -e 'install.packages("tidyr", repos = "http://cran.cnr.berkeley.edu/")'
RUN R -e 'install.packages("DT", repos = "http://cran.cnr.berkeley.edu/")'
RUN R -e 'install.packages("plumber", repos = "http://cran.cnr.berkeley.edu/")'
RUN R -e 'install.packages("mongolite", repos = "http://cran.cnr.berkeley.edu/")'
RUN R -e 'install.packages("jsonlite", repos = "http://cran.cnr.berkeley.edu/")'
COPY API.R /API.R
COPY Functions.R /Functions.R
COPY Reports.R /Reports.R
EXPOSE 8000
CMD Rscript /API.R
