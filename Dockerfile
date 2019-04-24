FROM rocker/r-base
MAINTAINER Pharbers Liu <contact@pharbershub>
RUN apt-get update
RUN apt-get install openssl
RUN R -e 'install.packages("openssl", repos = "http://mirrors.tuna.tsinghua.edu.cn/CRAN/")'
RUN R -e 'install.packages("plyr", repos = "http://mirrors.tuna.tsinghua.edu.cn/CRAN/")'
RUN R -e 'install.packages("tidyverse", repos = "http://mirrors.tuna.tsinghua.edu.cn/CRAN/")'
RUN R -e 'install.packages("DT", repos = "http://mirrors.tuna.tsinghua.edu.cn/CRAN/")'
RUN R -e 'install.packages("plumber", repos = "http://mirrors.tuna.tsinghua.edu.cn/CRAN/")'
RUN R -e 'install.packages("mongolite", repos = "http://mirrors.tuna.tsinghua.edu.cn/CRAN/")'
RUN R -e 'install.packages("jsonlite", repos = "http://mirrors.tuna.tsinghua.edu.cn/CRAN/")'
RUN R -e 'install.packages("utf8", repos = "http://mirrors.tuna.tsinghua.edu.cn/CRAN/")'
RUN mkdir /ntm_docker
RUN cd /ntm_docker
COPY API.R /ntm_docker/API.R
COPY Functions.R /ntm_docker/Functions.R
COPY Reports.R /ntm_docker/Reports.R
EXPOSE 8000
CMD Rscript /ntm_docker/API.R
