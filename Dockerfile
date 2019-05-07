# 镜像源
FROM rocker/r-base

#作者
MAINTAINER Pharbers Liu <contact@pharbershub>

#LABEL
LABEL 	NtmPods.version="1.0.0" maintainer="Liu"

#下载依赖
RUN apt-get update
RUN apt-get install -y libssl-dev libsasl2-dev
RUN R -e 'install.packages("plyr", repos = "http://cran.cnr.berkeley.edu/")'
RUN R -e 'install.packages("dplyr", repos = "http://cran.cnr.berkeley.edu/")'
RUN R -e 'install.packages("tidyr", repos = "http://cran.cnr.berkeley.edu/")'
RUN R -e 'install.packages("DT", repos = "http://cran.cnr.berkeley.edu/")'
RUN R -e 'install.packages("plumber", repos = "http://cran.cnr.berkeley.edu/")'
RUN R -e 'install.packages("mongolite", repos = "http://cran.cnr.berkeley.edu/")'
RUN R -e 'install.packages("jsonlite", repos = "http://cran.cnr.berkeley.edu/")'

#复制文件
COPY API.R /API.R
COPY Functions.R /Functions.R
COPY Reports.R /Reports.R
EXPOSE 8000

#执行
CMD Rscript /API.R
