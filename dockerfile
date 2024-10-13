#############
# Create image with: 
#
#   docker build --build-arg USR=name --build-arg PWD=password --build-arg R_VERSION=4.1.1  -t generatordashboard .
#
##############

# ********* base image
FROM centos:7
LABEL maintainer 

# ******* build arguments
ARG USR
ARG PWD
ARG R_VERSION
ENV USR_ENV=${USR}
ENV PWD_ENV=${PWD}

# ***** copy app content onto image 
RUN mkdir /gendash
COPY GenDash /gendash

# *** Add EPEL EXTENDED PACKAGE REPO
RUN yum -y install https://dl.fedoraproject.org/pub/epel/epel-release-latest-7.noarch.rpm

# ****** UPDATE & INSTALL DEPENDENCIES *********
RUN yum -y update && \
yum -y install nano libcurl texinfo-tex openblas-devel libxml2-devel openssl-devel krb5-workstation java-11-openjdk-devel

# **** Install MS SQL Server Driver
RUN curl https://packages.microsoft.com/config/rhel/8/prod.repo > /etc/yum.repos.d/mssql-release.repo && yum remove unixODBC-utf16 unixODBC-utf16-devel && ACCEPT_EULA=Y yum install -y msodbcsql17 && ACCEPT_EULA=Y yum install -y mssql-tools && echo 'export PATH="$PATH:/opt/mssql-tools/bin"' >> ~/.bashrc  && source ~/.bashrc && yum install -y unixODBC-devel

# ****** DOWNLOAD AND INSTALL R
RUN curl -O  https://cdn.rstudio.com/r/centos-7/pkgs/R-${R_VERSION}-1-1.x86_64.rpm &&\
yum -y install R-${R_VERSION}-1-1.x86_64.rpm &&\
ln -sfn /opt/R/${R_VERSION}/bin/R /usr/local/bin/R &&\
ln -sfn /opt/R/${R_VERSION}/bin/Rscript /usr/local/bin/Rscript

# ******* fix Java
ENV JAVA_HOME=/usr/lib/jvm/java-11-openjdk-11.0.12.0.7-0.el7_9.x86_64
RUN R CMD javareconf
RUN R -e "install.packages('rJava',INSTALL_opts = '--no-lock',repos='https://cloud.r-project.org')"

# ****** INSTALL R PACKAGES ***********
RUN R -e "install.packages(c('shiny','plyr','dplyr','RODBC','shinydashboard','shinydashboardPlus','DT','data.table','shinyjs','openxlsx','stringr','formattable','shinyWidgets','lubridate','tidyverse','shinyscreenshot'),INSTALL_opts = '--no-lock',repos='https://cloud.r-project.org')"

# ****** WRITE AUTH VARIABLES
RUN cat /gendash/.Renviron > /root/.Renviron
RUN sed -i 's/user/'"$USR_ENV"'/g' /root/.Renviron && sed -i 's/pwd/'"$PWD_ENV"'/g' /root/.Renviron

# ******* fix Kerberos
RUN sed -i 's/# default_realm = EXAMPLE.COM/default_realm = ******.INT/g' /etc/krb5.conf  &&\
sed -i 's/default_ccache_name = KEYRING:persistent:%{uid}/#  default_ccache_name = KEYRING:persistent:%{uid}/g' /etc/krb5.conf

# instruct Docker to expose port 3838 to the outside world
# (otherwise it will not be possible to connect to the Shiny application)
EXPOSE 3838

# finally, instruct how to launch the Shiny app when the container is started
CMD ["R", "-e", "shiny::runApp('/gendash')"]
