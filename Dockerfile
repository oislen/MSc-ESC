# get base image
FROM ubuntu:latest

# set environment variables
ENV user=ubuntu
ENV DEBIAN_FRONTEND=noninteractive

# install required software and programmes for development environment
RUN apt-get update 
RUN apt-get install -y apt-utils vim curl wget unzip tree htop

# set up home environment
RUN mkdir -p /home/${user} && chown -R ${user}: /home/${user}

# copy MSc-ESC repo
COPY . /home/ubuntu/MSc-ESC

# set up home environment
RUN mkdir -p /home/${user} && chown -R ${user}: /home/${user}

# install R and set up environment
# https://www.stats.bris.ac.uk/R/
WORKDIR /home/${user}/MSc-ESC
RUN apt-get install --no-install-recommends software-properties-common dirmngr -y
RUN add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/" -y
RUN apt-get install --no-install-recommends r-base -y
#RUN Rscript -e "renv::restore()"

ENTRYPOINT ["/bin/bash"]