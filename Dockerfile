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

# install R and set up environment: https://www.stats.bris.ac.uk/R/
WORKDIR /home/${user}/MSc-ESC
# install dependencies
RUN apt-get update -qq
RUN apt-get install  gcc g++ gfortran build-essential libblas-dev liblapack-dev -y
RUN apt-get install --no-install-recommends software-properties-common dirmngr -y
# install R
RUN wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
RUN add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/" -y
RUN apt-get install --no-install-recommends r-base -y
# install environment
RUN Rscript -e "renv::restore()"
#RUN bash scripts/exeUnittests.sh

ENTRYPOINT ["/bin/bash"]