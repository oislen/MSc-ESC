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
# remove any windows encodings
RUN sed -i 's/\r$//' /home/ubuntu/MSc-ESC/exeRmarkdownReports.sh
RUN sed -i 's/\r$//' /home/ubuntu/MSc-ESC/exeUnittests.sh

# set up home environment
RUN mkdir -p /home/${user} && chown -R ${user}: /home/${user}

# install R and set up environment: https://www.stats.bris.ac.uk/R/
WORKDIR /home/${user}/MSc-ESC
# install dependencies
RUN apt-get update -qq
RUN apt-get install cmake gcc g++ gfortran build-essential libblas-dev liblapack-dev texlive-latex-recommended -y
RUN apt-get install --no-install-recommends software-properties-common dirmngr -y
# install R
RUN wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
RUN add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/" -y
RUN apt-get install --no-install-recommends r-base -y
# install environment
RUN Rscript -e "options(renv.config.pak.enabled=TRUE); Sys.setenv(RENV_CONFIG_PAK_ENABLED=TRUE); renv::restore();"
#RUN bash exeRmarkdownReports.sh
#RUN bash exeUnittests.sh

ENTRYPOINT ["/bin/bash"]