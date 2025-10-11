# get base image
FROM r-base:4.4.2

# set environment variables
ENV user=user
ENV DEBIAN_FRONTEND=noninteractive

# install required software and programmes for development environment
RUN apt-get update
RUN apt-get install -y apt-utils vim curl wget unzip tree htop adduser texlive-latex-recommended

# set up home environment
RUN adduser ${user}
RUN mkdir -p /home/${user} && chown -R ${user}: /home/${user}

# copy MSc-ESC repo
COPY . /home/${user}/MSc-ESC
# remove any windows encodings
RUN sed -i 's/\r$//' /home/${user}/MSc-ESC/exeRmarkdownReports.sh
RUN sed -i 's/\r$//' /home/${user}/MSc-ESC/exeUnittests.sh

# install R environment
WORKDIR /home/${user}/MSc-ESC
RUN Rscript -e "options(renv.config.pak.enabled=TRUE); Sys.setenv(RENV_CONFIG_PAK_ENABLED=TRUE); renv::restore();"

ENTRYPOINT ["/bin/bash"]