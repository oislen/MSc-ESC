# Eurovision Song Contest - MSc Dissertation

This repository contains the code, data, and models for the 2018 TU Dublin MSc Disseration on the Eurovision Song Contest. The final submitted dissertation can be found here on the Technology University Dublin arrow website:

* [An Investigation into Factors Which Explain the Scores and Voting Patterns of the Eurovision Song Contest](https://arrow.tudublin.ie/scschcomdis/155/)

# Abstract

The Eurovision Song Contest (ESC) is an annual international television song competition. Participating countries send a group or individual artist to perform an original song at the competition. The winner is decided by all participating countries using a voting system that incorporates both a public televote and an expert jury vote. Countries are excluded from voting for their entry and the country with the highest score wins. A high scoring performance and the voting patterns of the ESC can be explained by a complex set of factors. These factors can be divided into three groups; performance factors, competition factors and external factors. Performance factors relate to the performance itself, such as the song and the music. Competition factors relate to the way the competition is run and organised, such as the type of voting method used and the order of appearance for the performers. External factors encompass the social, cultural and political factors that influence voting at the Eurovision. The research presented here focuses on among other factors, whether voting blocs, music factors derived from Echo Nest services and migration patterns can explain the points and voting patterns of the 2016 ESC. The data was stratified into three datasets based on the voting systems; combined vote, televote and jury vote. A multiple linear regression model was fitted to each dataset and the significance of the predictor variables in explaining the response variable Points were evaluated using T-tests. The results showed that both the voting blocs and migration patterns were significant in explaining the scores and voting patterns of the competition. With regards to the music factors, the most successful songs appeared to be more acoustic and less dance orientated.

# Docker

A stable version of the MSc-ESC code and data can be found as a [docker](https://www.docker.com/) image on dockerhub here:

* https://hub.docker.com/repository/docker/oislen/msc-esc/general

The image can be pulled from dockerhub using the following command:

```
docker pull oislen/msc-esc:latest
```

A docker container instance can then be created using the following command and the docker image:

```
docker run --name esc -it oislen/msc-esc:latest
```
