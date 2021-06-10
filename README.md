# Repository for MSc Dissertation on the Eurovision Song Contest

This repository is divided up into two main subdirectories; MSc-ESC and Proj_2021. The MSc-ESC subdirectory contains the data, serialized models and code used during the original masters dissertation. The Proj_2021 subdirectory contains supplementary code for applying collaborative filtering to historic Eurovision Song Contest voting data.
 
 1. [MSc-ESC](https://github.com/oislen/MSc-ESC/tree/master/MSc_2018)
    1. [data](https://github.com/oislen/MSc-ESC/tree/master/MSc_2018/data)
	2. [models](https://github.com/oislen/MSc-ESC/tree/master/MSc_2018/models)
	3. [scripts](https://github.com/oislen/MSc-ESC/tree/master/MSc_2018/scripts)
3. [Proj_2021](https://github.com/oislen/MSc-ESC/tree/master/Proj_2021)
    1. [config](https://github.com/oislen/MSc-ESC/tree/master/Proj_2021/config)
	2. [scripts](https://github.com/oislen/MSc-ESC/tree/master/Proj_2021/scripts)
 
The final submitted dissertation can be found here on the Technology University Dublin arrow website:
https://arrow.tudublin.ie/scschcomdis/155/

# Abstract

The Eurovision Song Contest (ESC) is an annual international television song competition. Participating countries send a group or individual artist to perform an original song at the competition. The winner is decided by all participating countries using a voting system that incorporates both a public televote and an expert jury vote. Countries are excluded from voting for their entry and the country with the highest score wins. A high scoring performance and the voting patterns of the ESC can be explained by a complex set of factors. These factors can be divided into three groups; performance factors, competition factors and external factors. Performance factors relate to the performance itself, such as the song and the music. Competition factors relate to the way the competition is run and organised, such as the type of voting method used and the order of appearance for the performers. External factors encompass the social, cultural and political factors that influence voting at the Eurovision. The research presented here focuses on among other factors, whether voting blocs, music factors derived from Echo Nest services and migration patterns can explain the points and voting patterns of the 2016 ESC. The data was stratified into three datasets based on the voting systems; combined vote, televote and jury vote. A multiple linear regression model was fitted to each dataset and the significance of the predictor variables in explaining the response variable Points were evaluated using T-tests. The results showed that both the voting blocs and migration patterns were significant in explaining the scores and voting patterns of the competition. With regards to the music factors, the most successful songs appeared to be more acoustic and less dance orientated.