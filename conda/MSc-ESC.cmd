:: list all available conda environments
call conda env list

:: create and activate new R environment
call conda env remove --name MSc-ESC --yes
call conda env list
call conda create -n MSc-ESC r-base --yes
call conda activate MSc-ESC
call conda list

:: install all relevant R libraries
call conda config --add channels r
call conda install -c r --file ..\requirements.txt --yes