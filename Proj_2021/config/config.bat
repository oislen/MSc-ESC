conda update -n base -c defaults conda -y
conda env remove --name esc
conda create --name esc --clone analytics_base
conda activate esc
conda install -c anaconda xlrd -y
conda install -c conda-forge pyarrow -y
conda env export > C:\Users\User\Documents\GitHub\ESC\config\esc.yml