# Copyright (c) Jupyter Development Team.
# Distributed under the terms of the Modified BSD License.
ARG BASE_CONTAINER=jupyter/r-notebook
FROM $BASE_CONTAINER

LABEL maintainer="Tiffany Timbers <tiffany.timbers@stat.ubc.ca>"

# R packages on conda-forge
RUN conda install --quiet --yes -c conda-forge \
  r-cowplot=1.* \
  r-ggally=1.5* \
  r-gridextra=2.3* \
  r-infer=0.5* \
  r-testthat=2.3* 

RUN Rscript -e "devtools::install_github('UBC-MDS/datateachr@v0.2.1')" \
    && Rscript -e "devtools::install_github('UBC-MDS/taxyvr@0.1.0')"

USER root
RUN pip install git+https://github.com/data-8/nbgitpuller \
    && jupyter serverextension enable --sys-prefix nbgitpuller \
    && jupyter labextension install @jupyterlab/git \
    && pip install jupyterlab-git \
    && jupyter serverextension enable --py jupyterlab_git \
    && pip install jupytext --upgrade \
    && jupyter lab build  
RUN useradd -m -s /bin/bash -N -u 9999 jupyter
USER jupyter
# Configure environment
ENV NB_USER=jupyter \
    NB_UID=9999
ENV HOME=/home/$NB_USER
WORKDIR $HOME
