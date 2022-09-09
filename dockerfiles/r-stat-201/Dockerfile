# Copyright (c) UBC-DSCI Development Team.
# Distributed under the terms of the Modified BSD License.
FROM jupyter/r-notebook

LABEL maintainer="Tiffany Timbers <tiffany.timbers@gmail.com>"

USER root

# install vim
RUN apt-get update && apt-get install -y vim

USER ${NB_UID}

WORKDIR "${HOME}"

# remove the "work/" directory added in an earlier layer...
RUN rm -rf work

# Install R packages
RUN mamba install --quiet --yes \
	'r-cowplot' \
	'r-ggally' \
	'r-gridextra' \
	'r-infer' \
	'r-kknn' \
	'r-testthat' \
	'r-rpostgres' \
	'r-memoise' \
	&& mamba clean --all -f -y \
	&& fix-permissions "${CONDA_DIR}" \
	&& fix-permissions "/home/${NB_USER}" \
	&& Rscript -e "remotes::install_github('allisonhorst/palmerpenguins@v0.1.0')" \
	&& Rscript -e "install.packages('ISLR', repos='http://cran.us.r-project.org')" \
	&& Rscript -e "devtools::install_github('UBC-MDS/datateachr@v0.2.1')" \
	&& Rscript -e "devtools::install_github('UBC-MDS/taxyvr@0.1.0')" 

# Install nbgitpuller, jupytext, jlab-git
RUN pip install git+https://github.com/data-8/nbgitpuller@c23461d4f7ce47ec4e0d2bcfc3b2fde4939b80f8 \
  && jupyter serverextension enable --sys-prefix nbgitpuller \
  && pip install jupyterlab-git \
  && pip install jupytext --upgrade \
  && jupyter labextension install @techrah/text-shortcuts \
  && jupyter lab build  

# Install the extension to remove the shift+M merge shortcut 
RUN mkdir -p /home/${NB_USER}/.jupyter/lab/user-settings/\@jupyterlab/shortcuts-extension
COPY rm-merge-shortcut.py /home/${NB_USER}/.jupyter/lab/user-settings/\@jupyterlab/shortcuts-extension/shortcuts.jupyterlab-settings
