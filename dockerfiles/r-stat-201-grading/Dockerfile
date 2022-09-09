

# Copyright (c) UBC-STAT Development Team.
# Distributed under the terms of the Modified BSD License.
FROM ubcstat/r-stat-201:latest

LABEL maintainer="Tiffany Timbers <tiffany.timbers@stat.ubc.ca>"

# install the rise slides extension
RUN mamba install --quiet --yes "rise"

# install nbgrader
RUN mamba install --quiet --yes "nbgrader=0.7.1" \
    && jupyter nbextension install --sys-prefix --py nbgrader --overwrite \
    && jupyter nbextension enable --sys-prefix --py nbgrader \
    && jupyter serverextension enable --sys-prefix --py nbgrader 

# Clean the cache
RUN mamba clean --all -f -y 
