# Copyright (c) UBC-DSCI Development Team.
# Distributed under the terms of the Modified BSD License.
ARG BASE_CONTAINER=ubcstat/r-stat-201:v0.4.0
FROM $BASE_CONTAINER

LABEL maintainer="Tiffany Timbers <tiffany.timbers@gmail.com>"

USER root

# install Rise slide extension
RUN conda install --quiet --yes -c conda-forge rise

RUN pip install SQLAlchemy==1.3

# install nbgrader 
RUN pip install nbgrader
RUN jupyter nbextension install --sys-prefix --py nbgrader --overwrite 
RUN jupyter nbextension enable --sys-prefix --py nbgrader
RUN jupyter serverextension enable --sys-prefix --py nbgrader

RUN pip install "nbconvert<6"

USER $NB_UID
