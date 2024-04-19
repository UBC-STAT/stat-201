# Copyright (c) UBC-STAT Development Team.
# Distributed under the terms of the Modified BSD License.
# TODO: For testing purposes only, change to ubcstat/r-stat-201:latest
FROM ubcstatit/r-stat-201-student:latest

LABEL maintainer="Brian Kim <brian.kim@stat.ubc.ca>"

# install rise slides extension, nbgrader, clean cache
RUN mamba install --quiet --yes \
    'nbgrader=0.8.2' && \
    mamba clean --all -f -y

# re-upgrade jupyter-server (mamba downgrades it)
RUN pip install --upgrade jupyter-server

# Allow all shortcuts
RUN rm -f /home/${NB_USER}/.jupyter/lab/user-settings/\@jupyterlab/shortcuts-extension/shortcuts.jupyterlab-settings

