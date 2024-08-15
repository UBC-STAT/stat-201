# Copyright (c) UBC-STAT Development Team.
# Distributed under the terms of the Modified BSD License.
FROM ubcstat/r-stat-201:latest

LABEL maintainer="Brian Kim <brian.kim@stat.ubc.ca>"

# install rise slides extension, nbgrader, clean cache
RUN pip install --no-cache nbgrader

# Allow all shortcuts
RUN rm -f /home/${NB_USER}/.jupyter/lab/user-settings/\@jupyterlab/shortcuts-extension/shortcuts.jupyterlab-settings

