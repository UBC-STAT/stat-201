# Configuration file for notebook.

c = get_config()  #noqa

# When a notebook is saved, save a timestamped copy in directory .history. Clear outputs.
import os
import shutil
import time
import glob
import subprocess
from datetime import datetime
from zoneinfo import ZoneInfo

def post_save(model, os_path, contents_manager):
    """post-save hook for saving backups of jupyter files."""

    if model['type'] != 'notebook':
        return # only do this for notebooks

    directory, filename = os.path.split(os_path)
    history_directory = os.path.join(directory, '.history')

    if not os.path.isdir(history_directory):
        os.mkdir(history_directory)

    files = list(filter(os.path.isfile, glob.glob(os.path.join(history_directory, '*'))))
    files.sort(key=lambda x: os.path.getctime(x))

    # If the last backup was taken less than time_since seconds ago, return and do not save copy
    if len(files) > 0:
        time_since = 15*60
        if time.time() - os.stat(files[-1]).st_ctime < time_since:
            return

    now = datetime.now(ZoneInfo('America/Vancouver'))
    backup_path=os.path.join(history_directory,now.strftime("%d_%m_%Y_%H_%M_%S")+"_"+filename)
    shutil.copy2(os_path, backup_path)

    # Clear all backup notebook outputs
    subprocess.call(["jupyter", "nbconvert", "--clear-output", "--inplace", backup_path])

    # Keep num_saved latest saved notebooks
    num_saved = 5 # No less than 1
    if len(files) + 1 > num_saved:
        os.remove(files[0])

c.FileContentsManager.post_save_hook = post_save