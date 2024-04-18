# Configuration file for jupyter-server.

c = get_config()  #noqa

#------------------------------------------------------------------------------
# ContentsManager(LoggingConfigurable) configuration
#------------------------------------------------------------------------------
## Base class for serving files and directories.
#  
#      This serves any text or binary file,
#      as well as directories,
#      with special handling for JSON notebook documents.
#  
#      Most APIs take a path argument,
#      which is always an API-style unicode path,
#      and always refers to a directory.
#  
#      - unicode, not url-escaped
#      - '/'-separated
#      - leading and trailing '/' will be stripped
#      - if unspecified, path defaults to '',
#        indicating the root path.

## Allow access to hidden files
#  Default: False
c.ContentsManager.allow_hidden = True

#------------------------------------------------------------------------------
# FileContentsManager(FileManagerMixin, ContentsManager) configuration
#------------------------------------------------------------------------------
## Allow access to hidden files
#  See also: ContentsManager.allow_hidden
c.FileContentsManager.allow_hidden = True

#------------------------------------------------------------------------------
# AsyncContentsManager(ContentsManager) configuration
#------------------------------------------------------------------------------
## Base class for serving files and directories asynchronously.

## Allow access to hidden files
#  See also: ContentsManager.allow_hidden
c.AsyncContentsManager.allow_hidden = True

#------------------------------------------------------------------------------
# AsyncFileContentsManager(FileContentsManager, AsyncFileManagerMixin, AsyncContentsManager) configuration
#------------------------------------------------------------------------------
## Allow access to hidden files
#  See also: ContentsManager.allow_hidden
c.AsyncFileContentsManager.allow_hidden = True
