(require 'my-func)
(require 'my-python)

(setq evernote-test `(
 "import sys"
 ,(concat "sys.path.append('" my-evernote-lib-dir "')")
 "import hashlib"
 "import binascii"
 "import time"
 "import thrift.protocol.TBinaryProtocol as TBinaryProtocol"
 "import thrift.transport.THttpClient as THttpClient"
 "import evernote.edam.userstore.UserStore as UserStore"
 "import evernote.edam.userstore.constants as UserStoreConstants"
 "import evernote.edam.notestore.NoteStore as NoteStore"
 "import evernote.edam.type.ttypes as Types"
 "import evernote.edam.error.ttypes as Errors"


))

(setq pyproc (my-python-start-console))

(my-python-exec-list pyproc evernote-test)
(my-python-eval pyproc   "userStoreHttpClient = THttpClient.THttpClient('https://sandbox.evernote.com/edma/user')")
(my-python-eval pyproc  "userStoreProtocol = TBinaryProtocol.TBinaryProtocol(userStoreHttpClient)")
(my-python-eval pyproc "userStore = UserStore.Client(userStoreProtocol)")
(my-python-eval pyproc "versionOK = userStore.checkVersion('Evernote EDAMTest (Python)', UserStoreConstants.EDAM_VERSION_MAJOR, UserStoreConstants.EDAM_VERSION_MINOR)")

(my-python-eval pyproc  "print 'Is my Evernote API version up to date? ', str(versionOK)")





(provide 'my-evernote)