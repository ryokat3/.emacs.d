<!-- -*- mode:markdown; code:utf-8 -*- -->

.emacs.d
========

The .emacs.d directory in github.



Usage
-----

Clone https://github.com/wak109/.emacs.d.git to your home directory.

    git clone https://github.com/wak109/.emacs.d.git

.emacs.d directory will be created.


Parameters
----------

* `my-registry-alist`

The association list of hostname and registry file.


Registry File
-------------

The registry file is an XML file that have all personal and/or node-specific
data, e.g. e-mail address, password, host-specific exec-path etc.

It is beneficial to keep elisp clean from such data, to make it generic,
and not to open the personal data in GitHub.


References
----------

[GIT for Beginners (Japanese)]: http://wp.yat-net.com/?p=3874
[GIT Markdown (Japanese)]: http://tokkono.cute.coocan.jp/blog/slow/index.php/programming/markdown-skills-for-github-beginners/
