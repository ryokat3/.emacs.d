<!-- -*- mode:gfm; code:utf-8 -*- -->

.emacs.d
========

The .emacs.d directory in github.



Usage
-----

1. Clone https://github.com/wak109/.emacs.d.git to your home directory.

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


*** Path

The default registry file is "~/.registry.xml".


*** Usage

If the registry file is like the example below,


```xml
<registry>
  <mew-name>Mew Name</mew-name>
  <mew-user>user@example.com</mew-user>
  <emacs>
    <exec-path>/path/foo</exec-path>
    <exec-path>/path/bar</exec-path>
  </emacs>
</registry>
```

then it's equivalent to evaluate the emacs lisp below.


```
(setq mew-name "Mew Name")
(setq mew-user "user@example.com")
(setq emacs.exec-path '("/path/foo" "/path/bar"))
```

The top level tag will be ignored to create the symbol name.


References
----------

- [Mastering Markdown](https://guides.github.com/features/mastering-markdown/)
- [GitHub Flavored Markdown](https://help.github.com/articles/github-flavored-markdown/)
- [A tutorial for GitHub beginners (Japanese)](http://wp.yat-net.com/?p=3874)
- [A tutorial for GitHub README.md and Wiki (Japanese)](http://tokkono.cute.coocan.jp/blog/slow/index.php/programming/markdown-skills-for-github-beginners/) 
  
