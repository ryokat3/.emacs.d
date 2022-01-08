<!-- -*- mode:gfm; code:utf-8 -*- -->

.emacs.d
========

The .emacs.d directory



Usage
-----

1. Clone https://github.com/ryokat3/.emacs.d.git to your home directory.


   ```
   git clone https://github.com/ryokat3/.emacs.d.git
   ```

   .emacs.d directory will be created.


2. (OPTIONAL) Create the registry file (~/.registry.xml) for your
   customization.

   Currently init.el supports those parameters below.

   + `emacs.exec-path` :: add paths to `exec-path` list



Registry File
-------------

The registry file is an XML file that have all personal and/or node-specific
data, e.g. e-mail address, password, host-specific exec-path etc.


If the registry file is like the example below,


```xml
<registry>
  <my-name>My Name</my-name>
  <my-email>user@example.com</my-email>
  <emacs>
    <exec-path>/path/foo</exec-path>
    <exec-path>/path/bar</exec-path>
  </emacs>
</registry>
```

then it's equivalent to evaluate the emacs lisp below.


```
(setq my-name "My Name")
(setq my-email "user@example.com")
(setq emacs.exec-path '("/path/foo" "/path/bar"))
```

The top level tag will be ignored to create the symbol name.

The default registry file is "~/.registry.xml".

The advantage to use XML instead of elisp for setting variables is that
other applications (e.g. xmllint) can parse the XML and extract the
value. For example,


```
xmllint -xpath "//my-email/text()" ~/.registry.xml
```


Parameters
----------

* `my-registry-alist`
  The association list of hostname and registry file.



References
----------

- [Project Page](https://github.com/ryokat3/.emacs.d)
- [Mastering Markdown](https://guides.github.com/features/mastering-markdown/)
- [GitHubを使ってみよう！導入と簡単な流れ、よく使うコマンドなど。](http://wp.yat-net.com/?p=3874)
- [脱GitHub初心者を目指す人のREADMEマークダウン使いこなし術](http://tokkono.cute.coocan.jp/blog/slow/index.php/programming/markdown-skills-for-github-beginners/) 
- [fakecygpty](https://github.com/d5884/fakecygpty)  
