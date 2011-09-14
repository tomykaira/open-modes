Open Modes
==========

What's This?
------------

Open modes make application development more faster.

Three most used actions in development are opening a file, searching a text in the project, and building (or restart the server in web apps).

A programming language (or framework) specific open mode provides easy way to do them.

Requirements
------------

- elscreen (for open a file)
- bundler (for rackup)

This needs [elscreen](http://www.morishima.net/~naoto/elscreen-en/?lang=en) to open a file (always create a new screen).
If you are not using elscreen, change om-anything-c-open-candidate to match your needs.

How to Use
----------

To use `rack-open-mode`, for example, add this to your `.emacs.el`.

    (add-to-list 'load-path "/path/to/this/directory")
    (require 'rack-open-mode)
    (rack-open-activate) ; automatically enable rack-open-mode in a project

rack-open-mode provides three key binds.

- C-c C-r (rack-open-anything) Recursively search the project directory to make anything candidates.
- C-c C-b (rack-open-grep-project) Search the project directory with `rgrep`.
- C-c C-d (rack-open-rackup) (re)Rackup the project's config.ru.  You can see the log in `*rackup*` buffer

How to customize
---------------

`open-mode.el` provides basic functions to implement an open mode for your language and framework.

`rack-open-mode` will be a good example.

Basically, what you have to do is

- defun `rootp` (which receive current path and return whether it is root).
- defvar `ignored` (directories excluded from `rgrep` and anything candidates).
- define a function to build, start server, or upload, like `rack-open-rackup`
- rename `rack-open-`

If you got a good open-mode, please let me know by sending pull request!
