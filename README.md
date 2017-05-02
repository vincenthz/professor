professor
=========

prof file tool

UI
--

2 different modes depending on parameters:

* normal : 1 profile file
* experimental diff mode: 2 profile files

keybindings:

* 'esc' : quit the application
* up/down arrow: move in the tree view
* page up/down: move in the tree view by 80 entries
* left/right arrow: collapse / expand the tree node
* '-' / '+' : change important threshold
* '[' / ']' : pop / push context
* 'i' : toggle between individual / inherited costs

CLI
---

Various utilities to process prof files

* stats baseDir
* report baseDir leftmost
* diff baseDir runDir

UI Example
----------

![UI](https://raw.githubusercontent.com/vincenthz/professor/master/examples/screenshot.png)
