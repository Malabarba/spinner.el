#+TITLE: spinner.el

Add spinners and progress-bars to the mode-line for ongoing operations.

[[file:some-spinners.gif]]

[[file:all-spinners.gif]]

* Usage

1. Add ~(spinner "VERSION")~ to your package’s dependencies.

2. Call ~(spinner-start)~ and a spinner will be added to the mode-line.

3. Call ~(spinner-stop)~ on the same buffer when you want to remove it.

* Behavior

The default spinner is a line drawing that rotates. You can pass an
argument to ~spinner-start~ to specify which spinner you want. All
possibilities are listed in the ~spinner-types~ variable, but here are
a few examples for you to try:

- ~(spinner-start 'vertical-breathing 10)~
- ~(spinner-start 'minibox)~
- ~(spinner-start 'moon)~
- ~(spinner-start 'triangle)~
