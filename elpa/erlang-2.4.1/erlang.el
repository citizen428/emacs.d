;;; erlang.el --- Major modes for editing and running Erlang

;; Copyright (C) 1995-1998,2000  Ericsson Telecom AB

;; Author:   Anders Lindgren
;; Version:  2.4.1
;; Keywords: erlang, languages, processes
;; Date:     2000-09-11

;; Lars Thorsýn's modifications of 2000-06-07 included.
 
;; The original version of this package was written by Robert Virding.
;;
;; Most skeletons has been written at Ericsson Telecom by
;; magnus@erix.ericsson.se and janne@erix.ericsson.se

;;; Commentary:

;; Introduction:
;; ------------
;;
;; This package provides support for the programming language Erlang.
;; The package provides an editing mode with lots of bells and
;; whistles, compilation support, and it makes it possible for the
;; user to start Erlang shells that run inside Emacs.
;;
;; See the Erlang distribution for full documentation of this package.

;; Installation:
;; ------------
;;
;; Place this file in Emacs load path, byte-compile it, and add the
;; following line to the appropriate init file:
;;
;;    (require 'erlang-start)
;;
;; The full documentation contains much more extensive description of
;; the installation procedure.

;; Reporting Bugs:
;; --------------
;;
;; Please send bug reports to the following email address:
;;     support@erlang.ericsson.se
;;
;; Please state as exactly as possible:
;;    - Version number of Erlang Mode (see the menu), Emacs, Erlang,
;;	and of any other relevant software.
;;    - What the expected result was.
;;    - What you did, preferably in a repeatable step-by-step form.
;;    - A description of the unexpected result.
;;    - Relevant pieces of Erlang code causing the problem.
;;    - Personal Emacs customisations, if any.
;;
;; Should the Emacs generate an error, please set the emacs variable
;; `debug-on-error' to `t'.  Repeat the error and enclose the debug
;; information in your bug-report.
;;
;; To set the variable you can use the following command:
;;     M-x set-variable RET debug-on-error RET t RET

;;; Code:

;; Variables:

(defconst erlang-version "2.4.1"
  "The version number of Erlang mode.")

(defvar erlang-root-dir nil
  "The directory where the Erlang system is installed.
The name should not contain the ending slash.

Should this variable be nil, no manual pages will show up in the
Erlang mode menu.")

(defvar erlang-menu-items '(erlang-menu-base-items
			    erlang-menu-skel-items
			    erlang-menu-shell-items
			    erlang-menu-compile-items
			    erlang-menu-man-items
			    erlang-menu-personal-items
			    erlang-menu-version-items)
  "*List of menu item list to combine to create Erland mode menu.

External programs which temporary adds menu items to the Erland mode
menu use this variable.  Please use the function `add-hook' to add
items.

Please call the function `erlang-menu-init' after every change to this
variable.")

(defvar erlang-menu-base-items
  '(("Indent"
     (("Indent Line" erlang-indent-command)
      ("Indent Region " erlang-indent-region
       (if erlang-xemacs-p (mark) mark-active))
      ("Indent Clause" erlang-indent-clause)
      ("Indent Function" erlang-indent-function)
      ("Indent Buffer" erlang-indent-current-buffer)))
    ("Edit"
     (("Fill Comment" erlang-fill-paragraph)
      ("Comment Region" comment-region
       (if erlang-xemacs-p (mark) mark-active))
      ("Uncomment Region" erlang-uncomment-region
       (if erlang-xemacs-p (mark) mark-active))
      nil
      ("Beginning of Function" erlang-beginning-of-function)
      ("End of Function" erlang-end-of-function)
      ("Mark Function" erlang-mark-function)
      nil
      ("Beginning of Clause" erlang-beginning-of-clause)
      ("End of Clause" erlang-end-of-clause)
      ("Mark Clause" erlang-mark-clause)
      nil
      ("New Clause" erlang-generate-new-clause)
      ("Clone Arguments" erlang-clone-arguments)))
    ("Syntax Highlighting"
     (("Level 3" erlang-font-lock-level-3)
      ("Level 2" erlang-font-lock-level-2)
      ("Level 1" erlang-font-lock-level-1)
      ("Off" erlang-font-lock-level-0)))
    ("TAGS"
     (("Find Tag" find-tag)
      ("Find Next Tag" erlang-find-next-tag)
					;("Find Regexp" find-tag-regexp)
      ("Complete Word" erlang-complete-tag)
      ("Tags Apropos" tags-apropos)
      ("Search Files" tags-search))))
  "*Description of menu used in Erlang mode.

This variable must be a list. The elements are either nil representing
a horisontal line or a list with two or three elements.  The first is
the name of the menu item, the second is the function to call, or a
submenu, on the same same form as ITEMS.  The third optional argument
is an expression which is evaluated every time the menu is displayed.
Should the expression evaluate to nil the menu item is ghosted.

Example:
    '((\"Func1\" function-one)
      (\"SubItem\"
       ((\"Yellow\" function-yellow)
        (\"Blue\" function-blue)))
      nil
      (\"Region Funtion\" spook-function midnight-variable))

Call the function `erlang-menu-init' after modifying this variable.")

(defvar erlang-menu-shell-items
  '(nil
    ("Shell"
     (("Start New Shell" erlang-shell)
      ("Display Shell"   erlang-shell-display))))
  "*Description of the Shell menu used by Erlang mode.

Please see the documentation of `erlang-menu-base-items'.")

(defvar erlang-menu-compile-items
  '(("Compile"
     (("Compile Buffer" erlang-compile)
      ("Display Result" erlang-compile-display)
      ("Next Error"     erlang-next-error))))
  "*Description of the Compile menu used by Erlang mode.

Please see the documentation of `erlang-menu-base-items'.")

(defvar erlang-menu-version-items
  '(nil
    ("Version" erlang-version))
  "*Description of the version menu used in Erlang mode.")

(defvar erlang-menu-personal-items nil
  "*Description of personal menu items used in Erlang mode.

Please see the variable `erlang-menu-base-items' for a description
of the format.")

(defvar erlang-menu-man-items nil
  "The menu containing man pages.

The format of the menu should be compatible with `erlang-menu-base-items'.
This variable is added to the list of Erlang menus stored in
`erlang-menu-items'.")

(defvar erlang-menu-skel-items '()
  "Description of the menu containing the skeleton entries.
The menu is in the form described by the variable `erlang-menu-base-items'.")

(defvar erlang-mode-hook nil
  "*Functions to run when Erlang mode is activated.

This hook is used to change the behaviour of Erlang mode.  It is
normally used by the user to personalise the programming environment.
When used in a site init file, it could be used to customise Erlang
mode for all users on the system.

The functions added to this hook is runed every time Erlang mode is
started.  See also `erlang-load-hook', a hook which is runed once,
when Erlang mode is loaded into Emacs, and `erlang-shell-mode-hook'
which is run every time a new inferior Erlang shell is started.

To use a hook, create an Emacs lisp function to perform your actions
and add the function to the hook by calling `add-hook'.

The following example binds the key sequence C-c C-c to the command
`erlang-compile' (normally bound to C-c C-k).  The example also
activates Font Lock mode to fontify the buffer and adds a menu
containing all functions defined in the current buffer.

To use the example, copy the following lines to your `~/.emacs' file:

    (add-hook 'erlang-mode-hook 'my-erlang-mode-hook)

    (defun my-erlang-mode-hook ()
      (local-set-key \"\\C-c\\C-c\" 'erlang-compile)
      (if window-system
          (progn
            (setq font-lock-maximum-decoration t)
            (font-lock-mode 1)))
      (if (and window-system (fboundp 'imenu-add-to-menubar))
          (imenu-add-to-menubar \"Imenu\")))")

(defvar erlang-load-hook nil
  "*Functions to run when Erlang mode is loaded.

This hook is used to change the behaviour of Erlang mode.  It is
normally used by the user to personalise the programming environment.
When used in a site init file, it could be used to customize Erlang
mode for all users on the system.

The difference between this hook and `erlang-mode-hook' and
`erlang-shell-mode-hook' is that the functions in this hook
is only called once, when the Erlang mode is loaded into Emacs
the first time.

Natural actions for the functions added to this hook are actions which
only should be performed once, and actions which should be performed
before starting Erlang mode.  For example, a number of variables are
used by Erlang mode before `erlang-mode-hook' is runed.

The following example sets the variable `erlang-root-dir' so that the
manual pages can be retrieved (note that you must set the value of
`erlang-root-dir' to match the loation of Erlang on your system):

    (add-hook 'erlang-load-hook 'my-erlang-load-hook)

    (defun my-erlang-load-hook ()
       (setq erlang-root-dir \"/usr/local/erlang\"))")

(defvar erlang-new-file-hook nil
  "Functions to run when a new Erlang source file is being edited.

A useful function is `tempo-template-erlang-normal-header'.
\(This function only exists when the `tempo' packags is available.)")

(defvar erlang-check-module-name 'ask
  "*Non-nil means check that module name and file name agrees when saving.

If the value of this variable is the atom `ask', the user is
prompted.  If the value is t the source is silently changed.")

(defvar erlang-electric-commands
  '(erlang-electric-comma
    erlang-electric-semicolon
    erlang-electric-gt)
  "*List of activated electric commands.

The list should contain the electric commands which should be active.
Currently, the available electric commands are:
    erlang-electric-comma
    erlang-electric-semicolon
    erlang-electric-gt
    erlang-electric-newline

Should the variable be bound to t, all electric commands
are activated.

To deactivate all electric commands, set this variable to nil.")

(defvar erlang-electric-newline-inhibit t
  "*Set to non-nil to inhibit newline after electric command.

This is useful since a lot of people press return after executing an
electric command.

In order to work, the command must also be in the
list `erlang-electric-newline-inhibit-list'.

Note that commands in this list are required to set the variable
`erlang-electric-newline-inhibit' to nil when the newline shouldn't be
inhibited.")

(defvar erlang-electric-newline-inhibit-list
  '(erlang-electric-semicolon
    erlang-electric-comma
    erlang-electric-gt)
  "*Command which can inhibit the next newline.")

(defvar erlang-electric-semicolon-insert-blank-lines nil
  "*Number of blank lines inserted before header, or nil.

This variable controls the behaviour of `erlang-electric-semicolon'
when a new function header is generated.  When nil, no blank line is
inserted between the current line and the new header.  When bound to a
number it represents the number of blank lines which should be
inserted.")

(defvar erlang-electric-semicolon-criteria
  '(erlang-next-lines-empty-p
    erlang-at-keyword-end-p
    erlang-at-end-of-function-p)
  "*List of functions controlling `erlang-electric-semicolon'.
The functions in this list are called, in order, whenever a semicolon
is typed.  Each function in the list is called with no arguments,
and should return one of the following values:

  nil             -- no determination made, continue checking
  'stop           -- do not create prototype for next line
  (anything else) -- insert prototype, and stop checking

If every function in the list is called with no determination made,
then no prototype is inserted.

The test is performed by the function `erlang-test-criteria-list'.")

(defvar erlang-electric-comma-criteria
  '(erlang-stop-when-inside-argument-list
    erlang-stop-when-at-guard
    erlang-next-lines-empty-p
    erlang-at-keyword-end-p
    erlang-at-end-of-function-p)
  "*List of functions controlling `erlang-electric-comma'.
The functions in this list are called, in order, whenever a comma
is typed.  Each function in the list is called with no arguments,
and should return one of the following values:

  nil             -- no determination made, continue checking
  'stop           -- do not create prototype for next line
  (anything else) -- insert pr