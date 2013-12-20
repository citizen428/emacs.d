This code allows you to grab either the current selected items, or
the frontmost url in various mac appliations, and insert them as
hyperlinks into the current org-mode document at point.

This code is heavily based on, and indeed incorporates,
org-mac-message.el written by John Wiegley and Christopher
Suckling.

Detailed comments for each application interface are inlined with
the code.  Here is a brief overview of how the code interacts with
each application:

Finder.app - grab links to the selected files in the frontmost window
Mail.app - grab links to the selected messages in the message list
AddressBook.app - Grab links to the selected addressbook Cards
Firefox.app - Grab the url of the frontmost tab in the frontmost window
Vimperator/Firefox.app - Grab the url of the frontmost tab in the frontmost window
Safari.app - Grab the url of the frontmost tab in the frontmost window
Google Chrome.app - Grab the url of the frontmost tab in the frontmost window
Together.app - Grab links to the selected items in the library list
Skim.app - Grab a link to the selected page in the topmost pdf document
Microsoft Outlook.app - Grab a link to the selected message in the message list


Installation:

add (require 'org-mac-link) to your .emacs, and optionally bind a
key to activate the link grabber menu, like this:

(add-hook 'org-mode-hook (lambda ()
  (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link)))

Usage:

Type C-c g (or whatever key you defined, as above), or type M-x
org-mac-grab-link RET to activate the link grabber.  This will present
you with a menu to choose an application from which to grab a link
to insert at point.  You may also type C-g to abort.

Customizing:

You may customize which applications appear in the grab menu by
customizing the group `org-mac-link'.  Changes take effect
immediately.
