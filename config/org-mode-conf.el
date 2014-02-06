;;; org-mode-conf.el --- org-mode specific config

;; Copyright (C) 2013  Michael Kohl

;; Author: Michael Kohl <citizen428@gmail.com>
;; Keywords: local

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Configuration for org-mode.

;;; Code:

(setq org-startup-indented t)

(require 'org-mac-link)
(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link)))
(setq org-mac-grab-Chrome-app-p nil
      org-mac-grab-Mail-app-p nil
      org-mac-grab-Outlook-app-p nil
      org-mac-grab-Safari-app-p nil
      org-mac-grab-Chrome-app-p nil
      org-mac-grab-Skim-app-p nil)

(setq org-todo-keywords '((sequence "IDEA" "TODO" "WIP" "DELEGATED" "VERIFY" "|" "DONE")))

;; LaTeX and PDf output
(require 'ox-latex)

(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))

(add-to-list 'org-export-latex-classes
             '("book"
               "\\documentclass{book}"
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
(add-to-list 'org-export-latex-classes
             '("koma-article"
               "\\documentclass{scrartcl}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(add-to-list 'org-export-latex-classes
             '("beamer"
               "\\documentclass[11pt]{beamer}\n
               \\mode<{{{beamermode}}}>\n
               \\usetheme{{{{beamertheme}}}}\n
               \\usecolortheme{{{{beamercolortheme}}}}\n
               \\beamertemplateballitem\n
               \\setbeameroption{show notes}
               \\usepackage[utf8]{inputenc}\n
               \\usepackage[T1]{fontenc}\n
               \\usepackage{hyperref}\n
               \\usepackage{color}
               \\usepackage{listings}
               \\lstset{numbers=none,language=[ISO]C++,tabsize=4,
             frame=single,
             basicstyle=\\small,
             showspaces=false,showstringspaces=false,
             showtabs=false,
             keywordstyle=\\color{blue}\\bfseries,
             commentstyle=\\color{red},
             }\n
               \\usepackage{verbatim}\n
               \\institute{{{{beamerinstitute}}}}\n
                \\subject{{{{beamersubject}}}}\n"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\begin{frame}[fragile]\\frametitle{%s}"
                "\\end{frame}"
                "\\begin{frame}[fragile]\\frametitle{%s}"
                "\\end{frame}")))

(setq org-export-latex-listings 'true)

(setq org-latex-to-pdf-process '("texi2dvi --pdf --clean --verbose --batch %f"))
(setq org-export-html-style-include-default nil)

(provide 'org-mode-conf)
;;; org-mode-conf.el ends here
