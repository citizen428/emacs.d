(setq org-startup-indented t)

(require 'org-mac-link-grabber)
(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map (kbd "C-c g") 'omlg-grab-link)))
(setq org-mac-grab-Addressbook-app-p nil)
(setq org-mac-grab-Firefox-app-p nil)
(setq org-mac-grab-Mail-app-p nil)

(setq org-todo-keywords '((sequence "IDEA" "TODO" "WIP" "DELEGATED" "|" "DONE")))

;; LaTeX and PDf output
(require 'org-latex)

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
