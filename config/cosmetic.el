;; Color-theme
(require 'color-theme)

(load-local-file "lib/color-theme-sanityinc-solarized.el")
(color-theme-sanityinc-solarized-dark)

;; highlight current line
(global-hl-line-mode 1)
;; (set-face-background 'hl-line "#333333")

;; set cursor colour
(set-cursor-color "#777777")

;; make sure ansi colour character escapes are honoured
(ansi-color-for-comint-mode-on)

;; get rid of clutter
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(set-default-font "-unknown-Inconsolata-normal-normal-normal-*-12-*-*-*-m-0-iso8859-15")
