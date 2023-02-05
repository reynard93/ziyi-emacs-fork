;;; early-init.el --- Emacs pre-initialization config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; package.el
;; disable package.el at startup
(setq package-enable-at-startup nil)
;; don't load from package cache
(setq package-quickstart nil)

;; The default is ~800kB. Now allocating 200 MB.
(setq gc-cons-threshold (* 200 1024 1024))

;; change eln-cache folder place
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
	  (expand-file-name  ".local/eln-cache/" user-emacs-directory))))



;;; UI settings
(scroll-bar-mode -1)
(tool-bar-mode -1)
;; (tooltip-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 10) ;; leave space for both side
(column-number-mode)
(global-display-line-numbers-mode 1)

(setq
 inhibit-startup-message t
 visible-bell t)

(provide 'early-init)
