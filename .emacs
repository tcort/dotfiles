;; A LISP program to configure GNU emacs 24 or later.
;;
;; jshint, tern, and ispell should be installed too.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Me!
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq user-full-name "Thomas Cort")
(setq user-mail-address "linuxgeek@gmail.com")
(setq user-website "https://www.tomcort.com/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Startup!
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; on my Mac, /usr/local/bin isn't in the exec-path
;; this is needed for ispell, jshint, etc.
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; disable all the bars
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; start maximized, the default size is altogether too small.
(toggle-frame-maximized)

;; without the startup message
(setq inhibit-startup-message t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Behavioural Changes
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make page up / page down move the cursor all
;; the way to the top or the bottom of the buffer
(setq scroll-error-top-bottom t)

;; No tabs! Use C-q <tab> if you need a tab
;; use M-x untabify to do tabs2spaces
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(setq js-indent-level 4)
(setq js2-indent-level 4)
(setq javascript-indent-level 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Backups!
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq backup-directory-alist `(("." . "~/.emacs.saves"))
      make-backup-files t
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 10
      kept-old-versions 10
      version-control t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Remote Editing!
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; use ssh by default (quicker than scp)
(setq tramp-default-method "ssh")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Spell Checking!
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; enable spell checking for text files
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

;; enable spell checking for code comments in C, C++, and JavaScript
(dolist (hook '(c-mode-hook c++-mode-hook js-mode-hook))
  (add-hook hook (lambda () (flyspell-prog-mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Packages!
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

;; This function is from Stack Overflow:
;; http://stackoverflow.com/questions/10092322/how-to-automatically-install-emacs-packages-by-specifying-a-list-of-package-name/10095853#10095853
;; and was post by RNA http://stackoverflow.com/users/489564/rna
;; SO content is licensed under CC-BY-SA 3.0 with attribution required http://blog.stackoverflow.com/2009/06/attribution-required/
(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))

;; Make sure to have downloaded archive description.
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; Activate installed packages
(package-initialize)

;; instruct emacs to download an execute arbitrary programs from the internet on startup.
(ensure-package-installed 'distinguished-theme
			  'flycheck
			  'magit
			  'markdown-mode
			  'powerline
			  'tern
			  'tern-auto-complete)

;;
;; distinguished-theme
;;
(load-theme 'distinguished t)

;;
;; flycheck
;;
(dolist (hook '(js-mode-hook))
  (add-hook hook (lambda () (flycheck-mode t))))

;;
;; powerline
;;
(powerline-default-theme)

;;
;; tern
;;
(dolist (hook '(js-mode-hook))
  (add-hook hook (lambda () (tern-mode t))))

;;
;; tern-auto-complete
;;
(eval-after-load 'tern
  '(progn (tern-ac-setup) (auto-complete-mode)))

