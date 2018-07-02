
;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.

(let ((minver "23.3"))
  (when (version<= emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version<= emacs-version "24")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

;;----------------------------------------------------------------------------
;; Temporarily reduce garbage collection during startup
;;----------------------------------------------------------------------------
(defconst sanityinc/initial-gc-cons-threshold gc-cons-threshold
  "Initial value of `gc-cons-threshold' at start-up time.")
(setq gc-cons-threshold (* 128 1024 1024))
(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold sanityinc/initial-gc-cons-threshold)))

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(require 'init-compat) ;;compat eamcs v23
(require 'init-utils)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
;; Calls (package-initialize)
;;(require 'init-elpa)      ;; Machinery for installing required packages
(require 'init-elpa-cn)
(require 'init-exec-path) ;; Set up $PATH

;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-preload-local.el"
;;----------------------------------------------------------------------------
(require 'init-preload-local nil t)

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------

(require-package 'wgrep);wgrep allows you to edit a grep buffer and apply those changes to the file buffer.
(require-package 'project-local-variables);Set project-local variables from a file.
(require-package 'diminish);Diminished modes are minor modes with no modeline display
(require-package 'scratch);Mode-specific scratch buffers
(require-package 'mwe-log-commands);log keyboard commands to buffer

;;(require 'init-frame-hooks)
;;(require 'init-xterm)
;;(require 'init-themes)
(require 'init-osx-keys)
(require 'init-gui-frames)
(require 'init-dired);; require `init-editing-utils`
(require 'init-isearch)
(require 'init-grep)
(require 'init-uniquify);;make the buffer names distinguishable.
(require 'init-ibuffer)
(require 'init-flycheck)

(require 'init-recentf)
(require 'init-smex);M-x interface with Ido-style fuzzy matching.
;;;; If you really prefer ido to ivy, change the comments below. I will
;;;; likely remove the ido config in due course, though.
;;;; (require 'init-ido)
(require 'init-ivy);Incremental Vertical completYon
(require 'init-hippie-expand);;ac
(require 'init-company)
(require 'init-windows)
(require 'init-sessions)
(require 'init-fonts)
;;(require 'init-mmm)
;;
(require 'init-editing-utils);;avy,expand-region,guide-key,page-break-lines in it
(require 'init-whitespace)
(require 'init-fci)
;;
(require 'init-vc)
;;(require 'init-darcs)
(require 'init-git)
(require 'init-github)
;;
(require 'init-projectile);;Project Interaction Library for Emacs
;;
;;(require 'init-compile)
(require 'init-crontab)
;;(require 'init-textile)
(require 'init-markdown)
;;(require 'init-csv)
;;(require 'init-erlang)
(require 'init-javascript)
;;(require 'init-php)
(require 'init-org)
;;(require 'init-nxml)
(require 'init-html)
(require 'init-css)
;;(require 'init-haml)
(require 'init-python-mode)
;;(unless (version<= emacs-version "24.3")
;;  (require 'init-haskell))
;;(require 'init-elm)
;;(require 'init-ruby-mode)
;;(require 'init-rails)
;;(require 'init-sql)
;;
(require 'init-paredit)
(require 'init-lisp)
(require 'init-slime)
;;(unless (version<= emacs-version "24.2")
;;  (require 'init-clojure)
;;  (require 'init-clojure-cider))
;;(require 'init-common-lisp)
;;
;;(when *spell-check-support-enabled*
;;  (require 'init-spelling))
;;
;;(require 'init-misc)
;;
;;(require 'init-folding)
(require 'init-dash)
;;(require 'init-ledger)
;;;; Extra packages which don't require any configuration
;;
;;(require-package 'gnuplot)
(require-package 'lua-mode)
;;(require-package 'htmlize)
;;(require-package 'dsvn)

(when *is-a-mac*
  (require-package 'osx-location))
(require-package 'regex-tool)

(require 'init-yasnippet)

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(require 'server)
(unless (server-running-p)
  (server-start))

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(when (file-exists-p custom-file)
  (load custom-file))

;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-local" containing personal settings
;;----------------------------------------------------------------------------
(require 'init-local nil t)

;;----------------------------------------------------------------------------
;; Locales (setting them earlier in this file doesn't work in X)
;;----------------------------------------------------------------------------
(require 'init-locales)


(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:


