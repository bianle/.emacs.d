


(require-package 'yasnippet)
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/yasnippet/snippets"
        "~/.emacs.d/yasnippet/mysnippets"
        ))
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)
(add-hook 'text-mode-hook #'yas-minor-mode)
;; below modes does NOT inherit from prog-mode
(add-hook 'cmake-mode-hook #'yas-minor-mode)
(add-hook 'web-mode-hook #'yas-minor-mode)
(add-hook 'scss-mode-hook #'yas-minor-mode)

(provide 'init-yasnippet)

