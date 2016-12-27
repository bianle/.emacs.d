;;------------------------------------------------------------------
;; hexo-mode
;;------------------------------------------------------------------
(require-package 'hexo)
(defun blog ()
  (interactive)
  (hexo "~/git/blog/"))

;;------------------------------------------------------------------
;; live-down
;;------------------------------------------------------------------
(require 'livedown)
(global-set-key (kbd "M-RET") 'livedown-preview)

;; (custom-set-variables
;;  '(livedown-autostart nil) ; automatically open preview when opening markdown files
;;  '(livedown-open t)        ; automatically open the browser window
;;  '(livedown-port 1337)     ; port for livedown server
;;  '(livedown-browser nil))  ; browser to use

;;------------------------------------------------------------------
;; hexo kbd标签
;;------------------------------------------------------------------
(defun hexo-tag-kbd()
  "insert a hexo tag"
  (interactive)
  (setq shortKbdMap '(("cmd" "Command")
                      ("opt" "Option")
                      ("ctl" "Ctrl")
                      ("spc" "Space")
                      ("ent" "Enter")
                      ("alt" "Alt")
                      ))
  (setq ipt (read-from-minibuffer "kbd :"))
  (setq k (nth 1 (assoc ipt shortKbdMap)))
  (insert (concat "{% kbd " (if k k ipt) " %}" ))
  )

;;------------------------------------------------------------------
;; hexo-tag-ruby
;;------------------------------------------------------------------

(defun hexo-tag-ruby()
  "insert a hexo-tag-ruby"
  (interactive)
  (setq base (read-from-minibuffer "Base:"))
  (setq top (read-from-minibuffer "top:"))
  (insert (concat "{% ruby " base "|" top "%}"))
  )

;;------------------------------------------------------------------
;; hexo-more
;;------------------------------------------------------------------

(defun hexo-more()
  "insert more"
  (interactive)
  (insert "<!-- more -->")
  )

;;------------------------------------------------------------------
;; current-datetime
;;------------------------------------------------------------------
(defun current-datetime ()
  "Insert the current date"
  (interactive "*")
  (insert (format-time-string "%Y-%m-%d %H:%M:%S" (current-time)))
  )

;;------------------------------------------------------------------
;; 粘贴图片到七牛服务器
;;------------------------------------------------------------------
(defun paste-image()
  "paste image from clipboard"
  (interactive)
  (setq localBaseDir "~/.qiniu/bianle/")
  (call-process-shell-command (concat "mkdir -p " (concat localBaseDir "$(date +%Y/%m/%d)")) )
  (setq relFilename (format-time-string "%Y/%m/%d/%Y%m%d%H%M%S.png" (current-time)) )
  (setq filename (concat localBaseDir relFilename ))
  (message (concat "/usr/local/bin/pngpaste " filename))
  (call-process-shell-command (concat "/usr/local/bin/pngpaste " filename))
  (call-process-shell-command "~/sh/sync.sh")
  (insert (concat "![](http://7xlbo3.com1.z0.glb.clouddn.com/" relFilename ")"))
  (beginning-of-line)
  (forward-char 2)
  )
;;------------------------------------------------------------------
;; 图片拖拽到七牛服务器
;;------------------------------------------------------------------
(defun md-dnd-func (event)
  (interactive "e")
  (goto-char (nth 1 (event-start event)))
  (x-focus-frame nil)
  (let* ((payload (car (last event)))
         (type (car payload))
         (fname (cadr payload))
         (img-regexp "\\(gif\\|png\\|jp[e]?g\\)\\>")
         (localBaseDir "~/.qiniu/bianle/")
         (relFilename (concat (format-time-string "%Y/%m/%d/" (current-time)) (nth 0 (last (split-string fname "/"))))))
    (cond
     ;; insert image link
     ((and  (eq 'drag-n-drop (car event))
            (eq 'file type)
            (string-match img-regexp fname))
      (call-process-shell-command (concat "mkdir -p " (concat localBaseDir "$(date +%Y/%m/%d)")) )
      (call-process-shell-command (format "cp %s %s" fname (concat localBaseDir "$(date +%Y/%m/%d)" "/") ))
      (call-process-shell-command "~/sh/sync.sh")
      (insert (format "![](http://7xlbo3.com1.z0.glb.clouddn.com/%s)" relFilename))
      (beginning-of-line)
      (forward-char 2)
      ;;(org-display-inline-images t t))
      ;; regular drag and drop on file
      ((eq 'file type)
       (insert (format "[[%s]]\n" fname)))
      (t
       (error "I am not equipped for dnd on %s" payload))))))
(require 'markdown-mode)
(define-key markdown-mode-map (kbd "<drag-n-drop>") 'md-dnd-func)

;;------------------------------------------------------------------
;; org-capture
;;------------------------------------------------------------------
(require 'org-capture)
(setq org-default-notes-file (concat org-directory "~/Documents/org-capture/capture.org"))
(define-key global-map "\C-cc" 'org-capture)

(setq org-capture-templates
      '(("s" "灵感" entry (file+headline "~/Documents/org-capture/spark.org" "灵感")
         "* %?\n  %i\n")
        ("j" "日记" entry (file+datetree "~/Documents/org-capture/journal.org")
         "* %?\n写于： [%<%Y-%m-%d %H:%M:%S>]\n  %i\n")
        ("m" "会议" entry (file+datetree "~/Documents/org-capture/meeting.org")
         "* %?\n写于： [%<%Y-%m-%d %H:%M:%S>]\n  %i\n")))


;;------------------------------------------------------------------
;; rainbow
;;------------------------------------------------------------------
(require-package 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(require-package 'yaml-mode)

;;------------------------------------------------------------------
;; proxy
;;------------------------------------------------------------------
(setq socks-noproxy '("localhost"))
(require 'socks)
(setq erc-server-connect-function 'socks-open-network-stream)

(setq socks-server (list "My socks server" "127.0.0.1" 7000 5))

;;------------------------------------------------------------------
;; 插入当前时间
;;------------------------------------------------------------------
(defun insert-current-time ()
  "Insert the current time"
  (interactive)
  ;;(insert (format-time-string "%Y/%m/%d %H:%M:%S" (current-time))))
  (insert (format-time-string "%Y-%m-%d %H:%M:%S" (current-time))))
(global-set-key "\C-xt" 'insert-current-time)

;;------------------------------------------------------------------
;; 格式化整个文件
;;------------------------------------------------------------------
(defun indent-whole ()
  (interactive)
  (indent-region (point-min) (point-max))
  (message "format successfully"))
;;绑定到 M-s-l 键
(global-set-key (kbd "M-¬") 'indent-whole)

;;------------------------------------------------------------------
;; elpa-mirror
;;------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/site-lisp/elpa-mirror")
(require 'elpa-mirror)
(setq elpamr-default-output-directory "~/Dropbox/myelpa")

;;------------------------------------------------------------------
;; ox-freemind
;;------------------------------------------------------------------
;;(add-to-list 'load-path "~/.emacs.d/site-lisp/ox-freeplane")
;;(require 'ox-freemind)

;;------------------------------------------------------------------
;; keyfreq
;;------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/site-lisp/keyfreq")
(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)



;;
;;http://svn.red-bean.com/repos/main/3bits/mobile_org_3bits.txt
;;

;; This is just an example.  The way you do it might be
;; completely different, and that's fine, as long as you wind up
;; with `org-agenda-files' set usefully.
(setq org-directory "~/Documents/MobileOrg")
(custom-set-variables
 '(org-agenda-files (quote ("~/Documents/MobileOrg/work.org"
                            "~/Documents/MobileOrg/inbox.org"
                            "~/Documents/MobileOrg/hobbies.org"
                            "~/Documents/MobileOrg/people.org"
                            "~/Documents/MobileOrg/private/personal.org"))))

(setq org-mobile-directory "/bl@tunnel.ink:org")
(setq org-mobile-inbox-for-pull (concat org-directory "/index.org"))


;;------------------------------------------------------------------
;; stylus-mode
;;------------------------------------------------------------------
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/stylus-mode"))
(require 'stylus-mode)


(provide 'init-local)
