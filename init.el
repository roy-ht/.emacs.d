;; load path 設定
(add-to-list 'load-path "~/.emacs.d/site-lisp" )
(add-to-list 'load-path "~/.emacs.d/auto-install" )

(cond
 ((window-system)
  (load "~/.emacs.d/init.app.el")
  (load "~/.emacs.d/init.term.el")))

;; paren 強調
(show-paren-mode t)

(setq show-paren-delay 0)
(setq show-paren-style 'parenthesis)
;; (setq show-paren-style 'mixed)
;; (set-face-background 'show-paren-match-face "#330066")
;; (set-face-foreground 'show-paren-match-face "99ff33")
(set-face-attribute 'show-paren-match-face nil :weight 'extra-bold)
;;(set-face-attribute 'show-paren-match-face nil
;;                    :background nil :foreground nil
;;                    :underline "#ffff00" :weight 'extra-bold)

;; goto
(global-set-key "\M-g" 'goto-line)
;; scroll
(setq scroll-step 3)
;; 改行
(setq require-final-newline t)
;; タイトルバー
(setq frame-title-format (format "emacs@%s : %%f" (system-name)))
;; indent設定
(setq-default indent-tabs-mode nil)
;; tab 幅を 4 に設定
(setq-default tab-width 4)

;; カーソル色をIMEのON/OFFで変更
;; うまくいかない。。。

;; isearch+
(eval-after-load "isearch" '(require 'isearch+))

(setq make-backup-files t)           ;; バックアップファイルをまとめる
(setq backup-directory-alist
  (cons (cons "\\.*$" (expand-file-name "~/.emacs.d/backup"))
    backup-directory-alist))
(setq version-control t)             ;; 複数のバックアップ世代を管理
(setq kept-new-versions 5)           ;; 新しいものをいくつ残すか
(setq kept-old-versions 5)           ;; 古いものをいくつ残すか
(setq delete-old-versions t)         ;; 確認せずに古いものを消す。
(setq vc-make-backup-files t)        ;; バージョン管理下のファイルもバックアップを作る。
(setq auto-save-default nil)         ;; #hoge#ファイルを作らない

;; 次回起動時にファイルの開き具合をそのままにしてくれる
(require 'session)
(setq session-undo-check -1)
(add-hook 'after-init-hook 'session-initialize)

;; auto-save 勝手に保存してくれる
(require 'auto-save-buffers-enhanced)
(setq auto-save-buffers-enhanced-interval 3)
(auto-save-buffers-enhanced t)

;; emacs起動時にパスをport selectしたものにあわせる
(dolist (dir (list
              "/sbin"  
              "/usr/sbin"  
              "/bin"  
              "/usr/bin"  
              "/opt/local/bin"  
              "/sw/bin"  
              "/usr/local/bin"  
	      "/opt/local/Library/Frameworks/Python.framework/Versions/Current/bin"
	      "/Users/hiro/.nvm/v0.6.6/bin"
              (expand-file-name "~/bin")  
              (expand-file-name "~/.emacs.d/bin")  
              ))  
 ;; PATH と exec-path に同じ物を追加します  
 (when (and (file-exists-p dir) (not (member dir exec-path)))  
   (setenv "PATH" (concat dir ":" (getenv "PATH")))  
   (setq exec-path (append (list dir) exec-path))))
(setenv "MANPATH" (concat "/usr/local/man:/usr/share/man:/Developer/usr/share/man:/sw/share/man" (getenv "MANPATH")))

;;; key 設定
;; OptionとCommandキー入れ替え
(setq ns-command-modifier (quote meta))
(setq ns-alternate-modifier (quote super))
;; システムショートカットを優先しない?
(setq mac-pass-control-to-system nil)
(setq mac-pass-command-to-system nil)
(setq mac-pass-option-to-system nil)

; 日本語
(set-language-environment 'Japanese)
; utf-8
(prefer-coding-system 'utf-8-unix)
; backspace
(global-set-key "\C-h" 'delete-backward-char)
;;; 行番号を表示する
(line-number-mode t)
(global-linum-mode t)
;;; 起動時の画面はいらない
(setq inhibit-startup-message t)
;;; ファイルオープン時に、ファイルダイアログを表示させずに、
;;; ミニバッファにて入力できるようにする
(setq use-file-dialog nil)

;; anything
(require 'anything-startup)

(defun my-anything-filelist+ ()
  "Preconfigured `anything' to open files/buffers/bookmarks instantly.
This is a replacement for `anything-for-files'."
  (interactive)
  (anything-other-buffer
   '(anything-c-source-ffap-line
     anything-c-source-ffap-guesser
     anything-c-source-buffers+
     anything-c-source-recentf
     anything-c-source-bookmarks
     anything-c-source-file-cache
;;     anything-c-source-filelist
     anything-c-source-mac-spotlight
     )
   "*anything file list*"))

;; (setq anything-c-filelist-file-name "/Users/tmaeda/tmp/all.filelist")
;; (setq anything-grep-candidates-fast-directory-regexp "^/tmp")
;;;;;;;;;;;;;;;;;;;
;; anything-kill-ring
(defun anything-kill-ring ()
  (interactive)
  (anything 'anything-c-source-kill-ring nil nil nil nil "*anything kill ring*"))

(global-set-key (kbd "M-y") 'anything-kill-ring)
(global-set-key (kbd "C-;") 'my-anything-filelist+)
;; reconf-ext
(require 'recentf-ext)
(recentf-mode t)
(setq recentf-max-saved-items 3000)

;; moccur
(require 'color-moccur)
(require 'moccur-edit)

;; wdired
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;; popwin
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
(push '(dired-mode :position top :noselect t) popwin:special-display-config)

;; undo, redo
(require 'redo+)
(require 'undo-tree)
(global-undo-tree-mode)

;; auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/site-lisp/ac-dict")
(ac-config-default)
(setq ac-auto-start 3)
(setq ac-auto-show-menu 0.7)
(setq ac-dwim t)
(setq ac-stop-flymake-on-completing t)
(define-key ac-mode-map (kbd "C-.") 'auto-complete)
(setq ac-use-menu-map t)
;; デフォルトで設定済み
;; 20行分表示
(setq ac-menu-height 20)

;; js2-mode
(add-to-list 'load-path "~/.emacs.d/js2-mode" )
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;; custom-set-variables was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
;; (custom-set-variables
;;  '(js2-always-indent-assigned-expr-in-decls-p t)
;;  '(js2-auto-indent-p t)
;;  '(js2-enter-indents-newline t)
;;  '(js2-highlight-level 3)
;;  '(js2-indent-on-enter-key t)
;;  '(js2-mirror-mode t))

;; for rst-mode
(setq frame-background-mode 'dark)
;; for coffee mode
(add-to-list 'load-path "~/.emacs.d/site-lisp/coffee-mode")
(require 'coffee-mode)

;; python
(add-hook 'python-mode-hook '(lambda () 
     (define-key python-mode-map "\C-m" 'newline-and-indent)))


;; org-mode
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-default-notes-file  "~/Dropbox/org/inbox.org")
(setq org-todo-keywords '((sequence "TASK(t)"  "STARTED(s)"  "WAITING(w)" "|" "DONE(x)" "CANCEL(c)")
                          (sequence  "APPT(a)" "|" "DONE(x)" "CANCEL(c)")
                          (sequence "NEXT(n)" "|" "DONE(x)")))
(setq org-capture-templates
  '(("t" "Task" entry (file+headline "~/Dropbox/org/task.org" "TaskList")
         "* TASK [%^G]%u %?\n %i\n %a")
    ("c" "CodeReading" entry (file+headline "~/Dropbox/org/codereding.org" "CodeReadingList")
         "* COMMENT [%^G]%U %?\n %i\n %a")
    ("j" "Journal" entry (file+datetree "~/Dropbox/org/journal.org")
         "* [%^G]%U %?\n %i\n %a")
    ("m" "Memo" entry (file+headline "~/Dropbox/org/notes.org" "MemoList")
         "* [%^G]%U %?\n %i")
))
