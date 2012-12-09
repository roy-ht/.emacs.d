;; load path 設定
(add-to-list 'load-path "~/.emacs.d/site-lisp" )
(add-to-list 'load-path "~/.emacs.d/auto-install" )

;;; el-get
(add-to-list 'load-path "~/.emacs.d/el-get")
(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (goto-char (point-max))
     (eval-print-last-sexp))))
(el-get 'sync)

(cond
 ((window-system)
  (load "~/.emacs.d/init.app.el")
  (load "~/.emacs.d/init.term.el")))

;; vc gitはoff
(setq vc-handled-backends ())

(require 'jaunte)
(global-set-key (kbd "C-c C-;") 'jaunte)
(setq-default jaunte-hint-unit 'symbol)


;; white spaces
;; (require 'show-wspace)
;; (add-hook 'font-lock-mode-hook 'ws-highlight-hard-hyphens)
;; (add-hook 'font-lock-mode-hook 'ws-highlight-hard-spaces)
;; (add-hook 'font-lock-mode-hook 'ws-highlight-trailing-whitespace)
;; (set-face-background 'ws-trailing-whitespace "dark red")

;; (add-hook 'font-lock-mode-hook 'ws-highlight-tabs)
;; (setq jaspace-highlight-tabs t)
;; (setq jaspace-alternate-jaspace-string "□")
;; あいうえお さしすせそ　　たちつてと

(when (and (>= emacs-major-version 23)
      (require 'whitespace nil t))
  (setq whitespace-style
	'(face
	  ;; tabs spaces newline trailing space-before-tab space-after-tab    
	  ;; space-mark tab-mark newline-mark))
	  tabs spaces trailing space-mark tab-mark))
  (let ((dark (eq 'dark (frame-parameter nil 'background-mode))))
    (set-face-attribute 'whitespace-space nil
			:foreground (if dark "pink4" "azure3")
			:background 'unspecified)
    (set-face-attribute 'whitespace-trailing nil
			:foreground 'unspecified
			:background (if dark "dark red" "red"))
    (set-face-attribute 'whitespace-tab nil
			:foreground (if dark "gray20" "gray80")
			:background 'unspecified
			:strike-through t)
    (set-face-attribute 'whitespace-newline nil
			:foreground (if dark "darkcyan" "darkseagreen")))
  (setq whitespace-space-regexp "\\(　+\\)")
  (setq whitespace-display-mappings
	'(;; (space-mark   ?\     [?\u00B7]     [?.]) ; space - centered dot
      (space-mark   ?\xA0  [?\xA4]  [?_]) ; hard space - currency
	  (space-mark   ?\x8A0 [?\x8A4] [?_]) ; hard space - currency
	  (space-mark   ?\x920 [?\x924] [?_]) ; hard space - currency
	  (space-mark   ?\xE20 [?\xE24] [?_]) ; hard space - currency
	  (space-mark   ?\xF20 [?\xF24] [?_]) ; hard space - currency
	  (space-mark   ?　    [?□]    [?＿]) ; full-width space - square
	  (newline-mark ?\n    [?\xAB ?\n])   ; eol - right quote mark
	  ))
  (setq whitespace-global-modes '(not dired-mode tar-mode))
  (global-whitespace-mode 1))


;; paren 強調
(show-paren-mode t)

(setq show-paren-delay 0)
(setq show-paren-style 'parenthesis)
(set-face-attribute 'show-paren-match-face nil :weight 'extra-bold)

(global-set-key "\M-g" 'goto-line) ;; goto
(setq scroll-step 3) ;; scroll
(setq require-final-newline t) ;; 改行
(setq frame-title-format (format "emacs@%s : %%f" (system-name))) ;; タイトルバー
(setq-default indent-tabs-mode nil) ;; indent設定
(setq-default tab-width 4) ;; tab 幅を 4 に設定

;; カーソル色をIMEのON/OFFで変更  ;; うまくいかない。。。
(eval-after-load "isearch" '(require 'isearch+))  ;; isearch+

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
;; (require 'auto-save-buffers-enhanced)
;; (setq auto-save-buffers-enhanced-interval 3)
;; (auto-save-buffers-enhanced t)

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

(set-language-environment 'Japanese) ; 日本語
(prefer-coding-system 'utf-8-unix) ; utf-8
(global-set-key "\C-h" 'delete-backward-char) ; backspace
(line-number-mode t) ;; 行番号を表示する
(global-linum-mode t) ;; 行番号を表示する
(column-number-mode t) ;; 列番号
(setq inhibit-startup-message t) ;;; 起動時の画面はいらない
(setq use-file-dialog nil) ;;; ファイルオープン時に、ファイルダイアログを表示させずに、ミニバッファにて入力できるようにする
;; 同名バッファを分りやすくする
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")


;; helm
;; ----------------------------------------------------
(require 'helm-config)
(helm-mode 1)

(global-set-key (kbd "C-;") 'helm-mini)
(setq helm-idle-delay 0.1)
(setq helm-input-idle-delay 0)
(setq helm-candidate-number-limit 500)

;; List files in git repos
(defun helm-c-sources-git-project-for (pwd)
  (loop for elt in
        '(("Modified files" . "--modified")
          ("Untracked files" . "--others --exclude-standard")
          ("All controlled files in this project" . nil))
        for title  = (format "%s (%s)" (car elt) pwd)
        for option = (cdr elt)
        for cmd    = (format "git ls-files %s" (or option ""))
        collect
        `((name . ,title)
          (init . (lambda ()
                    (unless (and (not ,option) (helm-candidate-buffer))
                      (with-current-buffer (helm-candidate-buffer 'global)
                        (call-process-shell-command ,cmd nil t nil)))))
          (candidates-in-buffer)
          (type . file))))

(defun helm-git-project-topdir ()
  (file-name-as-directory
   (replace-regexp-in-string
    "\n" ""
    (shell-command-to-string "git rev-parse --show-toplevel"))))

(defun helm-git-project ()
  (interactive)
  (let ((topdir (helm-git-project-topdir)))
    (unless (file-directory-p topdir)
      (error "I'm not in Git Repository!!"))
    (let* ((default-directory topdir)
           (sources (helm-c-sources-git-project-for default-directory)))
      (helm-other-buffer sources "*helm git project*"))))
(define-key global-map (kbd "C-x C-g") 'helm-git-project)

;; helm binding
(global-set-key (kbd "C-M-z")   'helm-resume)
(global-set-key (kbd "C-x C-r") 'helm-recentf)
(global-set-key (kbd "C-x C-c") 'helm-M-x)
(global-set-key (kbd "M-y")     'helm-show-kill-ring)
(global-set-key (kbd "C-x C-i") 'helm-imenu)
(global-set-key (kbd "C-M-s")   'helm-occur)
(global-set-key (kbd "C-x b")   'helm-buffers-list)



;; recentf-ext
(require 'recentf-ext)
(setq recentf-exclude '("/auto-install/" ".recentf" "/repos/" "/elpa/"
                        "\\.mime-example" "\\.ido.last" "COMMIT_EDITMSG"))
(setq recentf-auto-cleanup 10)
(if window-system
    (run-at-time t 600 'recentf-save-list))
(defadvice recentf-save-list (around no-message activate)
  (flet ((write-file (file &optional confirm)
                     (let ((str (buffer-string)))
                       (with-temp-file file
                         (insert str)))))
    ad-do-it))
(recentf-mode 1)

;; moccur
(require 'color-moccur)
(require 'moccur-edit)

;; wdired
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;; undo, redo
(require 'redo+)
(global-set-key (kbd "C-?") 'redo)
(setq undo-limit 600000)
(setq undo-strong-limit 900000)

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


;; expand-region
(require 'expand-region)
(global-set-key (kbd "C-0") 'er/expand-region)
(global-set-key (kbd "C-9") 'er/contract-region) ;; リージョンを狭める
;; transient-mark-modeが nilでは動作しませんので注意
(transient-mark-mode t)
;; js2-mode
(add-to-list 'load-path "~/.emacs.d/js2-mode" )
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; for rst-mode
(setq frame-background-mode 'dark)
;; for coffee mode
(add-to-list 'load-path "~/.emacs.d/site-lisp/coffee-mode")
(require 'coffee-mode)

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

;; for markdown-mode
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(defun markdown-custom () "markdown-mode-hook"
  (setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))
  (setq markdown-command "redcarpet --smarty --parse-tables --parse-fenced_code_blocks --parse-autolink --parse-lax_spacing"))
(add-hook 'markdown-mode-hook '(lambda() (markdown-custom)))

