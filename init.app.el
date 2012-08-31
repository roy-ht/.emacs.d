;;;サーバ起動
(server-start)
;;;クライアントを終了するとき終了するかどうかを聞かない
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;; font 設定
(setq mac-allow-anti-aliasing t) ;; アンチエイリアス
;; フォントセットを作る
(let* ((fontset-name "myfonts") ; フォントセットの名前
       (size 14) ; ASCIIフォントのサイズ [9/10/12/14/15/17/19/20/...]
       (asciifont "Menlo") ; ASCIIフォント
       (jpfont "Hiragino Maru Gothic ProN") ; 日本語フォント
       (font (format "%s-%d:weight=normal:slant=normal" asciifont size))
       (fontspec (font-spec :family asciifont))
       (jp-fontspec (font-spec :family jpfont)) 
       (fsn (create-fontset-from-ascii-font font nil fontset-name)))
  (set-fontset-font fsn 'japanese-jisx0213.2004-1 jp-fontspec)
  (set-fontset-font fsn 'japanese-jisx0213-2 jp-fontspec)
  (set-fontset-font fsn 'katakana-jisx0201 jp-fontspec) ; 半角カナ
  (set-fontset-font fsn '(#x0080 . #x024F) fontspec) ; 分音符付きラテン
  (set-fontset-font fsn '(#x0370 . #x03FF) fontspec) ; ギリシャ文字
  )
;; デフォルトのフレームパラメータでフォントセットを指定
(add-to-list 'default-frame-alist '(font . "fontset-myfonts"))
;; フォントサイズの比を設定
(dolist (elt '(("^-apple-hiragino.*" . 1.2)
		 (".*osaka-bold.*" . 1.2)
		 (".*osaka-medium.*" . 1.2)
		 (".*courier-bold-.*-mac-roman" . 1.0)
		 (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
		 (".*monaco-bold-.*-mac-roman" . 0.9)))
    (add-to-list 'face-font-rescale-alist elt))
;; デフォルトフェイスにフォントセットを設定
;; # これは起動時に default-frame-alist に従ったフレームが
;; # 作成されない現象への対処
(set-face-font 'default "fontset-myfonts")

;; 透明度
(set-frame-parameter (selected-frame) 'alpha '(85 60))

;; テーマ読み込み
(add-to-list 'load-path "/opt/local/share/emacs/site-lisp/color-theme-6.6.0")
 (require 'color-theme)
 (eval-after-load "color-theme"
 	'(progn
 		(color-theme-initialize)
 		(color-theme-clarity)))
 
;;; 起動時のツールバーを消す
(tool-bar-mode -1)

;;; windowサイズ
(set-frame-size
   (selected-frame)
   120  ; 横幅
   (- (/ (- (x-display-pixel-height) 22) (frame-char-height)) 1))  ; 縦幅は画面に合わせて最大化

;; auto-install.el
(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/auto-install/")
(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup) ; 互換性確保

;; yasnippet
(add-to-list 'load-path
              "~/.emacs.d/site-lisp/yasnippet-0.6.1c")
(require 'yasnippet)
;;  (setq yas/trigger-key nil)
(setq yas/trigger-key "C-,")
;; コメントやリテラルではスニペットを展開しない
(setq yas/buffer-local-condition
      '(or (not (or (string= "font-lock-comment-face"
                             (get-char-property (point) 'face))
                    (string= "font-lock-string-face"
                             (get-char-property (point) 'face))))
           '(require-snippet-condition . force-in-comment)))
(yas/load-directory "~/.emacs.d/site-lisp/yasnippet-0.6.1c/snippets")
(require 'dropdown-list)
(setq yas/prompt-functions '(yas/dropdown-prompt))
(yas/initialize)


;; flymake definition
(when (load "flymake" t)
  ;; flymake python
  (defun flymake-pyflakes-init () 
    (let* ((temp-file (flymake-init-create-temp-buffer-copy 
		       'flymake-create-temp-with-folder-structure)) 
	   (local-file (file-relative-name 
			temp-file 
			(file-name-directory buffer-file-name)))) 
      (list "pyflakes" (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks 
	       '("\\.py\\'" flymake-pyflakes-init)) 
  
  ;; FlymakeHtml
  ;; http://www.emacswiki.org/emacs/FlymakeHtml
  (delete '("\\.html?\\'" flymake-xml-init) flymake-allowed-file-name-masks)
  (defun flymake-html-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-with-folder-structure))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      ;; (list "tidy" (list local-file))))
      (list "tidy" (list "-utf8" local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.html$\\|\\.ctp" flymake-html-init))
  (add-to-list 'flymake-err-line-patterns
               '("line \\([0-9]+\\) column \\([0-9]+\\) - \\(Warning\\|Error\\): \\(.*\\)"
                 nil 1 2 4))
  (add-hook 'html-mode-hook '(lambda () (flymake-mode t)))
  (add-hook 'nxml-mode-hook '(lambda () (flymake-mode t)))
  ;; flymake coffee
  (require 'flymake-coffee)
  (add-hook 'coffee-mode-hook 'flymake-coffee-load)

  ;;;;;;;;;;;;;;;;;;
  ;; error avoidance
  ;; http://d.hatena.ne.jp/sugyan/20100705/1278306885
  (defadvice flymake-post-syntax-check
    (before flymake-force-check-was-interrupted)
    (setq flymake-check-was-interrupted t))
  (ad-activate 'flymake-post-syntax-check)
  ;; option
  (setq flymake-gui-warnings-enabled t)
)

;; flymake
(require 'flymake-cursor) ;; tooltipを出すように直接書き換え
(require 'flymake-extension)
(add-hook 'find-file-hook 'flymake-find-file-hook)

;; (setq flymake-extension-use-showtip t)
;; (setq flymake-extension-auto-show t)
(global-set-key "\C-cff" 'flymake-goto-next-error)
(global-set-key "\C-cfb" 'flymake-goto-prev-error)

(require 'rfringe)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((((class color)) (:italic t :underline "red" :weight extra-bold))))
 '(flymake-warnline ((((class color)) (:italic t :underline "violet" :weight extra-bold)))))
