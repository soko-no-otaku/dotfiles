(require 'package)
;; melpa
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; melpa-stable
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;; marmalade
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
;; org
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
;; package initialize
(package-initialize)

;; 言語設定
(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)

;; スタートアップメッセージを消す
(setq inhibit-startup-message t)
;; *scratch* のメッセージを消す
(setq initial-scratch-message nil)
;; ツールバーを消す
(tool-bar-mode -1)

;; 行間幅
(setq line-spacing 0.1)
;; 現在行を目立たせる
(global-hl-line-mode)
;; 行数表示
(global-linum-mode t)
;; 行番号・桁番号をモードラインに表示する・しない設定
(line-number-mode t)   ; 行番号。tなら表示、nilなら非表示
(column-number-mode t) ; 桁番号。tなら表示、nilなら非表示

;; 閉じ括弧の自動挿入
(electric-pair-mode t)
;; 対応する括弧の強調
(show-paren-mode t)

;; 現在時刻の書式
(setq display-time-format "%m/%d %H:%M")
;; モードラインに現在時刻を表示する
(display-time)

;; 外部デバイスとクリップボードを共有
(setq x-select-enable-clipboard t)

;; タブをスペースで扱う
(setq-default indent-tabs-mode nil)
;; タブ幅
(setq default-tab-width 4)
;; 空白文字の可視化
(require 'whitespace)
(setq whitespace-style '(face           ; faceで可視化
                         trailing       ; 行末
                         tabs           ; タブ
                         space-mark     ; 表示のマッピング
                         tab-mark
                         ))
(setq whitespace-display-mappings '((tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
(global-whitespace-mode 1)

;; 自動バックアップしない
(setq make-backup-files nil)
(setq auto-save-default nil)

;; カーソルを縦棒型にする
(setq-default cursor-type 'bar)
;; 透明度
(if window-system
    (set-frame-parameter nil 'alpha 90)
  nil)

;; monokai-theme
(load-theme 'monokai t)

;; yes or no を y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; 警告音もフラッシュも全て無効(警告音が完全に鳴らなくなるので注意)
(setq ring-bell-function 'ignore)

;; フォント設定(よく分かってない)
(set-face-attribute 'default nil :family "Rounded M+ 1m" :height 220)

;; auto-complete
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-modes 'text-mode)         ;; text-modeでも自動的に有効にする
(add-to-list 'ac-modes 'fundamental-mode)  ;; fundamental-mode
(add-to-list 'ac-modes 'org-mode)
(add-to-list 'ac-modes 'yatex-mode)
(ac-set-trigger-key "TAB")
(setq ac-use-menu-map t)       ;; 補完メニュー表示時にC-n/C-pで補完候補選択
(setq ac-use-fuzzy t)          ;; 曖昧マッチ

;; より下に記述した物が PATH の先頭に追加されます
(dolist (dir (list
              "/sbin"
              "/usr/sbin"
              "/bin"
              "/usr/bin"
              "/opt/local/bin"
              "/sw/bin"
              "/usr/local/bin"
              "/Library/TeX/texbin"
              (expand-file-name "~/bin")
              (expand-file-name "~/.emacs.d/bin")
              ))
;; PATH と exec-path に同じ物を追加します
(when (and (file-exists-p dir) (not (member dir exec-path)))
  (setenv "PATH" (concat dir ":" (getenv "PATH")))
  (setq exec-path (append (list dir) exec-path))))

;; ---------------------------------------------------------
;; YaTeX の設定
;; ---------------------------------------------------------

;; Add library path
(add-to-list 'load-path "~/.emacs.d/lisp/yatex")
;; YaTeX mode
(setq auto-mode-alist
      (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq tex-command "platex")
(setq dviprint-command-format "dvipdfmx %s")
;; use Preview.app
(setq dvi2-command "open -a Preview")
(defvar YaTeX-dvi2-command-ext-alist
  '(("xdvi" . ".dvi")
    ("ghostview\\|gv" . ".ps")
    ("acroread\\|pdf\\|Preview\\|open" . ".pdf")))
