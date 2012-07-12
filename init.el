;; Randy Ridenour - Emacs Customizations

; derived from ELPA installation
; http://tromey.com/elpa/install.html
(defun eval-url (url)
  (let ((buffer (url-retrieve-synchronously url)))
  (save-excursion
    (set-buffer buffer)
    (goto-char (point-min))
    (re-search-forward "^$" nil 'move)
    (eval-region (point) (point-max))
    (kill-buffer (current-buffer)))))

;; Load ELPA
(add-to-list 'load-path "~/.emacs.d/elpa")

(defun install-elpa ()
  (eval-url "http://tromey.com/elpa/package-install.el"))

(if (require 'package nil t)
    (progn
      ;; Emacs 24+ includes ELPA, but requires some extra setup
      ;; to use the (better) tromey repo
      (if (>= emacs-major-version 24)
          (setq package-archives
                (cons '("tromey" . "http://tromey.com/elpa/")
                package-archives)))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
      (package-initialize))
  (install-elpa))

;; Load el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(defun install-el-get ()
  (eval-url
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"))

(unless (require 'el-get nil t)
  (install-el-get))

; extra recipes for packages unknown to el-get (yet)



; list all packages you want installed
(setq my-el-get-packages
      (append
       '(smex markdown-mode textmate)
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync my-el-get-packages)



(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("bf9d5728e674bde6a112979bd830cc90327850aaaf2e6f3cc4654f077146b406" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(load-theme 'zenburn t)


;; turn on highlighting current line
(global-hl-line-mode 1)

;; Word Wrap
(global-visual-line-mode 1)

(global-linum-mode 1) ; always show line numbers

; sane path
(setq path "/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/X11/bin:/usr/texbin:/Users/Randy/.rvm/bin")
(setenv "PATH" path)

;; Add /opt/local/bin to my path
(setq exec-path (cons "/opt/local/bin" (cons "/usr/local/bin" exec-path)))

;;evil-mode
;;(add-to-list 'load-path "~/.emacs.d/evil")
;;     (require 'evil)
;;     (evil-mode 1)

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq inhibit-startup-message t)

(fset 'yes-or-no-p 'y-or-n-p)

(delete-selection-mode t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode t)
(show-paren-mode t)
(column-number-mode t)
(set-fringe-style -1)
(tooltip-mode -1)
(setq sentence-end-double-space nil)   

(set-frame-font "Menlo-13")

;; recentf stuff
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; use abbrev-mode
(setq default-abbrev-mode t)

(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
  (progn
    ;; use 120 char wide window for largeish displays
    ;; and smaller 80 column windows for smaller displays
    ;; pick whatever numbers make sense for you
    (if (> (x-display-pixel-width) 1280)
           (add-to-list 'default-frame-alist (cons 'width 80))
           (add-to-list 'default-frame-alist (cons 'width 80)))
    ;; for the height, subtract a couple hundred pixels
    ;; from the screen height (for panels, menubars and
    ;; whatnot), then divide by the height of a char to
    ;; get the height we want
    (add-to-list 'default-frame-alist
         (cons 'height (/ (- (x-display-pixel-height) 80)
                             (frame-char-height)))))))

(set-frame-size-according-to-resolution)


(global-visual-line-mode t)

(global-linum-mode 1) 

(setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (ido-mode 1)
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer 'always)
(setq ido-file-extensions-order '(".md" ".org" ".txt" ".tex"))
(setq ido-ignore-extensions t)

;; Add Homebrew-installed packages to load path
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")
;;'(exec-path (quote ("/usr/bin" "/bin" "/usr/sbin" "/sbin" "/usr/local/bin"))))

;; Make mouse scrolling work properly
(defun sd-mousewheel-scroll-up (event)
  "Scroll window under mouse up by five lines."
  (interactive "e")
  (let ((current-window (selected-window)))
    (unwind-protect
        (progn
          (select-window (posn-window (event-start event)))
          (scroll-up 5))
      (select-window current-window))))

(defun sd-mousewheel-scroll-down (event)
  "Scroll window under mouse down by five lines."
  (interactive "e")
  (let ((current-window (selected-window)))
    (unwind-protect
        (progn
          (select-window (posn-window (event-start event)))
          (scroll-down 5))
      (select-window current-window))))

(global-set-key (kbd "<mouse-5>") 'sd-mousewheel-scroll-up)
(global-set-key (kbd "<mouse-4>") 'sd-mousewheel-scroll-down)



;; C-c m previews Markdown files in marked
(defun markdown-preview-file ()
  "run Marked on the current file and revert the buffer"
  (interactive)
  (shell-command
   (format "open -a /Applications/Marked.app %s"
       (shell-quote-argument (buffer-file-name))))
)
(global-set-key "\C-cm" 'markdown-preview-file)

;; Make markdown-mode use multimarkdown
(defun markdown-custom ()
  "markdown-mode-hook"
  (setq markdown-command "multimarkdown"))
(add-hook 'markdown-mode-hook '(lambda() (markdown-custom)))

;; Org-mode suggested key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Highlight the region currently under a mark
(setq-default transient-mark-mode t)

;; Always end files with a newline
(setq require-final-newline t)

;; Don't add newlines past the last newline
(setq next-line-add-newlines nil)



;; C-c C-c : comment region
;; C-c C-u : uncomment region
;; C-c C-g : go to line
;;(global-set-key [(control ?c) (control ?c)] 'comment-region)
;;(global-set-key [(control ?x) (control ?u)] 'uncomment-region)
;;(global-set-key [(control ?c) (control ?g)] 'goto-line)

(defun uncomment-region (beg end &optional arg)
   (interactive "*r\np")
   (comment-region beg end (- arg)))

(global-set-key "\C-c\C-u" 'uncomment-region)

;; Bind file extensions to modes

(setq auto-mode-alist (cons '("\\.log\\'" . fundamental-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.dot\\'" . fundamental-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.tex$" . latex-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.org$" . org-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.markdown" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.text" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))

(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(setq-default ispell-program-name "/usr/local/bin/aspell")

(require 'flyspell)

;; Enable flyspell for text and LaTeX modes
(add-hook 'latex-mode-hook 'flyspell-mode-on)
(add-hook 'text-mode-hook 'flyspell-mode)

;; configure aspell for flyspell
(setq ispell-parser 'tex)
;;(setq ispell-program-name "aspell")
(setq ispell-list-command "list")

;; Store personal ispell dictionary in ~/.emacsfiles
(setq ispell-personal-dictionary "~/Dropbox/emacs/ispell-dictionary")

;; In LaTeX mode, automatically re-fill text
(add-hook 'latex-mode-hook 'auto-fill-mode)
