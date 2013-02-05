(setq package-archives '(("ELPA" . "http://tromey.com/elpa/") 
                          ("gnu" . "http://elpa.gnu.org/packages/")
                          ("marmalade" . "http://marmalade-repo.org/packages/")))

;; ido-mode
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (ido-mode 1)
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer 'always)
(setq ido-file-extensions-order '(".md" ".org" ".txt" ".tex"))
(setq ido-ignore-extensions t)

;; Setup SMEX
(add-to-list 'load-path "~/.emacs.d/elpa/smex-2.0/")
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(add-to-list 'custom-theme-load-path "~/.emacs.d/elpa/zenburn-theme-1.5")
(load-theme 'zenburn t)



;; Font-face setup. Check the availability of a some default fonts, in
;; order of preference. The first of these alternatives to be found is
;; set as the default font, together with base size and fg/bg
;; colors. If none of the preferred fonts is found, nothing happens
;; and Emacs carries on with the default setup. We do this here to
;; prevent some of the irritating flickering and resizing that
;; otherwise goes on during startup. You can reorder or replace the
;; options here with the names of your preferred choices.

(defun font-existsp (font)
  "Check to see if the named FONT is available."
  (if (null (x-list-fonts font))
      nil t))

;; Set default font. First one found is selected.
(cond
 ((eq window-system nil) nil)
 ((font-existsp "PragmataPro")
  (set-face-attribute 'default nil :height 140 :font "PragmataPro"))
 ((font-existsp "Menlo")
  (set-face-attribute 'default nil :height 140 :font "Menlo"))
 ((font-existsp "Consolas")
  (set-face-attribute 'default nil :height 140 :font "Consolas"))
 ((font-existsp "Inconsolata")
  (set-face-attribute 'default nil :height 140 :font "Inconsolata"))
 )


;;(load-theme 'solarized-dark t)

(global-visual-line-mode t)
(blink-cursor-mode t)
(setq sentence-end-double-space nil)

(add-to-list 'load-path "~/.emacs.d/elpa/yasnippet-0.8.0/")
(require 'yasnippet)
(yas-global-mode 1)

(add-to-list 'load-path "~/.emacs.d/elpa/markdown-mode-1.9/")
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(defun markdown-unset-tab ()
  "markdown-mode-hook"
  (define-key markdown-mode-map (kbd "<tab>") nil))
(add-hook 'markdown-mode-hook '(lambda() (markdown-unset-tab)))

; sane path
(setq path "/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/X11/bin:/usr/texbin:/Users/Randy/.rvm/bin")
(setenv "PATH" path)

;; Add /opt/local/bin to my path
(setq exec-path (cons "/opt/local/bin" (cons "/usr/local/bin" exec-path)))

;; Add Homebrew-installed packages to load path
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")
;;'(exec-path (quote ("/usr/bin" "/bin" "/usr/sbin" "/sbin" "/usr/local/bin"))))

;; turn on highlighting current line
(global-hl-line-mode 1)

;; Word Wrap
;;(global-visual-line-mode 1)

(global-linum-mode 1) ; always show line numbers


;; Fix line numbers - removes graphics glitch
(setq linum-format " %d ")


(setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (ido-mode 1)
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer 'always)
(setq ido-file-extensions-order '(".md" ".org" ".txt" ".tex"))
(setq ido-ignore-extensions t)

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq-default tab-width 4)
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

;;(set-frame-font "Bitstream\ Vera\ Sans\ Mono-12")

;; recentf stuff
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; use abbrev-mode
(setq default-abbrev-mode t)


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

;; ignore byte-compile warnings
;; (setq byte-compile-warnings '(not nresolved
;;                                   free-vars
;;                                   callargs
;;                                   redefine
;;                                   obsolete
;;                                   noruntime
;;                                   cl-functions
;;                                   interactive-only
;;                                  ))

;;(load-theme 'zenburn t)

;;========================================
;; start the emacsserver that listens to emacsclient
(server-start)

