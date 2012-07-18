(require 'cl)				; common lisp goodies, loop

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (let (el-get-master-branch)
     (end-of-buffer)
     (eval-print-last-sexp)))))


;; set local recipes
(setq
 el-get-sources
 '((:name buffer-move			; have to add your own keys
	  :after (progn
		   (global-set-key (kbd "<C-S-up>")     'buf-move-up)
		   (global-set-key (kbd "<C-S-down>")   'buf-move-down)
		   (global-set-key (kbd "<C-S-left>")   'buf-move-left)
		   (global-set-key (kbd "<C-S-right>")  'buf-move-right)))

   (:name smex				; a better (ido like) M-x
	  :after (progn
		   (setq smex-save-file "~/.emacs.d/.smex-items")
		   (global-set-key (kbd "M-x") 'smex)
		   (global-set-key (kbd "M-X") 'smex-major-mode-commands)))

   (:name magit				; git meet emacs, and a binding
	  :after (progn
		   (global-set-key (kbd "C-x C-z") 'magit-status)))

   (:name goto-last-change		; move pointer back to last change
	  :after (progn
		   ;; when using AZERTY keyboard, consider C-x C-_
		   (global-set-key (kbd "C-x C-/") 'goto-last-change)))))

;; now set our own packages
(setq
 my:el-get-packages
 '(el-get				; el-get is self-hosting
   markdown-mode                        ; mode for editing markdown-files
   textmate
   escreen            			; screen for emacs, C-\ C-h
   php-mode-improved			; if you're into php...
   switch-window			; takes over C-x o
   ;; auctex
  ;; auto-complete			; complete as you type with overlays
   zenburn-theme))	                ; check out color-theme-solarized

;;
;; Some recipes require extra tools to be installed
;;
;; Note: el-get-install requires git, so we know we have at least that.
;;

;;(when (el-get-executable-find "cvs")
;;  (add-to-list 'my:el-get-packages 'emacs-goodies-el)) ; the debian addons for emacs

(when (el-get-executable-find "svn")
  (loop for p in '(psvn    		; M-x svn-status
		   yasnippet		; powerful snippet mode
		   )
	do (add-to-list 'my:el-get-packages p)))

(setq my:el-get-packages
      (append
       my:el-get-packages
 (loop for src in el-get-sources collect (el-get-source-name src))))

;; install new packages and init already installed packages
(el-get 'sync my:el-get-packages)




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
(global-visual-line-mode 1)

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

(set-frame-font "Bitstream\ Vera\ Sans\ Mono-12")

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
           (add-to-list 'default-frame-alist (cons 'width 90))
           (add-to-list 'default-frame-alist (cons 'width 80)))
    ;; for the height, subtract a couple hundred pixels
    ;; from the screen height (for panels, menubars and
    ;; whatnot), then divide by the height of a char to
    ;; get the height we want
    (add-to-list 'default-frame-alist
         (cons 'height (/ (- (x-display-pixel-height) 80)
                             (frame-char-height)))))))

(set-frame-size-according-to-resolution)

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

(load-theme 'zenburn t)

;;========================================
;; start the emacsserver that listens to emacsclient
(server-start)
