;; yatex
;; https://oku.edu.mie-u.ac.jp/~okumura/texwiki/?YaTeX
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq auto-mode-alist
      (append '(("\\.tex$" . yatex-mode)
                ("\\.ltx$" . yatex-mode)
                ("\\.cls$" . yatex-mode)
                ("\\.sty$" . yatex-mode)
                ("\\.clo$" . yatex-mode)
                ("\\.bbl$" . yatex-mode)) auto-mode-alist))
(setq YaTeX-inhibit-prefix-letter t)
(setq YaTeX-kanji-code nil)
(setq YaTeX-latex-message-code 'utf-8)
(setq YaTeX-use-LaTeX2e t)
(setq YaTeX-use-AMS-LaTeX t)
(setq YaTeX-dvi2-command-ext-alist
      '(("TeXworks\\|texworks\\|texstudio\\|mupdf\\|SumatraPDF\\|Preview\\|Skim\\|TeXShop\\|evince\\|okular\\|zathura\\|qpdfview\\|Firefox\\|firefox\\|chrome\\|chromium\\|Adobe\\|Acrobat\\|AcroRd32\\|acroread\\|pdfopen\\|xdg-open\\|open\\|start" . ".pdf")))
(setq tex-command "latexmk -e '$latex=q/uplatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -e '$dvipdf=q/dvipdfmx %O -o %D %S/' -norc -gg -pdfdvi")
(setq bibtex-command "latexmk -e '$latex=q/uplatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -e '$dvipdf=q/dvipdfmx %O -o %D %S/' -norc -gg -pdfdvi")
(setq makeindex-command  "latexmk -e '$latex=q/uplatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -e '$dvipdf=q/dvipdfmx %O -o %D %S/' -norc -gg -pdfdvi")
(setq dvi2-command "evince")
(setq tex-pdfview-command "evince")
(setq dviprint-command-format "xdg-open `echo %s | sed -e \"s/\\.[^.]*$/\\.pdf/\"`")

(custom-set-variables
 '(safe-local-variable-values (quote ((TeX-master . t)))))

(require 'dbus)

(defun un-urlify (fname-or-url)
  "A trivial function that replaces a prefix of file:/// with just /."
  (if (string= (substring fname-or-url 0 8) "file:///")
      (substring fname-or-url 7)
    fname-or-url))

(defun evince-inverse-search (file linecol &rest ignored)
  (let* ((fname (un-urlify file))
         (buf (find-file fname))
         (line (car linecol))
         (col (cadr linecol)))
    (if (null buf)
        (message "[Synctex]: %s is not opened..." fname)
      (switch-to-buffer buf)
      (goto-line (car linecol))
      (unless (= col -1)
        (move-to-column col)))))

(dbus-register-signal
 :session nil "/org/gnome/evince/Window/0"
 "org.gnome.evince.Window" "SyncSource"
 'evince-inverse-search)

;; disable some functions
(add-hook 'yatex-mode-hook
          '(lambda ()
             (auto-fill-mode -1)
             (electric-indent-local-mode -1)
             (setq indent-tabs-mode nil)
             (setq YaTeX-environment-indent 0)))

;; load unsafe local variable without confirmation
(put 'TeX-master 'safe-local-variable #'stringp)
(put 'YaTeX-parent-file 'safe-local-variable #'stringp)

;;----------------------------------------------------------------

;; RefTeX with YaTeX
;(add-hook 'yatex-mode-hook 'turn-on-reftex)
(add-hook 'yatex-mode-hook
          '(lambda ()
             (reftex-mode 1)
             (define-key reftex-mode-map (concat YaTeX-prefix ">") 'YaTeX-comment-region)
             (define-key reftex-mode-map (concat YaTeX-prefix "<") 'YaTeX-uncomment-region)))
(setq reftex-enable-partial-scans t)
(setq reftex-save-parse-info t)
(setq reftex-use-multiple-selection-buffers t)
(setq reftex-refontify-context t)
;; Use \eqref for citation of formulas
(setq reftex-label-alist '((nil ?e nil "~\\eqref{%s}" nil nil)))

;;--------------------------------------------------------------

;; use auto-complete and yasnippet on yatex
(add-to-list 'ac-modes 'yatex-mode)
(defun ac-yatex-mode-setup ()
  (setq ac-sources (append '(ac-source-yasnippet) ac-sources)))
(add-hook 'yatex-mode-hook 'ac-yatex-mode-setup)

;;----------------------------------------------------------------

;; replace zenkaku characters
(defun replace-zenkaku-yatex ()
  (if (string= mode-name "やてふ")
      (replace-zenkaku)))

(defun replace-zenkaku ()
  "Replace zenkaku characters"
  (interactive "*")
  (let ((key nil)
        (reg (and transient-mark-mode mark-active)))
    (save-excursion
      (save-restriction
        (if reg
            (narrow-to-region (region-beginning) (region-end)))
        (goto-char (point-min))
        (while (re-search-forward "。" nil t) ;読点の置換
          (replace-match "．" nil nil))
        (if reg
            (narrow-to-region (region-beginning) (region-end)))
        (goto-char (point-min))
        (while (re-search-forward "、" nil t) ;句点の置換
          (replace-match "，" nil nil)))))
  (deactivate-mark))

(add-hook 'before-save-hook 'replace-zenkaku-yatex)

;;--------------------------------------------------------------

;; latex-math-preview.el
;; Ref: http://transitive.info/software/latex-math-preview/
;; Ref: http://www.emacswiki.org/emacs/LaTeXMathPreview
;;
(add-hook 'yatex-mode-hook
       '(lambda ()
          (YaTeX-define-key " " nil)
          (YaTeX-define-key "\C-p" 'latex-math-preview-expression)
          ;; (YaTeX-define-key "\C-p" 'latex-math-preview-save-image-file)
          (YaTeX-define-key "\C-j" 'latex-math-preview-insert-symbol)
          ;; (YaTeX-define-key "\C-j" 'latex-math-preview-last-symbol-again)
          ;; (YaTeX-define-key "\C-b" 'latex-math-preview-beamer-frame)
          ))
(autoload 'latex-math-preview-expression "latex-math-preview" nil t)
(autoload 'latex-math-preview-insert-symbol "latex-math-preview" nil t)
(autoload 'latex-math-preview-save-image-file "latex-math-preview" nil t)
(autoload 'latex-math-preview-beamer-frame "latex-math-preview" nil t)
(setq latex-math-preview-in-math-mode-p-func 'YaTeX-in-math-mode-p)
(setq latex-math-preview-tex-to-png-for-preview '(platex dvipng))
(setq latex-math-preview-tex-to-png-for-save '(platex dvipng))
(setq latex-math-preview-tex-to-eps-for-save '(platex dvips-to-eps))
(setq latex-math-preview-tex-to-ps-for-save '(platex dvips-to-ps))
(setq latex-math-preview-beamer-to-png '(platex dvipdfmx gs-to-png))
