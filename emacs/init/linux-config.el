;; font
(let ((size (if (>= (x-display-pixel-width) 3000) 18 13.5)))
  (condition-case err
      (let ((myfont (format "Ricty Discord-%d" size)))
	(set-frame-font myfont)
	(add-to-list 'default-frame-alist `(font . ,myfont)))
    (error (message "%s" err))))
