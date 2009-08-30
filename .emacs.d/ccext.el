(defun ccext-get-next-buffer-name ()
  "Return a .h or .cpp name for the current buffer."
  (let ((strtmp (split-string (buffer-name) "\\."))
	)
    ( cond
      ((string=  (car(cdr strtmp)) "h") 
       (concat (car strtmp) ".cpp"))
      ((string=  (car(cdr strtmp)) "cpp")
       (concat (car strtmp) ".h"))
      )
    )
  )

;find file pour creer le fichier
(defun ccext-goto-defdecl-buffer ()
  "Go to the corresponding .h or .cpp file of the current buffer."
  (interactive)
  (let ((buflist (buffer-list))
	(nextbuf (ccext-get-next-buffer-name))
	(nextfile)
	(done nil)
	)
    (setq nextfile (expand-file-name nextbuf))
    (while buflist
      (if (string= (buffer-name (car buflist)) nextbuf)
	  (setq done t))
      (setq buflist (cdr buflist))
      )

    (if done
	(switch-to-buffer nextbuf);Si le buffer existe switch
      (if (and (not done)         ;Si le fichier existe charge
	       (file-exists-p nextfile))
	  (let ((path (file-name-directory (buffer-file-name))))
	    (setq nextfile (concat path nextbuf))
	    (find-file nextfile))
	(message "File doesn't exist")))
  )
)

;;Create a new class
(defun ccext-create-class ( classname )
  "Create a cpp a h buffer for the class classname"
  (interactive "sClass name: ")
  (find-file (concat (downcase classname) ".h"))
  (insert-file "~/.emacs.d/mkclassh.tpl")
  (while (re-search-forward "classname_H" nil t)
    (replace-match (upcase (concat classname "_H")) nil nil))
  (while (re-search-forward "classname" nil t)
    (replace-match classname nil nil))
  (indent-region 0 (point-max) nil)

  (find-file (concat (downcase classname) ".cpp"))
  (insert-file "~/.emacs.d/mkclasscpp.tpl")
  (while (re-search-forward "classname.h" nil t)
    (replace-match (concat (downcase classname) ".h") nil nil))
  (while (re-search-forward "classname" nil t)
    (replace-match classname nil nil))
  (indent-region 0 (point-max) nil)
)

;;Utiliser f6 pour la version de kde
;;(define-key global-map [f1] 'ccext-goto-defdecl-buffer)
(define-key global-map [f2] 'ccext-create-class)

;Sous xemacs seulement
;(add-menu-button '("CC-Ext") [ "New class" ccext-create-class ])
;(add-menu-button '("CC-Ext") [ "Switch h/cpp" ccext-goto-defdecl-buffer ])
