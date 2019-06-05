; dir-locals.el

((c++-mode . ((eval . (set (make-local-variable 'project-path)
                           (file-name-directory
                            (let ((d (dir-locals-find-file ".")))
                              (if (stringp d) d (car d))))))
              (eval . (setq cmake-ide-project-dir project-path ))
              (eval . (message "Cmake Project directory set to `%s'" project-path))
              (eval . (setq cmake-ide-build-dir (concat project-path "build")))
              (eval . (message "Cmake IDE build directory set to `%s'" cmake-ide-build-dir))
              (eval . (setq compile-command (concat "cd " project-path " && ./pounce -t oms")))
              (eval . (message "Compile directory set to `%s'" compile-command))
              )))

; One wants to paste some previously copied
;; (from application other than Emacs) text

(use-package evil
  :config
  (evil-mode 0)
  (dolist (mode '(ag-mode
                  flycheck-error-list-mode
                  git-rebase-mode))
    (add-to-list 'evil-emacs-state-modes mode))
; One wants to paste some previously copied; (from application other than Emacs)
  (setq evil-mode-line-format nil
		evil-visual-update-x-selection 'ignore
        evil-insert-state-cursor '(bar "White")
        evil-visual-state-cursor '(box "#F86155"))

  (evil-add-hjkl-bindings occur-mode-map 'emacs
    (kbd "/")       'evil-search-forward
    (kbd "n")       'evil-search-next
    (kbd "N")       'evil-search-previous
    (kbd "C-d")     'evil-scroll-down
    (kbd "C-u")     'evil-scroll-up
    (kbd "C-w C-w") 'other-window))
