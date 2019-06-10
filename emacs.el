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
