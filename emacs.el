;;; Text mode and auto Fill mode
;; The next two lines put Emacs into Text mode
;; and Auto fill mode, and are for writers who
;; want to start writing prose rather than code.
(setq-default major-mode 'text-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;; Prevent Extraneous Tabs
(setq-default indent-tabs-mode nil)

;;; Compare windows
(global-set-key "\C-cw" 'compare-windows)

;;; Keybinding for 'occur'
(global-set-key "\C-co" 'occur)

;;; Unbind 'C-x f'
(global-unset-key "\C-xf")

;;; Rebind 'C-x C-b' for 'buffer-menu'
(global-set-key "\C-x\C-b" 'buffer-menu)

;;; Emacs Load path
(setq load-path (cons "~/emacs" load-path))

;;; Line to top of window;
;;; replace three keystroke sequence C-u 0 C-l
(defun line-to-top-of-window()
  "Move the line point is on to top of window."
  (interactive)
  (recenter 2))

(global-set-key [f6] 'line-to-top-of-window)

(defun compile-run-c ()
  "Sets up a frame layout with 3 windows. Frame is vertially split into code
and run area. The run area on the right is horizontally split into
compile output and shell that runs the code. The current buffer is compiled
with -Wall and -ansi options. Warnings are shown in the compile output."
  (interactive)
  (save-buffer)
  (let* ((src-file-name (buffer-file-name))
         (src-dir-name (file-name-directory src-file-name))
         (compile-cmd (format "cd %s\ngcc -Wall -ansi %s" src-dir-name src-file-name)))
         (delete-other-windows)
         (split-window-right)
         (other-window 1)
         (switch-to-buffer "*compile-output*")
         (shell-command compile-cmd (current-buffer))
         (compilation-mode)
         (split-window-below)
         (other-window 1)
         (switch-to-buffer "run-shell")
         (shell (current-buffer))
         (sit-for 0.01)
         (comint-interrupt-subjob)
         (sit-for 0.01)
         (insert (format "cd %s" src-dir-name))
         (comint-send-input)
         (sit-for 0.01)
         (insert "./a.out")
         (comint-send-input)
         ))

(defun compile-test()
  (interactive)
  (save-restriction
    (message "min %d, max %d" (point-min) (point-max))
    (let ((compile-result  (call-process-region 0 1 "gcc" nil "*compile-output*" t "-Wall" "-ansi" "-xc" "-")))
      (message "compile result is %d" compile-result))))
  
(global-set-key (kbd "C-c b") 'switch-to-prev-buffer)
(global-set-key (kbd "C-c f") 'switch-to-next-buffer)

(defun duplicate-line()
       (interactive)
       (move-beginning-of-line 1)
       (kill-line 1)
       (yank)
       (yank)
       (previous-line)
       (setq kill-ring (cdr kill-ring)))
       
(global-set-key (kbd "C-c d") 'duplicate-line)

(column-number-mode)

(menu-bar-mode 0)

(global-set-key [f2] 'compile-run-c)
(global-set-key [f3] 'compile-test)
(add-hook 'shell-mode
          (lambda () (local-set-key (kbd "C-c C-d") 'comint-interrupt-subjob)))

