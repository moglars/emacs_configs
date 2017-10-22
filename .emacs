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

(defun compile-run-c-2 ()
  "Sets up a frame layout with 3 windows. Frame is vertially split into code
and run area. The run area on the right is horizontally split into
compile output and shell that runs the code. The current buffer is compiled
with -Wall and -ansi options. Warnings are shown in the compile output."
  (interactive)
  (save-buffer)
  (let* ((src-file-name (buffer-file-name))
         (src-dir-name (file-name-directory src-file-name))
         (src-buffer (current-buffer))
         (compile-output-buffer (get-buffer-create "*compile-output2*"))
         (shell-buffer (get-buffer-create "run-shell"))
         (compile-result))
    (delete-other-windows)
    (split-window-right)
    (other-window 1)
    (switch-to-buffer compile-output-buffer)
    (setq buffer-read-only nil)
    (delete-region (point-min) (point-max))
    (set-buffer src-buffer)
    (setq default-directory src-dir-name)
    (save-restriction
      (widen)
      ;;I don't know yet, how to inform the compilation mode, in which buffer it should search the next error.
      ;;(setq compile-result (call-process-region (point-min) (point-max) "gcc" nil compile-output-buffer t "-Wall" "-ansi" "-xc" "-")))
      (setq compile-result (call-process "gcc" nil compile-output-buffer t "-Wall" "-ansi" src-file-name)))
    (set-buffer compile-output-buffer)
    (compilation-mode)
    (setq buffer-read-only t)
    (split-window-below)
    (other-window 1)
    (switch-to-buffer "run-shell")
    (shell (current-buffer))
    (sit-for 0.01)
    (comint-interrupt-subjob)
    (sit-for 0.01)
    (insert "PS1=\">\"")
    (comint-send-input)
    (sit-for 0.01)
    (insert (format "cd %s" src-dir-name))
    (comint-send-input)
    (sit-for 0.01)
    (if (= 0 compile-result)
        (progn
          (insert "./a.out")
          (comint-send-input))
      (insert "echo compile-error")
      (comint-send-input))
    ))

(defun compile-test()
  (interactive)
  (save-restriction
    (let ((compile-result  (call-process-region (point-min) (point-max) "gcc" nil "*test-compile-output*" t "-Wall" "-ansi" "-xc" "-")))
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
(global-set-key [f3] 'compile-run-c-2)
(add-hook 'shell-mode
          (lambda () (local-set-key (kbd "C-c C-d") 'comint-interrupt-subjob)))

(defun open-in-browser()
  (interactive)
  (shell-command (format "xdg-open \"%s\"" (buffer-file-name))))

(global-set-key (kbd "C-c §") 'open-in-browser)

(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

(add-to-list 'load-path "~/.emacs.d/elisp/")
(load "~/.emacs.d/elisp/lorem-ipsum.el")
