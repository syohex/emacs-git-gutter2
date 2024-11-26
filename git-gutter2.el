;;; git-gutter2.el --- Port of Sublime Text plugin GitGutter -*- lexical-binding: t; -*-

;; Copyright (C) 2023 by Shohei YOSHIDA

;; Author: Shohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-git-gutter2
;; Version: 0.90
;; Package-Requires: ((emacs "29.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Port of GitGutter which is a plugin of Sublime Text

;;; Code:

(require 'cl-lib)

(defgroup git-gutter2 nil
  "Port GitGutter"
  :prefix "git-gutter2-"
  :group 'vc)

(defcustom git-gutter2-update-commands
  '(helm-buffers-list)
  "Each command of this list is executed, gutter information is updated."
  :type '(list (function :tag "Update command")
               (repeat :inline t (function :tag "Update command"))))

(defcustom git-gutter2-update-windows-commands
  '(kill-buffer)
  "Each command of this list is executed, gutter information is updated and
gutter information of other windows."
  :type '(list (function :tag "Update command")
               (repeat :inline t (function :tag "Update command"))))

(defcustom git-gutter2-update-hooks
  '(after-save-hook after-revert-hook find-file-hook after-change-major-mode-hook text-scale-mode-hook)
  "hook points of updating gutter"
  :type '(list (hook :tag "HookPoint")
               (repeat :inline t (hook :tag "HookPoint"))))

(defcustom git-gutter2-added-sign "+"
  "Added sign"
  :type 'string)

(defcustom git-gutter2-deleted-sign " "
  "Deleted sign"
  :type 'string)

(defcustom git-gutter2-modified-sign " "
  "Modified sign"
  :type 'string)

(defface git-gutter2-added
  '((t (:foreground "green" :weight bold :inherit default)))
  "Face of added sign")

(defface git-gutter2-deleted
  '((t (:foreground unspecified :background "brightred")))
  "Face of deleted sign")

(defface git-gutter2-modified
  '((t (:foreground unspecified :background "brightmagenta")))
  "Face of modified")

(cl-defstruct git-gutter2-hunk
  type content start-line end-line)

(defvar git-gutter2--enabled nil)
(defvar git-gutter2--diffinfos nil)
(defvar git-gutter2--real-this-command nil)
(defvar git-gutter2--in-repository nil)

(defvar git-gutter2--popup-buffer "*git-gutter2-diff*")
(defvar git-gutter2--ignore-commands
  '(minibuffer-complete-and-exit
    exit-minibuffer
    ido-exit-minibuffer
    helm-maybe-exit-minibuffer
    helm-confirm-and-exit-minibuffer))

(defsubst git-gutter2--execute-command (cmd output &rest args)
  (apply #'process-file cmd nil output nil args))

(defun git-gutter2--in-repository-p ()
  (let ((in-repository-p
         (with-temp-buffer
           (when (zerop (git-gutter2--execute-command "git" t "rev-parse" "--is-inside-work-tree"))
             (goto-char (point-min))
             (looking-at-p "true")))))
    (setq-local git-gutter2--in-repository in-repository-p)))

(defsubst git-gutter2--changes-to-number (str)
  (if (string= str "")
      1
    (string-to-number str)))

(defsubst git-gutter2--base-file ()
  (buffer-file-name (buffer-base-buffer)))

(defun git-gutter2--diff-content ()
  (save-excursion
    (goto-char (line-beginning-position))
    (let ((curpoint (point)))
      (forward-line 1)
      (if (re-search-forward "^@@" nil t)
          (backward-char 3) ;; for '@@'
        (goto-char (point-max)))
      (buffer-substring curpoint (point)))))

(defun git-gutter2--process-diff-output (buf)
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (goto-char (point-min))
      (cl-loop with regexp = "^@@ -\\(?:[0-9]+\\),?\\([0-9]*\\) \\+\\([0-9]+\\),?\\([0-9]*\\) @@"
               while (re-search-forward regexp nil t)
               for new-line  = (string-to-number (match-string 2))
               for orig-changes = (git-gutter2--changes-to-number (match-string 1))
               for new-changes = (git-gutter2--changes-to-number (match-string 3))
               for type = (cond ((zerop orig-changes) 'added)
                                ((zerop new-changes) 'deleted)
                                (t 'modified))
               for end-line = (if (eq type 'deleted)
                                  new-line
                                (1- (+ new-line new-changes)))
               for content = (git-gutter2--diff-content)
               collect
               (let ((start (if (zerop new-line) 1 new-line))
                     (end (if (zerop end-line) 1 end-line)))
                 (make-git-gutter2-hunk
                  :type type :content content :start-line start :end-line end))))))

(defun git-gutter2--set-window-margin (width)
  (when (>= width 0)
    (let ((curwin (get-buffer-window)))
      (set-window-margins curwin width (cdr (window-margins curwin))))))

(defun git-gutter2--start-diff-process1 (file proc-buf)
  (let ((args (list file)))
    (apply #'start-file-process "git-gutter" proc-buf
           "git" "--no-pager" "-c" "diff.autorefreshindex=0"
           "diff" "--no-color" "--no-ext-diff" "--relative" "-U0"
           args)))

(defun git-gutter2--start-diff-process (curfile proc-buf)
  (git-gutter2--set-window-margin (git-gutter2--window-margin))
  (let ((curbuf (current-buffer))
        (process (git-gutter2--start-diff-process1 curfile proc-buf)))
    (set-process-query-on-exit-flag process nil)
    (set-process-sentinel
     process
     (lambda (proc _event)
       (when (eq (process-status proc) 'exit)
         (setq git-gutter2--enabled nil)
         (let ((diffinfos (git-gutter2--process-diff-output (process-buffer proc))))
           (when (buffer-live-p curbuf)
             (with-current-buffer curbuf
               (git-gutter2--update-diffinfo diffinfos)
               (setq git-gutter2--enabled t)))
           (kill-buffer proc-buf)))))))

(defsubst git-gutter2--before-string (sign)
  (propertize " " 'display `((margin left-margin) ,sign)))

(defun git-gutter2--propertized-sign (type)
  (let (sign face)
    (cl-case type
      (added (setq sign git-gutter2-added-sign
                   face 'git-gutter2-added))
      (modified (setq sign git-gutter2-modified-sign
                      face 'git-gutter2-modified))
      (deleted (setq sign git-gutter2-deleted-sign
                     face 'git-gutter2-deleted)))
    (when (get-text-property 0 'face sign)
      (setq face (append
                  (get-text-property 0 'face sign)
                  `(:inherit ,face))))
    (propertize sign 'face face)))

(defun git-gutter2--put-signs (sign points)
  (dolist (pos points)
    (let ((ov (make-overlay pos pos))
          (gutter-sign (git-gutter2--before-string sign)))
      (overlay-put ov 'before-string gutter-sign)
      (overlay-put ov 'git-gutter2 t))))

(defsubst git-gutter2--sign-width (sign)
  (cl-loop for s across sign
           sum (char-width s)))

(defun git-gutter2--window-margin ()
  ;; choose the longest sign as window margin
  (let ((signs (list git-gutter2-modified-sign
                     git-gutter2-added-sign
                     git-gutter2-deleted-sign)))
    (apply #'max (mapcar #'git-gutter2--sign-width signs))))

(defsubst git-gutter2--check-file-and-directory ()
  (and (git-gutter2--base-file)
       default-directory (file-directory-p default-directory)))

(defun git-gutter2--pre-command-hook ()
  (unless (memq this-command git-gutter2--ignore-commands)
    (setq git-gutter2--real-this-command this-command)))

(defun git-gutter2--update-other-window-buffers (curwin curbuf)
  (save-selected-window
    (cl-loop for win in (window-list)
             unless (eq win curwin)
             do
             (progn
               (select-window win)
               (let ((win-width (window-margins win)))
                 (unless (car win-width)
                   (if (eq (current-buffer) curbuf)
                       (git-gutter2--set-window-margin (git-gutter2--window-margin))
                     (git-gutter2--update-diffinfo git-gutter2--diffinfos))))))))

(defun git-gutter2--post-command-hook ()
  (cond ((memq git-gutter2--real-this-command git-gutter2-update-commands)
         (git-gutter2-update))
        ((memq git-gutter2--real-this-command git-gutter2-update-windows-commands)
         (git-gutter2-update)
         (git-gutter2--update-other-window-buffers (selected-window) (current-buffer)))))

(defsubst git-gutter2--diff-process-buffer (curfile)
  (concat " *git-gutter2-" curfile "-*"))

(defun git-gutter2--kill-buffer-hook ()
  (when-let* ((proc-buf (get-buffer (git-gutter2--diff-process-buffer (git-gutter2--base-file)))))
    (kill-buffer proc-buf)))

;;;###autoload
(define-minor-mode git-gutter2-mode
  "Git-Gutter mode"
  :init-value nil
  :global     nil
  :lighter    " GitGutter2"
  (if git-gutter2-mode
      (if (and (git-gutter2--check-file-and-directory)
               (git-gutter2--in-repository-p))
          (progn
            (make-local-variable 'git-gutter2--enabled)
            (make-local-variable 'git-gutter2--diffinfos)
            (add-hook 'kill-buffer-hook #'git-gutter2--kill-buffer-hook nil t)
            (add-hook 'pre-command-hook #'git-gutter2--pre-command-hook)
            (add-hook 'post-command-hook #'git-gutter2--post-command-hook nil t)
            (dolist (hook git-gutter2-update-hooks)
              (add-hook hook #'git-gutter2-update nil t))
            (git-gutter2-update))
        (git-gutter2-mode -1))
    (remove-hook 'kill-buffer-hook #'git-gutter2--kill-buffer-hook t)
    (remove-hook 'pre-command-hook #'git-gutter2--pre-command-hook)
    (remove-hook 'post-command-hook #'git-gutter2--post-command-hook t)
    (dolist (hook git-gutter2-update-hooks)
      (remove-hook hook #'git-gutter2-update t))
    (git-gutter2-clear-gutter)))

(defun git-gutter2--turn-on ()
  (when (buffer-file-name)
    (git-gutter2-mode +1)))

;;;###autoload
(define-global-minor-mode global-git-gutter2-mode git-gutter2-mode git-gutter2--turn-on)

(defun git-gutter2--view-set-overlays (diffinfos)
  (save-excursion
    (goto-char (point-min))
    (cl-loop with curline = 1
             for info in diffinfos
             for start-line = (git-gutter2-hunk-start-line info)
             for end-line = (git-gutter2-hunk-end-line info)
             for type = (git-gutter2-hunk-type info)
             for sign = (git-gutter2--propertized-sign type)
             for points = nil
             do
             (let ((bound (progn
                            (forward-line (- end-line curline))
                            (point))))
               (forward-line (- start-line end-line))
               (cl-case type
                 ((modified added)
                  (while (and (<= (point) bound) (not (eobp)))
                    (push (point) points)
                    (forward-line 1))
                  (git-gutter2--put-signs sign points))
                 (deleted
                  (git-gutter2--put-signs sign (list (point)))
                  (forward-line 1)))
               (setq curline (1+ end-line))))))

(defun git-gutter2-clear-gutter ()
  (interactive)
  (save-restriction
    (widen)
    (unless global-git-gutter2-mode
      (git-gutter2--set-window-margin 0))
    (remove-overlays (point-min) (point-max) 'git-gutter2 t))
  (setq git-gutter2--enabled nil
        git-gutter2--diffinfos nil))

(defun git-gutter2--update-diffinfo (diffinfos)
  (save-restriction
    (widen)
    (git-gutter2-clear-gutter)
    (setq git-gutter2--diffinfos diffinfos)
    (when diffinfos
      (git-gutter2--view-set-overlays diffinfos))
    (when (or global-git-gutter2-mode diffinfos)
      (git-gutter2--set-window-margin (git-gutter2--window-margin)))))

(defun git-gutter2--search-near-diff-index (diffinfos is-reverse)
  (cl-loop with current-line = (line-number-at-pos)
           with cmp-fn = (if is-reverse #'> #'<)
           for diffinfo in (if is-reverse (reverse diffinfos) diffinfos)
           for index = 0 then (1+ index)
           for start-line = (git-gutter2-hunk-start-line diffinfo)
           when (funcall cmp-fn current-line start-line)
           return (if is-reverse
                      (1- (- (length diffinfos) index))
                    index)))

(defun git-gutter2--search-here-diffinfo (diffinfos)
  (save-restriction
    (widen)
    (cl-loop with current-line = (line-number-at-pos)
             for diffinfo in diffinfos
             for start = (git-gutter2-hunk-start-line diffinfo)
             for end   = (or (git-gutter2-hunk-end-line diffinfo) (1+ start))
             when (and (>= current-line start) (<= current-line end))
             return diffinfo
             finally do (error "Here is not changed!!"))))

(defun git-gutter2--collect-deleted-line (str)
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (cl-loop while (re-search-forward "^-\\(.*?\\)$" nil t)
             collect (match-string 1) into deleted-lines
             finally return deleted-lines)))

(defun git-gutter2--delete-added-lines (start-line end-line)
  (forward-line (1- start-line))
  (let ((start-point (point)))
    (forward-line (1+ (- end-line start-line)))
    (delete-region start-point (point))))

(defun git-gutter2--insert-deleted-lines (content)
  (dolist (line (git-gutter2--collect-deleted-line content))
    (insert (concat line "\n"))))

(defsubst git-gutter2--delete-from-first-line-p (start-line end-line)
  (and (not (= start-line 1)) (not (= end-line 1))))

(defun git-gutter2--do-revert-hunk (diffinfo)
  (save-excursion
    (goto-char (point-min))
    (let ((start-line (git-gutter2-hunk-start-line diffinfo))
          (end-line (git-gutter2-hunk-end-line diffinfo))
          (content (git-gutter2-hunk-content diffinfo)))
      (cl-case (git-gutter2-hunk-type diffinfo)
        (added (git-gutter2--delete-added-lines start-line end-line))
        (deleted (when (git-gutter2--delete-from-first-line-p start-line end-line)
                   (forward-line start-line))
                 (git-gutter2--insert-deleted-lines content))
        (modified (git-gutter2--delete-added-lines start-line end-line)
                  (git-gutter2--insert-deleted-lines content))))))

(defsubst git-gutter2--popup-buffer-window ()
  (get-buffer-window (get-buffer git-gutter2--popup-buffer)))

(defun git-gutter2--query-action (action action-fn update-fn)
  (when-let* ((here-diff-info (git-gutter2--search-here-diffinfo git-gutter2--diffinfos)))
    (let ((diff-info (git-gutter2--adjust-diff-info here-diff-info)))
      (save-window-excursion
        (git-gutter2-popup-hunk diff-info)
        (when (yes-or-no-p (format "%s current hunk ? " action))
          (funcall action-fn diff-info)
          (funcall update-fn))
        (delete-window (git-gutter2--popup-buffer-window))))))

(defun git-gutter2-revert-hunk ()
  "Revert current hunk."
  (interactive)
  (git-gutter2--query-action "Revert" #'git-gutter2--do-revert-hunk #'save-buffer))

(defun git-gutter2--adjust-added-hunk (diff-info start-line end-line)
  (unless (= (git-gutter2-hunk-start-line diff-info) start-line)
    (error "Invalid region. Staging region for added hunk must start from first line of this hunk"))
  (let ((region-changes (1+ (- end-line start-line))))
    (with-temp-buffer
      (insert (git-gutter2-hunk-content diff-info))
      (goto-char (point-min))
      ;; re-write header
      (re-search-forward "\\+\\([0-9]+\\),\\([0-9]+\\)" nil t)
      (let ((base-line (string-to-number (match-string-no-properties 1))))
        (replace-match (number-to-string region-changes) t t nil 2)
        (let ((end-offset (1+ (- end-line base-line))))
          (forward-line (1+ end-offset))
          (delete-region (point) (point-max))
          (buffer-string))))))

(defun git-gutter2--adjust-diff-by-region (pre-keep-lines keep-lines post-keep-lines)
  (let ((delete-fn (lambda (lines)
                     (dotimes (_i lines)
                       (let ((pos (point)))
                         (forward-line 1)
                         (delete-region pos (point)))))))
    (funcall delete-fn pre-keep-lines)
    (forward-line keep-lines)
    (funcall delete-fn post-keep-lines)))

(defun git-gutter2--adjust-modified-hunk (diff-info start-line end-line)
  (with-temp-buffer
    (insert (git-gutter2-hunk-content diff-info))
    (goto-char (point-min))
    (re-search-forward ",[0-9]+ \\+\\([0-9]+\\),[0-9]+" nil t)
    (let* ((base-line (string-to-number (match-string-no-properties 1)))
           (pre-keep-lines (- start-line base-line))
           (keep-lines (1+ (- end-line start-line)))
           (post-keep-lines (- (git-gutter2-hunk-end-line diff-info) end-line))
           (new-header (format ",%d +%d,%d" keep-lines base-line keep-lines)))
      (replace-match new-header)
      (forward-line 1)
      ;; adjust '-' part
      (git-gutter2--adjust-diff-by-region pre-keep-lines keep-lines post-keep-lines)
      (re-search-forward "^\\+" nil t)
      (goto-char (match-beginning 0))
      ;; adjust '+' part
      (git-gutter2--adjust-diff-by-region pre-keep-lines keep-lines post-keep-lines)
      (buffer-string))))

(defun git-gutter2--adjust-diff-info (diff-info)
  (let ((hunk-type (git-gutter2-hunk-type diff-info)))
    (if (or (not (use-region-p)) (not (memq hunk-type '(added modified))))
        diff-info
      (let ((start-line (max (line-number-at-pos (region-beginning))
                             (git-gutter2-hunk-start-line diff-info)))
            (end-line (min (line-number-at-pos (region-end))
                           (git-gutter2-hunk-end-line diff-info))))
        (let ((new-hunk (cl-case hunk-type
                          (added (git-gutter2--adjust-added-hunk diff-info start-line end-line))
                          (modified (git-gutter2--adjust-modified-hunk diff-info start-line end-line))))
              (adjusted-hunk (copy-git-gutter2-hunk diff-info)))
          (setf (git-gutter2-hunk-content adjusted-hunk) new-hunk)
          adjusted-hunk)))))

(defun git-gutter2--update-popuped-buffer (diffinfo)
  (let ((coding buffer-file-coding-system))
    (with-current-buffer (get-buffer-create git-gutter2--popup-buffer)
      (set-buffer-file-coding-system coding)
      (view-mode -1)
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (git-gutter2-hunk-content diffinfo))
      (insert "\n")
      (goto-char (point-min))
      (diff-mode)
      (view-mode +1)
      (current-buffer))))

(defun git-gutter2-popup-hunk (&optional diff-info)
  "Popup current diff hunk."
  (interactive)
  (when-let* ((diff-info (or diff-info (git-gutter2--search-here-diffinfo git-gutter2--diffinfos))))
    (save-selected-window
      (pop-to-buffer (git-gutter2--update-popuped-buffer diff-info)))))

(defun git-gutter2-next-hunk (arg)
  "Move to next diff hunk"
  (interactive "p")
  (when git-gutter2--diffinfos
    (save-restriction
      (widen)
      (let* ((is-reverse (< arg 0))
             (diffinfos git-gutter2--diffinfos)
             (len (length diffinfos))
             (index (git-gutter2--search-near-diff-index diffinfos is-reverse))
             (real-index (if index
                             (let ((next (if is-reverse (1+ index) (1- index))))
                               (mod (+ arg next) len))
                           (if is-reverse (1- len) 0)))
             (diffinfo (nth real-index diffinfos)))
        (goto-char (point-min))
        (forward-line (1- (git-gutter2-hunk-start-line diffinfo)))
        (when (buffer-live-p (get-buffer git-gutter2--popup-buffer))
          (git-gutter2--update-popuped-buffer diffinfo))))))

(defun git-gutter2-previous-hunk (arg)
  "Move to previous diff hunk"
  (interactive "p")
  (git-gutter2-next-hunk (- arg)))

;;;###autoload
(defun git-gutter2-update ()
  "Show diff information in gutter"
  (interactive)
  (when (and git-gutter2-mode (or git-gutter2--in-repository (git-gutter2--in-repository-p)))
    (let* ((file (git-gutter2--base-file))
           (proc-buf (git-gutter2--diff-process-buffer file)))
      (when (and (called-interactively-p 'interactive) (get-buffer proc-buf))
        (kill-buffer proc-buf))
      (when (and file (file-exists-p file) (not (get-buffer proc-buf)))
        (git-gutter2--start-diff-process (file-name-nondirectory file)
                                         (get-buffer-create proc-buf))))))

(defun git-gutter2--after-vc-revert (&rest _args)
  (run-with-idle-timer 0.1 nil #'git-gutter2-update))

(defun git-gutter2--after-update (&rest _args)
  (git-gutter2-update))

(advice-add 'vc-revert :after #'git-gutter2--after-vc-revert)
(advice-add 'quit-window :after #'git-gutter2--after-update)
(advice-add 'switch-to-buffer :after #'git-gutter2--after-update)

(defun git-gutter2-buffer-hunks ()
  "Count unstaged hunks in current buffer."
  (length git-gutter2--diffinfos))

(provide 'git-gutter2)

;;; git-gutter2.el ends here
