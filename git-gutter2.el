;;; git-gutter2.el --- Port of Sublime Text plugin GitGutter -*- lexical-binding: t; -*-

;; Copyright (C) 2020 by Shohei YOSHIDA

;; Author: Shohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-git-gutter2
;; Version: 0.90
;; Package-Requires: ((emacs "26.3"))

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

(defcustom git-gutter2-window-width nil
  "Character width of gutter window. Emacs mistakes width of some characters.
It is better to explicitly assign width to this variable, if you use full-width
character for signs of changes"
  :type 'integer)

(defcustom git-gutter2-diff-option ""
  "Option of 'git diff'"
  :type 'string)

(defcustom git-gutter2-update-commands
  '(ido-switch-buffer helm-buffers-list)
  "Each command of this list is executed, gutter information is updated."
  :type '(list (function :tag "Update command")
               (repeat :inline t (function :tag "Update command"))))

(defcustom git-gutter2-update-windows-commands
  '(kill-buffer ido-kill-buffer)
  "Each command of this list is executed, gutter information is updated and
gutter information of other windows."
  :type '(list (function :tag "Update command")
               (repeat :inline t (function :tag "Update command"))))

(defcustom git-gutter2-update-hooks
  '(after-save-hook after-revert-hook find-file-hook after-change-major-mode-hook
    text-scale-mode-hook)
  "hook points of updating gutter"
  :type '(list (hook :tag "HookPoint")
               (repeat :inline t (hook :tag "HookPoint"))))

(defcustom git-gutter2-modified-sign "="
  "Modified sign"
  :type 'string)

(defcustom git-gutter2-added-sign "+"
  "Added sign"
  :type 'string)

(defcustom git-gutter2-deleted-sign "-"
  "Deleted sign"
  :type 'string)

(defcustom git-gutter2-lighter " GitGutter2"
  "Minor mode lighter in mode-line"
  :type 'string)

(defcustom git-gutter2-verbosity 0
  "Log/message level. 4 means all, 0 nothing."
  :type 'integer)

(defface git-gutter2-modified
  '((t (:foreground "magenta" :weight bold :inherit default)))
  "Face of modified")

(defface git-gutter2-added
  '((t (:foreground "green" :weight bold :inherit default)))
  "Face of added")

(defface git-gutter2-deleted
  '((t (:foreground "red" :weight bold)))
  "Face of deleted")

(defcustom git-gutter2-disabled-modes nil
  "A list of modes which `global-git-gutter2-mode' should be disabled."
  :type '(repeat symbol))

(defcustom git-gutter2-mode-on-hook nil
  "Hook run when git-gutter mode enable"
  :type 'hook)

(defcustom git-gutter2-mode-off-hook nil
  "Hook run when git-gutter mode disable"
  :type 'hook)

(defcustom git-gutter2-update-interval 0
  "Time interval in seconds for updating diff information."
  :type 'integer)

(defcustom git-gutter2-ask-p t
  "Ask whether commit/revert or not"
  :type 'boolean)

(cl-defstruct git-gutter2-hunk
  type content start-line end-line)

(defvar git-gutter2--enabled nil)
(defvar git-gutter2--diffinfos nil)
(defvar git-gutter2--real-this-command nil)
(defvar git-gutter2--in-repository nil)
(defvar git-gutter2--update-timer nil)
(defvar git-gutter2--last-sha1 nil)

(defvar git-gutter2--popup-buffer "*git-gutter2-diff*")
(defvar git-gutter2--ignore-commands
  '(minibuffer-complete-and-exit
    exit-minibuffer
    ido-exit-minibuffer
    helm-maybe-exit-minibuffer
    helm-confirm-and-exit-minibuffer))

(defmacro git-gutter2-awhen (test &rest body)
  "Anaphoric when."
  (declare (indent 1))
  `(let ((it ,test))
     (when it ,@body)))

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

(defsubst git-gutter2--window-margin ()
  (or git-gutter2-window-width (git-gutter2--longest-sign-width)))

(defun git-gutter2--set-window-margin (width)
  (when (>= width 0)
    (let ((curwin (get-buffer-window)))
      (set-window-margins curwin width (cdr (window-margins curwin))))))

(defun git-gutter2--git-diff-arguments (file)
  (let (args)
    (unless (string= git-gutter2-diff-option "")
      (setq args (nreverse (split-string git-gutter2-diff-option))))
    (nreverse (cons file args))))

(defun git-gutter2--start-diff-process1 (file proc-buf)
  (let ((arg (git-gutter2--git-diff-arguments file)))
    (apply #'start-file-process "git-gutter" proc-buf
           "git" "--no-pager" "-c" "diff.autorefreshindex=0"
           "diff" "--no-color" "--no-ext-diff" "--relative" "-U0"
           arg)))

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

(defun git-gutter2--longest-sign-width ()
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
  (let ((buf (git-gutter2--diff-process-buffer (git-gutter2--base-file))))
    (git-gutter2-awhen (get-buffer buf)
      (kill-buffer it))))

;;;###autoload
(define-minor-mode git-gutter2-mode
  "Git-Gutter mode"
  :init-value nil
  :global     nil
  :lighter    git-gutter2-lighter
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
            (git-gutter2-update)
            (when (and (not git-gutter2--update-timer) (> git-gutter2-update-interval 0))
              (setq git-gutter2--update-timer
                    (run-with-idle-timer git-gutter2-update-interval t #'git-gutter2--live-update))))
        (when (> git-gutter2-verbosity 2)
          (message "Here is not git work tree"))
        (git-gutter2-mode -1))
    (remove-hook 'kill-buffer-hook #'git-gutter2--kill-buffer-hook t)
    (remove-hook 'pre-command-hook #'git-gutter2--pre-command-hook)
    (remove-hook 'post-command-hook #'git-gutter2--post-command-hook t)
    (dolist (hook git-gutter2-update-hooks)
      (remove-hook hook #'git-gutter2-update t))
    (git-gutter2--clear-gutter)))

(defun git-gutter2--turn-on ()
  (when (and (buffer-file-name)
             (not (memq major-mode git-gutter2-disabled-modes)))
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

(defun git-gutter2--clear-gutter ()
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
    (git-gutter2--clear-gutter)
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
  (git-gutter2-awhen (git-gutter2--search-here-diffinfo git-gutter2--diffinfos)
    (let ((diff-info (git-gutter2--adjust-diff-info it)))
      (save-window-excursion
        (when git-gutter2-ask-p
          (git-gutter2-popup-hunk diff-info))
        (when (or (not git-gutter2-ask-p) (yes-or-no-p (format "%s current hunk ? " action)))
          (funcall action-fn diff-info)
          (funcall update-fn))
        (if git-gutter2-ask-p
            (delete-window (git-gutter2--popup-buffer-window))
          (message "%s current hunk." action))))))

(defun git-gutter2-revert-hunk ()
  "Revert current hunk."
  (interactive)
  (git-gutter2--query-action "Revert" #'git-gutter2--do-revert-hunk #'save-buffer))

(defun git-gutter2--extract-hunk-header ()
  (git-gutter2-awhen (git-gutter2--base-file)
    (with-temp-buffer
      (when (zerop (git-gutter2--execute-command
                    "git" t "--no-pager" "-c" "diff.autorefreshindex=0"
                    "diff" "--no-color" "--no-ext-diff"
                    "--relative" (file-name-nondirectory it)))
        (goto-char (point-min))
        (forward-line 4)
        (buffer-substring-no-properties (point-min) (point))))))

(defun git-gutter2--read-hunk-header (header)
  (let ((header-regexp "^@@ -\\([0-9]+\\),?\\([0-9]*\\) \\+\\([0-9]+\\),?\\([0-9]*\\) @@"))
    (when (string-match header-regexp header)
      (list (string-to-number (match-string 1 header))
            (git-gutter2--changes-to-number (match-string 2 header))
            (string-to-number (match-string 3 header))
            (git-gutter2--changes-to-number (match-string 4 header))))))

(defun git-gutter2--convert-hunk-header (type)
  (let ((header (buffer-substring-no-properties (point) (line-end-position))))
    (delete-region (point) (line-end-position))
    (cl-destructuring-bind
        (orig-line orig-changes new-line new-changes) (git-gutter2--read-hunk-header header)
      (cl-case type
        (added (setq new-line (1+ orig-line)))
        (t (setq new-line orig-line)))
      (let ((new-header (format "@@ -%d,%d +%d,%d @@"
                                orig-line orig-changes new-line new-changes)))
        (insert new-header)))))

(defun git-gutter2--insert-staging-hunk (hunk type)
  (save-excursion
    (insert hunk "\n"))
  (git-gutter2--convert-hunk-header type))

(defun git-gutter2--apply-directory-option ()
  (let ((root (locate-dominating-file default-directory ".git")))
    (file-name-directory (file-relative-name (git-gutter2--base-file) root))))

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
          (message "@@ check1 %s" new-hunk)
          (setf (git-gutter2-hunk-content adjusted-hunk) new-hunk)
          adjusted-hunk)))))

(defun git-gutter2--do-stage-hunk (diff-info)
  (let ((content (git-gutter2-hunk-content diff-info))
        (type (git-gutter2-hunk-type diff-info))
        (header (git-gutter2--extract-hunk-header))
        (patch (make-temp-name "git-gutter")))
    (when header
      (with-temp-file patch
        (insert header)
        (git-gutter2--insert-staging-hunk content type))
      (let ((dir-option (git-gutter2--apply-directory-option))
            (options (list "--cached" patch)))
        (when dir-option
          (setq options (cons "--directory" (cons dir-option options))))
        (unless (zerop (apply #'git-gutter2--execute-command
                              "git" nil "apply" "--unidiff-zero"
                              options))
          (message "Failed: stating this hunk"))
        (delete-file patch)))))

(defun git-gutter2-stage-hunk ()
  "Stage this hunk like 'git add -p'."
  (interactive)
  (git-gutter2--query-action "Stage" #'git-gutter2--do-stage-hunk #'git-gutter2-update))

(defsubst git-gutter2--line-point (line)
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (point)))

(defun git-gutter2-mark-hunk ()
  (interactive)
  (git-gutter2-awhen (git-gutter2--search-here-diffinfo git-gutter2--diffinfos)
    (let ((start (git-gutter2--line-point (git-gutter2-hunk-start-line it)))
          (end (git-gutter2--line-point (1+ (git-gutter2-hunk-end-line it)))))
      (goto-char start)
      (push-mark end nil t))))

(defun git-gutter2--update-popuped-buffer (diffinfo)
  (with-current-buffer (get-buffer-create git-gutter2--popup-buffer)
    (view-mode -1)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert (git-gutter2-hunk-content diffinfo))
    (insert "\n")
    (goto-char (point-min))
    (diff-mode)
    (view-mode +1)
    (current-buffer)))

(defun git-gutter2-popup-hunk (&optional diffinfo)
  "Popup current diff hunk."
  (interactive)
  (git-gutter2-awhen (or diffinfo (git-gutter2--search-here-diffinfo git-gutter2--diffinfos))
    (save-selected-window
      (pop-to-buffer (git-gutter2--update-popuped-buffer it)))))

(defun git-gutter2-next-hunk (arg)
  "Move to next diff hunk"
  (interactive "p")
  (if (not git-gutter2--diffinfos)
      (when (> git-gutter2-verbosity 3)
        (message "There are no changes!!"))
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
        (when (> git-gutter2-verbosity 0)
          (message "Move to %d/%d hunk" (1+ real-index) len))
        (when (buffer-live-p (get-buffer git-gutter2--popup-buffer))
          (git-gutter2--update-popuped-buffer diffinfo))))))

(defun git-gutter2-previous-hunk (arg)
  "Move to previous diff hunk"
  (interactive "p")
  (git-gutter2-next-hunk (- arg)))

(defun git-gutter2-end-of-hunk ()
  "Move to end of current diff hunk"
  (interactive)
  (git-gutter2-awhen (git-gutter2--search-here-diffinfo git-gutter2--diffinfos)
    (let ((lines (- (git-gutter2-hunk-end-line it) (line-number-at-pos))))
      (forward-line lines))))

;;;###autoload
(defun git-gutter2-update ()
  "Show diff information in gutter"
  (interactive)
  (when (or git-gutter2--in-repository (git-gutter2--in-repository-p))
    (let* ((file (git-gutter2--base-file))
           (proc-buf (git-gutter2--diff-process-buffer file)))
      (when (and (called-interactively-p 'interactive) (get-buffer proc-buf))
        (kill-buffer proc-buf))
      (when (and file (file-exists-p file) (not (get-buffer proc-buf)))
        (git-gutter2--start-diff-process (file-name-nondirectory file)
                                         (get-buffer-create proc-buf))))))

(defadvice vc-revert (after git-gutter2-vc-revert activate)
  (when git-gutter2-mode
    (run-with-idle-timer 0.1 nil 'git-gutter)))

;; `quit-window' and `switch-to-buffer' are called from other
;; commands. So we should use `defadvice' instead of `post-command-hook'.
(defadvice quit-window (after git-gutter2-quit-window activate)
  (when git-gutter2-mode
    (git-gutter2-update)))

(defadvice switch-to-buffer (after git-gutter2-switch-to-buffer activate)
  (when git-gutter2-mode
    (git-gutter2-update)))

(defun git-gutter2-start-update-timer ()
  (interactive)
  (when git-gutter2--update-timer
    (error "Update timer is already running."))
  (setq git-gutter2--update-timer
        (run-with-idle-timer git-gutter2-update-interval t #'git-gutter2--live-update)))

(defun git-gutter2-cancel-update-timer ()
  (interactive)
  (unless git-gutter2--update-timer
    (error "Timer is no running."))
  (cancel-timer git-gutter2--update-timer)
  (setq git-gutter2--update-timer nil))

(defsubst git-gutter2--write-current-content (tmpfile)
  (let ((content (buffer-substring-no-properties (point-min) (point-max))))
    (with-temp-file tmpfile
      (insert content))))

(defsubst git-gutter2--original-file-content (file)
  (with-temp-buffer
    (when (zerop (process-file "git" nil t nil "show" (concat ":" file)))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun git-gutter2--write-original-content (tmpfile filename)
  (git-gutter2-awhen (git-gutter2--original-file-content filename)
    (with-temp-file tmpfile
      (insert it)
      t)))

(defsubst git-gutter2--start-raw-diff-process (proc-buf original now)
  (start-file-process "git-gutter2-update-timer" proc-buf
                      "diff" "-U0" original now))

(defun git-gutter2--start-live-update (file original now)
  (let ((proc-bufname (git-gutter2--diff-process-buffer file)))
    (when (get-buffer proc-bufname)
      (kill-buffer proc-bufname))
    (let* ((curbuf (current-buffer))
           (proc-buf (get-buffer-create proc-bufname))
           (process (git-gutter2--start-raw-diff-process proc-buf original now)))
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
             (kill-buffer proc-buf)
             (delete-file original)
             (delete-file now))))))))

(defun git-gutter2--should-update-p ()
  (let ((sha1 (secure-hash 'sha1 (current-buffer))))
    (unless (equal sha1 git-gutter2--last-sha1)
      (setq git-gutter2--last-sha1 sha1))))

(defun git-gutter2--live-update ()
  (git-gutter2-awhen (git-gutter2--base-file)
    (when (and git-gutter2--enabled
               (buffer-modified-p)
               (git-gutter2--should-update-p))
      (let ((file (file-name-nondirectory it))
            (root (file-truename (locate-dominating-file default-directory ".git")))
            (now (make-temp-file "git-gutter-cur"))
            (original (make-temp-file "git-gutter-orig")))
        (if (git-gutter2--write-original-content original (file-relative-name it root))
            (progn
              (git-gutter2--write-current-content now)
              (git-gutter2--start-live-update file original now))
          (delete-file now)
          (delete-file original))))))

(defun git-gutter2-buffer-hunks ()
  "Count unstaged hunks in current buffer."
  (length git-gutter2--diffinfos))

(defun git-gutter2-all-hunks ()
  "Cound unstaged hunks in all buffers"
  (let ((sum 0))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when git-gutter2-mode
          (cl-incf sum (length git-gutter2--diffinfos)))))
    sum))

(defun git-gutter2--stat-hunk (hunk)
  (cl-case (git-gutter2-hunk-type hunk)
    (modified (with-temp-buffer
                (insert (git-gutter2-hunk-content hunk))
                (goto-char (point-min))
                (let ((added 0)
                      (deleted 0))
                  (while (not (eobp))
                    (cond ((looking-at-p "\\+") (cl-incf added))
                          ((looking-at-p "\\-") (cl-incf deleted)))
                    (forward-line 1))
                  (cons added deleted))))
    (added (cons (- (git-gutter2-hunk-end-line hunk) (git-gutter2-hunk-start-line hunk)) 0))
    (deleted (cons 0 (- (git-gutter2-hunk-end-line hunk) (git-gutter2-hunk-start-line hunk))))))

(defun git-gutter2-statistic ()
  "Return statistic unstaged hunks in current buffer."
  (interactive)
  (cl-loop for hunk in git-gutter2--diffinfos
           for (add . del) = (git-gutter2--stat-hunk hunk)
           sum add into added
           sum del into deleted
           finally
           return (progn
                    (when (called-interactively-p 'interactive)
                      (message "Added %d lines, Deleted %d lines" added deleted))
                    (cons added deleted))))

(defun git-gutter2-update-all-windows ()
  "Update git-gutter information for all visible buffers."
  (interactive)
  (dolist (win (window-list))
    (let ((buf (window-buffer win)))
      (with-current-buffer buf
        (when git-gutter2-mode
          (git-gutter2-update))))))

(provide 'git-gutter2)

;;; git-gutter2.el ends here
