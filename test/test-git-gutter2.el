;;; test-git-gutter2.el --- Test for git-gutter2.el -*- lexical-binding: t -*-

;; Copyright (C) 2020 by Shohei YOSHIDA

;; Author: Shohei YOSHIDA <syohex@gmail.com>

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

;;; Code:

(require 'ert)
(require 'git-gutter2)

(ert-deftest git-gutter2--sign-width ()
  "helper function `git-gutter2-sign-width'"
  (let ((got1 (git-gutter2--sign-width "a"))
        (got2 (git-gutter2--sign-width "0123456789")))
    (should (= got1 1))
    (should (= got2 10))))

(ert-deftest git-gutter2--propertized-sign ()
  "helper function `git-gutter2--propertized-sign'"
  (should (string= (git-gutter2--propertized-sign 'added) "+")))

(ert-deftest git-gutter2--changes-to-number ()
  "helper function `git-gutter2--changes-to-number'"
  (should (= (git-gutter2--changes-to-number "") 1))
  (should (= (git-gutter2--changes-to-number "123") 123)))

(ert-deftest git-gutter2-in-git-repository-p ()
  "Should return nil if default-directory does not exist"

  ;; In git repository, but here is '.git'
  (when (file-directory-p ".git") ;; #36
    (let ((buf (find-file-noselect ".git/config")))
      (with-current-buffer buf
        (should-not (git-gutter2--in-repository-p)))))

  (let ((default-directory (file-name-directory (locate-library "git-gutter2"))))
    (should (git-gutter2--in-repository-p))))

(ert-deftest git-gutter2-update ()
  "Should return nil if buffer does not related with file or file is not existed"
  (with-current-buffer (get-buffer-create "*not-related-file*")
    (should-not (git-gutter2-update)))
  (let ((buf (find-file-noselect "not-found")))
    (with-current-buffer buf
      (should-not (git-gutter2-update)))))

(ert-deftest git-gutter2--collect-deleted-line ()
  "Should return lines which start with '-'"
  (let* ((input (mapconcat 'identity
                           (list "-apple" "-melon" "+orange")
                           "\n"))
         (got (git-gutter2--collect-deleted-line input)))
    (should (equal got '("apple" "melon")))))

(ert-deftest git-gutter2--insert-deleted-lines ()
  "Should insert deleted line"
  (let ((input (mapconcat 'identity
                          (list "-apple" "-melon" "+orange")
                          "\n")))
    (with-temp-buffer
      (git-gutter2--insert-deleted-lines input)
      (should (string= (buffer-string)
                       "apple\nmelon\n")))))

(ert-deftest git-gutter2--diff-content ()
  "Should return diff hunk"
  (let* ((input "@@-1,1+1,1@@
foo
bar
@@ -2,2 +2,2 @@")
         (got (with-temp-buffer
                (insert input)
                (goto-char (point-min))
                (goto-char (line-end-position))
                (git-gutter2--diff-content))))
    (should (string= got "@@-1,1+1,1@@\nfoo\nbar"))))

(ert-deftest git-gutter2--set-window-margin ()
  "Should change window margin"
  (git-gutter2--set-window-margin 4)
  (let ((got (car (window-margins))))
    (should (= got 4))))

(ert-deftest git-gutter2-mode-success ()
  "Case git-gutter2-mode enabled"
  (with-current-buffer (find-file-noselect "test-git-gutter2.el")
    (git-gutter2-mode 1)
    (should git-gutter2-mode))
  (kill-buffer "test-git-gutter2.el"))

(ert-deftest git-gutter2-mode-failed ()
  "Case git-gutter2-mode disabled"
  (with-temp-buffer
    (git-gutter2-mode 1)
    (should-not git-gutter2-mode))

  (let ((default-directory nil))
    (git-gutter2-mode 1)
    (should-not git-gutter2-mode))

  (let ((default-directory "foo"))
    (git-gutter2-mode 1)
    (should-not git-gutter2-mode))

  (when (file-directory-p ".git") ;; #36
    (with-current-buffer (find-file-noselect ".git/config")
      (git-gutter2-mode 1)
      (should-not git-gutter2-mode))))

(ert-deftest global-git-gutter2-mode-success ()
  "Case global-git-gutter2-mode enabled"
  (with-current-buffer (find-file-noselect "test-git-gutter2.el")
    (global-git-gutter2-mode t)
    (should git-gutter2-mode))

  (kill-buffer "test-git-gutter2.el"))

;;; test-git-gutter2.el end here
