# git-gutter2.el ![](https://github.com/syohex/emacs-git-gutter2/workflows/CI/badge.svg)

This is not [melpa version](https://github.com/emacsorphanage/git-gutter).
This is my own git-gutter.el implementation. I'm tired from maintaining emacs package.

## Introduction

`git-gutter2.el` is port of [GitGutter](https://github.com/jisaacks/GitGutter)
which is a plugin of Sublime Text.


## Features

- Asynchronous updating
- Work without `vc-mode`


## Screenshot

![Screenshot of git-gutter.el](image/git-gutter1.png)


## Requirements

* [Git](http://git-scm.com/)(1.7.0 or higher)


## Global Minor Mode and Minor Mode

`git-gutter2.el` provides global minor-mode(`global-git-gutter2-mode`) and minor-mode(`git-gutter2-mode`).

If you want to use `git-gutter2` for files in git repository.
You add following s-exp in your configuration file(`~/.emacs.d/init.el` or `~/.emacs`).

```lisp
(global-git-gutter2-mode +1)
```

Other case, you want to use `git-gutter2` for some files, you can use `git-gutter2-mode`.
Following example of enabling `git-gutter2` for some mode.

```lisp
(add-hook 'ruby-mode-hook 'git-gutter2-mode)
(add-hook 'python-mode-hook 'git-gutter2-mode)
```

## Commands

`git-gutter2.el` provides following commands.
**Obsoleted interfaces will be removed when 1.0 released.**

#### `git-gutter2-next-hunk`

Jump to next hunk

#### `git-gutter2-previous-hunk`

Jump to previous hunk

#### `git-gutter2-popup-hunk`

Popup current diff hunk(alias `git-gutter2-popup-diff`)

`git-gutter2-next-hunk` and `git-gutter2-previous-hunk` update content
of buffer popuped by `git-gutter2-popup-diff` to current hunk.

#### `git-gutter2-revert-hunk`

Revert current hunk

#### `git-gutter2-update`

Show/update changes from last commit or Update change information.
Please execute this command if diff information is not be updated.


## Sample Configuration

```lisp
(require 'git-gutter2)

;; If you enable global minor mode
(global-git-gutter2-mode t)

;; If you enable git-gutter2-mode for some modes
(add-hook 'ruby-mode-hook 'git-gutter2-mode)

(global-set-key (kbd "C-x C-g") 'git-gutter2-update)
(global-set-key (kbd "C-x v =") 'git-gutter2-popup-hunk)

;; Jump to next/previous hunk
(global-set-key (kbd "C-x p") 'git-gutter2-previous-hunk)
(global-set-key (kbd "C-x n") 'git-gutter2-next-hunk)

;; Revert current hunk
(global-set-key (kbd "C-x v r") 'git-gutter2-revert-hunk)
```


## Customize

### Look and feel

![Screenshot of multiple characters in gutter](image/git-gutter2-multichar.png)

You can change the signs and those faces.

```lisp
(custom-set-variables
 '(git-gutter2-modified-sign "  ") ;; two space
 '(git-gutter2-added-sign "++")    ;; multiple character is OK
 '(git-gutter2-deleted-sign "--"))

(set-face-background 'git-gutter2-modified "purple") ;; background color
(set-face-foreground 'git-gutter2-added "green")
(set-face-foreground 'git-gutter2-deleted "red")
```

### Updates hooks

diff information is updated at hooks in `git-gutter2-update-hooks`.

```lisp
(add-to-list 'git-gutter2-update-hooks 'focus-in-hook)
```

### Updates commands

diff information is updated after command in `git-gutter2-update-commands` executed.

```lisp
(add-to-list 'git-gutter2-update-commands 'other-window)
```


### Run hook

Run hook `git-gutter2-mode-on-hook` when `git-gutter2-mode` is turn on, and
run hook `git-gutter2-mode-off-hook` when `git-gutter2-mode` is turn off.


#### `(git-gutter2-buffer-hunks)`

Count unstaged hunks in current buffer.
