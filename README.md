# git-gutter2.el

This is not [melpa version](https://github.com/emacsorphanage/git-gutter).

## Introduction

`git-gutter2.el` is port of [GitGutter](https://github.com/jisaacks/GitGutter)
which is a plugin of Sublime Text.


## Features

- Asynchronous updating
- [Live updating](#live-updating)
- Work without `vc-mode`


## Screenshot

![Screenshot of git-gutter.el](image/git-gutter1.png)


## Requirements

* Emacs 26.1 or higher
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

#### `git-gutter2-end-of-hunk`

Move to end of current hunk

#### `git-gutter2-mark-hunk`

Mark current hunk.

#### `git-gutter2-popup-hunk`

Popup current diff hunk(alias `git-gutter2-popup-diff`)

`git-gutter2-next-hunk` and `git-gutter2-previous-hunk` update content
of buffer popuped by `git-gutter2-popup-diff` to current hunk.

#### `git-gutter2-stage-hunk`

Stage current hunk. You can use this command like `git add -p`.
This command is supported only for `git`.

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

;; Stage current hunk
(global-set-key (kbd "C-x v s") 'git-gutter2-stage-hunk)

;; Revert current hunk
(global-set-key (kbd "C-x v r") 'git-gutter2-revert-hunk)

;; Mark current hunk
(global-set-key (kbd "C-x v SPC") #'git-gutter2-mark-hunk)
```


## Customize

### Live updating

If you set `git-gutter2-update-interval` seconds larger than 0, `git-gutter2` updates
diff information in real-time by idle timer.

```lisp
(custom-set-variables
 '(git-gutter2-update-interval 2))
```

You can stop timer by `git-gutter2-cancel-update-timer` and starts by `git-gutter2-start-update-timer`.

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

You can change minor-mode name in mode-line to set `git-gutter2-lighter`.
Default is " GitGutter"

```lisp
;; first character should be a space
(custom-set-variables
 '(git-gutter2-lighter " GG"))
```


### Using full width characters

![Screenshot of using full-width character as diff sign](image/git-gutter2-fullwidth.png)

Emacs has `char-width` function which returns character width.
`git-gutter2.el` uses it for calculating character length of the signs.
But `char-width` does not work for some full-width characters.
So you should explicitly specify window width, if you use full-width
character.

```lisp
(custom-set-variables
 '(git-gutter2-window-width 2)
 '(git-gutter2-modified-sign "☁")
 '(git-gutter2-added-sign "☀")
 '(git-gutter2-deleted-sign "☂"))
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

### Disabled modes

If you use `global-git-gutter2-mode`, you may want some modes to disable
`git-gutter2-mode`. You can make it by setting `git-gutter2-disabled-modes`
to `non-nil`.

```lisp
;; inactivate git-gutter2-mode in asm-mode and image-mode
(custom-set-variables
 '(git-gutter2-disabled-modes '(asm-mode image-mode)))
```

Default is `nil`.


### Pass option to 'git diff' command

You can pass `git diff` option to set `git-gutter2-diff-option`.

```lisp
;; ignore all spaces
(custom-set-variables
 '(git-gutter2-diff-option "-w"))
```

### Don't ask whether commit/revert or not

`git-gutter2.el` always asks you whether commit/revert or not. If you don't want,
please set `git-gutter2-ask-p` to `nil`.

```lisp
;; Don't ask me!!
(custom-set-variables
 '(git-gutter2-ask-p nil))
```

### Log/Message Level

```lisp
;; Don't need log/message.
(custom-set-variables
 '(git-gutter2-verbosity 0))
```

Default value is 4(`0` is lowest, `4` is highest).

### Run hook

Run hook `git-gutter2-mode-on-hook` when `git-gutter2-mode` is turn on, and
run hook `git-gutter2-mode-off-hook` when `git-gutter2-mode` is turn off.

## Statistic

`git-gutter2.el` provides some statistic API. This is useful for knowing how much
code you changed etc. To display them in mode-line is also useful.

#### `(git-gutter2-buffer-hunks)`

Cound unstaged hunks in current buffer.

#### `(git-gutter2-all-hunks)`

Cound unstaged hunks in all buffers

#### `(git-gutter2-statistic)`

Return statistic unstaged hunks in current buffer. Return value is dot-list.
First element is total added lines, second element is total deleted lines.
