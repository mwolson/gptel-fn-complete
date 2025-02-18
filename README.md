# gptel-fn-complete

Rewriting completion of function at point using
[gptel](https://github.com/karthink/gptel) in Emacs.

This uses the existing `gptel-rewrite.el` library to perform completion on an
entire function, replacing what's already written so far in that function in a
way that prefers to complete the end of the function, but may also apply small
changes to the original function.

## Setup

To use:

* Install [gptel](https://github.com/karthink/gptel), configure it, and provide
  the appropriate API keys.
* Clone this repo to a location like `~/gptel-fn-complete`, or install
  `gptel-fn-complete` on MELPA.
* When not using MELPA: Also add the following to your Emacs configuration
  (typically `~/.emacs.d/init.el` or similar):
  ```elisp
  (add-to-list 'load-path (expand-file-name "~/gptel-fn-complete"))
  (autoload #'gptel-fn-complete "gptel-fn-complete" "Complete function at point using an LLM." t)
  ```
* Optionally, let `gptel-fn-complete` and `gptel-rewrite` automatically apply
  the code completion from the LLM and skip the extra prompt about it:
  ```elisp
  (setq gptel-rewrite-default-action 'accept)
  ```
* Now choose which key you'd like to bind `gptel-fn-complete` to. I typically
  add something like this to my Emacs config:
  ```elisp
  (defvar my-xref-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "c") #'gptel-fn-complete)
      (define-key map (kbd ".") #'xref-find-definitions)
      (define-key map (kbd ",") #'xref-go-back)
      (define-key map (kbd "/") #'xref-find-references)
      map)
    "My key customizations for AI and xref.")

  (global-set-key (kbd "C-c .") my-xref-map)
  ```
* Restart Emacs

## Usage

If you've used the above keybinds, they work like this (the only with AI is the
first one):

* <kbd>C-c . c</kbd> to complete the code at point using Claude AI; if you have
  a comment near the end, that will better inform the completion
* <kbd>C-c . .</kbd> to visit the definition of the thing at point
* <kbd>C-c . ,</kbd> to return to the original point after visiting something
* <kbd>C-c . /</kbd> to find references to the thing at point

## Example

When I write this code in a `sample.el` file:

```elisp
(defun my-code ()
  "AI should not modify this."
  (message "Sample 1"))

(defun my-hello
;; print a welcoming message in a window off to the right
)

(defun my-other-code ()
  "AI should not modify this either."
  (message "Sample 2"))
```

Move the cursor into the body of `my-hello` and hit <kbd>C-c . c</kbd> then
gptel will rewrite that `my-hello` function to something like this, without
touching the other functions or deleting lines around it (results may vary, I
used Claude 3.5 Sonnet in this example):

```elisp
(defun my-hello ()
  "Print a welcoming message in a window off to the right."
  (let ((buf (get-buffer-create "*Hello*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "Welcome to Emacs!\n\nHave a productive session."))
    (display-buffer buf
                    '((display-buffer-reuse-window
                       display-buffer-in-side-window)
                      (side . right)
                      (window-width . 40)))))
```

From here, you can use the standard `gptel-rewrite` keys like `C-c C-a` on that
code to accept it and remove the overlay on it.

Note that the function must have balanced parentheses, otherwise the code will
throw an error. This is to make it easier to locate the beginning and end of the
function to send to gptel's context.

## Inspiration

After adding a function to gptel's context, I was using `gptel-rewrite` and
accidentally hit Enter twice.  This resulted in just the basic "Rewrite: " text
being sent, and to my surprise that was very effective at having Claude fix the
problem I was going to ask about.

I decided to see if Claude could also do code completions this way, with a very
terse kind of prompt on top of the standard `gptel-rewrite` prompt, and it turns
out that it can!

## Comparison with other Emacs AI modes

* [elysium](https://github.com/lanceberge/elysium) - `elysium` sends either the
  entire file or a selected region to the LLM. It also shows a window on the
  side with the LLM response, and includes smerge markers into the buffer with
  the result from the LLM.

  `gptel-fn-complete` uses a standard `gptel-rewrite` overlay on the code result
  instead of using the more visually-busy smerge markers. It can be configured
  using `(setq gptel-rewrite-default-action 'accept)` to automatically skip the
  `gptel-rewrite` overlay and directly insert the text. It does not change the
  window layout to show a chat buffer.

* [minuet](https://github.com/milanglacier/minuet-ai.el) - `minuet` typically
  does a "fill-in-the-middle" style completion that doesn't modify the code
  before the point. It also has an optional auto-suggestion capability as you
  type.

  `gptel-fn-complete` allows a partial rewrite of the entire function, if
  the LLM thinks there is stylistic value in the rewrite, and is typically
  manually triggered by a keybind.

## Notes

* I've tested this with Claude, Gemini 2.0 Flash (which may still sometimes
  include unwanted code fences), and a few local LLMs.
* For automatically identifying the entire current function to complete, you may
  have the best luck with either Emacs Lisp or files with major modes that have
  a tree-sitter grammar installed, as otherwise we have to guess.  In general it
  should err on the side of sending too little rather than too much.
* My Emacs setup is available at https://github.com/mwolson/emacs-shared which
  has this and other features
  * Note that the install doc might take a while to get through, and may have
    opinionated settings
  * The additional AI features which have more bindings on <kbd>C-c .</kbd> than
    in the above example
    [are described here](https://github.com/mwolson/emacs-shared/blob/master/doc/tips.md#using-ai-and-finding-definitions)
* `karthink` and other gptel contributors may use the code in this repo freely
  and reassign copyright to themselves as need be if they would like.
