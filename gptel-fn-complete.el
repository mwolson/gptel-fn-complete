;;; gptel-fn-complete.el --- Complete the function at point using gptel  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Michael Olson
;; Copyright (C) 2024  Karthik Chikmagalur

;; Author: Michael Olson <mwolson@gnu.org>
;; Maintainer: Michael Olson <mwolson@gnu.org>
;; Version: 0.3.1
;; URL: https://github.com/mwolson/gptel-fn-complete
;; Package-Requires: ((emacs "29.1") (gptel "0.9.7"))
;; Keywords: hypermedia, convenience, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Rewriting completion of function at point using gptel in Emacs.
;;
;; This uses the existing `gptel-rewrite.el` library to perform completion on an
;; entire function, replacing what's already written so far in that function in
;; a way that prefers to complete the end of the function, but may also apply
;; small changes to the original function.
;;
;; To use this library, install both gptel and gptel-fn-complete, and then bind
;; `gptel-fn-complete` to your key of choice.

;;; Code:
(require 'gptel)
(require 'gptel-rewrite)

(declare-function treesit-beginning-of-defun "treesit")
(declare-function treesit-end-of-defun "treesit")

(defgroup gptel-fn-complete nil
  "Complete the function at point using gptel."
  :group 'gptel)

(defcustom gptel-fn-complete-extra-directive "Complete at end:\n\n%s"
  "Directive to use as the last user message when sending completion prompts.

`format' will be called on it with the string contents of the function as the
only argument."
  :group 'gptel-fn-complete
  :type 'string)

(defcustom gptel-fn-complete-function-directive
  'gptel-fn-complete--function-directive-default
  "Active system message for function completion actions.

These are instructions not specific to any particular required change.

The returned string is interpreted as the system message for the
completion request.  To use your own, customize this option or (to affect
gptel rewrites as well) add to `gptel-rewrite-directives-hook'."
  :group 'gptel-fn-complete
  :type '(function :tag "Function that returns a directive string"))

(defcustom gptel-fn-complete-region-directive
  'gptel-fn-complete--region-directive-default
  "Active system message for region completion actions.

These are instructions not specific to any particular required change.

The returned string is interpreted as the system message for the
completion request.  To use your own, customize this option or (to affect
gptel rewrites as well) add to `gptel-rewrite-directives-hook'."
  :group 'gptel-fn-complete
  :type '(function :tag "Function that returns a directive string"))

(defun gptel-fn-complete--prog-mode-system-prompt (single-function-p)
  "Return a system prompt for gptel-fn-complete for `prog-mode' buffers.

If SINGLE-FUNCTION-P is non-nil, encourage the LLM to return a single
function."
  (let* ((lang (downcase (gptel--strip-mode-suffix major-mode)))
         (article (if (and lang (not (string-empty-p lang))
                           (memq (aref lang 0) '(?a ?e ?i ?o ?u)))
                      "an" "a")))
    (concat (format "You are %s %s programmer.  " article lang)
            (format "Follow my instructions and refactor %s " lang)
            "code that I provide.\n"
            (format "- Generate ONLY %s code as output, " lang)
            "without any explanation.\n"
            "- Do not abbreviate or omit code.\n"
            (if single-function-p "- Write only a single function.\n" "")
            "- It is acceptable to slightly adjust the existing "
            (if single-function-p "function" "code")
            " for readability.\n"
            "- Give me the final and best answer only.\n"
            "- Do not ask for further clarification, and make "
            "any assumptions you need to follow instructions.\n"
            "- NEVER include markdown code fences like \"```\" in "
            "the output.")))

(defun gptel-fn-complete--normal-system-prompt (single-function-p)
  "Return a system prompt for gptel-fn-complete for `prog-mode' buffers.

If SINGLE-FUNCTION-P is non-nil, encourage the LLM to return a single
function."
  (let* ((lang (downcase (gptel--strip-mode-suffix major-mode)))
         (article (if (and lang (not (string-empty-p lang))
                           (memq (aref lang 0) '(?a ?e ?i ?o ?u)))
                      "an" "a")))
    (concat
     (if (string-empty-p lang)
         "You are an editor."
       (format "You are %s %s editor." article lang))
     "  Follow my instructions and improve or rewrite the text I provide."
     "  Generate ONLY the replacement text,"
     " without any explanation."
     (if single-function-p "  Write only a single paragraph or function." "")
     "  It is acceptable to slightly adjust the existing"
     (if single-function-p " function" " code")
     " for readability."
     "  NEVER include markdown code fences like \"```\" in"
     " the output.")))

(defun gptel-fn-complete--function-directive-default ()
  "Generic directive for function completion."
  (or (save-mark-and-excursion
        (run-hook-with-args-until-success
         'gptel-rewrite-directives-hook))
      (if (derived-mode-p 'prog-mode)
          (gptel-fn-complete--prog-mode-system-prompt t)
        (gptel-fn-complete--normal-system-prompt t))))

(defun gptel-fn-complete--region-directive-default ()
  "Generic directive for function completion."
  (or (save-mark-and-excursion
        (run-hook-with-args-until-success
         'gptel-rewrite-directives-hook))
      (if (derived-mode-p 'prog-mode)
          (gptel-fn-complete--prog-mode-system-prompt nil)
        (gptel-fn-complete--normal-system-prompt nil))))

(defun gptel-fn-complete--mark-function-or-para (&optional steps)
  "Put mark at end of this function or paragraph, point at beginning.

If STEPS is negative, mark `- arg - 1` extra functions backward.
The behavior for when STEPS is positive is not currently well-defined."
  (let ((pt-min (point))
        (pt-mid (point))
        (pt-max (point)))
    (save-mark-and-excursion
      (ignore-errors
        (mark-defun steps)
        (setq pt-min (region-beginning)
              pt-max (region-end))))
    (save-mark-and-excursion
      (mark-paragraph steps)
      (when (<= (region-beginning) pt-min)
        (when (save-excursion
                (goto-char pt-mid)
                (beginning-of-line)
                (looking-at-p "[[:space:]]*$"))
          (forward-paragraph 1))
        (setq pt-min (region-beginning)
              pt-max (max pt-max (region-end)))))
    (set-mark pt-min)
    (goto-char pt-max)))

(defun gptel-fn-complete--mark-function-treesit (&optional steps)
  "Put mark at end of this function, point at beginning, for a treesit mode.

If STEPS is negative, mark `- arg - 1` extra functions backward.
The behavior for when STEPS is positive is not currently well-defined."
  (treesit-end-of-defun)
  (let ((pt-max (point)))
    (treesit-beginning-of-defun 1)
    (setq steps (1- (- 0 (or steps 0))))
    (while (> steps 0)
      (treesit-beginning-of-defun 1)
      (cl-decf steps))
    (set-mark (point))
    (goto-char pt-max)))

;;;###autoload
(defun gptel-fn-complete-mark-function (&optional steps)
  "Put mark at end of this function, point at beginning.

If STEPS is negative, mark `- arg - 1` extra functions backward.
The behavior for when STEPS is positive is not currently well-defined."
  (interactive)
  (let ((pt-min (point))
        (pt-max nil))
    (when (null steps) (setq steps -1))
    (when (treesit-parser-list)
      (save-mark-and-excursion
        (gptel-fn-complete--mark-function-treesit steps)
        (setq pt-min (region-beginning)
              pt-max (region-end))))
    (gptel-fn-complete--mark-function-or-para steps)
    (when (< (region-beginning) pt-min)
      (setq pt-min (region-beginning)
            pt-max (region-end)))
    (goto-char pt-min)
    (while (and (looking-at-p "[[:space:]\r\n]")
                (< (point) pt-max))
      (forward-char))
    (setq pt-min (point))
    (goto-char pt-max)
    (save-match-data
      (while (and (looking-back "[[:space:]\r\n]" (1- (point)))
                  (> (point) pt-min))
        (backward-char)))
    (push-mark pt-min nil t)))

;;;###autoload
(defun gptel-fn-complete ()
  "Complete function at point using an LLM.

Either the function at point or the current region will be used for context,
along with any other context that has already been added to gptel."
  (interactive)
  (let ((fn-p (not (region-active-p))))
    (when fn-p
      (gptel-fn-complete-mark-function))
    (gptel-fn-complete-send fn-p)))

;;;###autoload
(defun gptel-fn-complete-send (single-function-p)
  "Complete region using an LLM.

If SINGLE-FUNCTION-P is non-nil, encourage the LLM to return a single
function."
  (let* ((nosystem (gptel--model-capable-p 'nosystem))
         ;; Try to send context with system message
         (gptel-use-context
          (and gptel-use-context (if nosystem 'user 'system)))
         (prompt (list nil
                       "What is the required change?"
                       (format gptel-fn-complete-extra-directive
                               (or (get-char-property (point) 'gptel-rewrite)
                                   (buffer-substring-no-properties
                                    (region-beginning) (region-end))))))
         (buffer (current-buffer))
         (directive (if single-function-p
                        gptel-fn-complete-function-directive
                      gptel-fn-complete-region-directive)))
    (deactivate-mark)
    (when nosystem
      (setcar prompt (car-safe (gptel--parse-directive directive 'raw))))
    (gptel-request prompt
      :dry-run nil
      :system directive
      :stream gptel-stream
      :context
      (let ((ov (or (cdr-safe (get-char-property-and-overlay
                               (point) 'gptel-rewrite))
                    (make-overlay (region-beginning) (region-end) nil t))))
        (overlay-put ov 'category 'gptel)
        (overlay-put ov 'evaporate t)
        (cons ov (generate-new-buffer "*gptel-fn-complete*")))
      :callback `(lambda (&rest args)
                   (apply #'gptel--rewrite-callback args)
                   (with-current-buffer ,buffer
                     (backward-char))))))

(provide 'gptel-fn-complete)
;;; gptel-fn-complete.el ends here

;; Local Variables:
;; outline-regexp: "^;; \\*+"
;; End:
