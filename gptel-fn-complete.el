;;; gptel-fn-complete.el --- Function at point completion for gptel  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Michael Olson
;; Copyright (C) 2024  Karthik Chikmagalur

;; Author: Michael Olson <mwolson@gnu.org>
;; Version: 0.1.0
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

;;

;;; Code:
(require 'gptel)
(require 'gptel-rewrite)

(declare-function treesit-beginning-of-defun "treesit")
(declare-function treesit-end-of-defun "treesit")

(defvar gptel-fn-complete-extra-directive "Complete at end:\n\n%s")
(defvar gptel-fn-complete-directive 'gptel-fn-complete-directive-default)

(defun gptel-fn-complete-directive-default ()
  "Generic directive for rewriting or refactoring.

These are instructions not specific to any particular required
change.

The returned string is interpreted as the system message for the
rewrite request.  To use your own, add a different directive to
`gptel-directives', or add to `gptel-rewrite-directives-hook',
which see."
  (or (save-mark-and-excursion
        (run-hook-with-args-until-success
         'gptel-rewrite-directives-hook))
      (let* ((lang (downcase (gptel--strip-mode-suffix major-mode)))
             (article (if (and lang (not (string-empty-p lang))
                               (memq (aref lang 0) '(?a ?e ?i ?o ?u)))
                          "an" "a")))
        (if (derived-mode-p 'prog-mode)
            (format (concat "You are %s %s programmer.  "
                            "Follow my instructions and refactor %s code I provide.\n"
                            "- Generate ONLY %s code as output, without any explanation.\n"
                            "- Do not abbreviate or omit code.\n"
                            "- Write only a single function.\n"
                            "- It is acceptable to slightly adjust the existing "
                            "function for readability.\n"
                            "- Give me the final and best answer only.\n"
                            "- Do not ask for further clarification, and make "
                            "any assumptions you need to follow instructions.\n"
                            "- Never include markdown code fences like \"```\" in "
                            "the output.")
                    article lang lang lang)
          (concat
           (if (string-empty-p lang)
               "You are an editor."
             (format "You are %s %s editor." article lang))
           "  Follow my instructions and improve or rewrite the text I provide."
           "  Generate ONLY the replacement text,"
           " without any explanation."
           "  Write only a single paragraph or function."
           "  It is acceptable to slightly adjust the existing"
           " function for readability."
           "  Never include markdown code fences like \"```\" in"
           " the output.")))))

(defun gptel-fn-complete--mark-function-default (&optional steps)
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
  (treesit-end-of-defun)
  (let ((pt-max (point)))
    (treesit-beginning-of-defun)
    (setq steps (1- (- 0 (or steps 0))))
    (while (> steps 0)
      (treesit-beginning-of-defun)
      (cl-decf steps))
    (set-mark (point))
    (goto-char pt-max)))

(defun gptel-fn-complete--mark-function (&optional steps)
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
    (gptel-fn-complete--mark-function-default steps)
    (when (< (region-beginning) pt-min)
      (setq pt-min (region-beginning)
            pt-max (region-end)))
    (goto-char pt-min)
    (while (and (looking-at-p "[[:space:]\r\n]")
                (< (point) pt-max))
      (forward-char))
    (setq pt-min (point))
    (goto-char (1- pt-max))
    (push-mark pt-min nil t)))

;;;###autoload (autoload 'gptel-fn-complete "gptel-fn-complete" nil t)
(defun gptel-fn-complete ()
  "Complete function at point using an LLM.

Either the last function or the current region will be used for context."
  (interactive)
  (gptel-fn-complete--mark-function)
  (gptel-fn-complete-send))

(defun gptel-fn-complete-send ()
  "Complete using an LLM."
  (let* ((nosystem (gptel--model-capable-p 'nosystem))
         ;; Try to send context with system message
         (gptel-use-context
          (and gptel-use-context (if nosystem 'user 'system)))
         (prompt (list ""
                       "What is the required change?"
                       (format gptel-fn-complete-extra-directive
                               (or (get-char-property (point) 'gptel-rewrite)
                                   (buffer-substring-no-properties (region-beginning) (region-end))))))
         (buffer (current-buffer)))
    (deactivate-mark)
    (when nosystem
      (setcar prompt (concat (car-safe (gptel--parse-directive
                                        #'gptel-fn-complete-directive-default 'raw)))))
    (gptel-request prompt
      :dry-run nil
      :system #'gptel-fn-complete-directive-default
      :stream gptel-stream
      :context
      (let ((ov (or (cdr-safe (get-char-property-and-overlay (point) 'gptel-rewrite))
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
