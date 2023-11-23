;;; completion-preview.el --- Preview completion with inline overlay  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Eshel Yaron

;; Author: Eshel Yaron <me@eshelyaron.com>
;; Maintainer: Eshel Yaron <~eshel/dev@lists.sr.ht>
;; Keywords: convenience completion
;; URL: https://git.sr.ht/~eshel/completion-preview
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "27.2"))

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

;; This library provides the Completion Preview mode.  This minor mode
;; displays the top completion candidate for the symbol at point in an
;; overlay after point.  If you want to enable Completion Preview mode
;; in all programming modes, add the following to your Emacs init:
;;
;;     (add-hook 'prog-mode-hook #'completion-preview-mode)
;;
;; Also check out the customization group `completion-preview` for
;; some user options that you may want to tweak.

;;; Code:

(defgroup completion-preview nil
  "In-buffer completion preview."
  :group 'completion)

(defcustom completion-preview-exact-match-only nil
  "Whether to show completion preview only when there is exact match.

If this option is non-nil, Completion Preview mode only shows the
preview overlay when there is exactly one completion candidate
that matches the symbol at point, otherwise it shows the top
candidate also when there are multiple matching candidates."
  :type 'boolean)

(defcustom completion-preview-commands '(self-insert-command)
  "List of commands that should trigger completion preview."
  :type '(repeat (function :tag "Command" :value self-insert-command)))

(defcustom completion-preview-minimum-symbol-length 3
  "Minimum length of the symbol at point for showing completion preview."
  :type 'natnum)

(defcustom completion-preview-hook
  '(completion-preview-require-certain-commands
    completion-preview-require-minimum-symbol-length)
  "Hook for functions that determine whether to show preview completion.

Completion Preview mode calls each of these functions in order
after each command, and only displays the completion preview when
all of the functions return non-nil."
  :type 'hook)

(defvar completion-preview-sort-function #'completion-preview--sort-by-length-alpha
  "Sort function to use for choosing a completion candidate to preview.")

(defface completion-preview
  '((t :inherit shadow))
  "Face for completion preview overlay.")

(defface completion-preview-exact
  '((t :underline t :inherit completion-preview))
  "Face for exact completion preview overlay.")

(defvar-local completion-preview--overlay nil)

(defvar-local completion-preview--skip nil)

(defun completion-preview--sort-by-length-alpha (elems)
  "Sort ELEMS first by length, then alphabetically.

Compatibility definition for `minibuffer--sort-by-length-alpha'."
  (sort elems (lambda (c1 c2)
                (or (< (length c1) (length c2))
                    (and (= (length c1) (length c2))
                         (string< c1 c2))))))

(defun completion-preview-require-certain-commands ()
  "Check if `this-command' is one of `completion-preview-commands'."
  (memq this-command completion-preview-commands))

(defun completion-preview-require-minimum-symbol-length ()
  "Check if the length of symbol at point is at least above a certain threshold.
`completion-preview-minimum-symbol-length' determines that threshold."
  (pcase (bounds-of-thing-at-point 'symbol)
    (`(,beg . ,end)
     (<= completion-preview-minimum-symbol-length (- end beg)))))

(defun completion-preview-hide ()
  "Hide the completion preview."
  (when completion-preview--overlay
    (delete-overlay completion-preview--overlay)
    (setq completion-preview--overlay nil)))

(defun completion-preview--make-overlay (pos string)
  "Make a new completion preview overlay at POS showing STRING."
  (completion-preview-hide)
  (add-text-properties 0 1 '(cursor 1) string)
  (setq completion-preview--overlay (make-overlay pos pos))
  (overlay-put completion-preview--overlay 'after-string string)
  completion-preview--overlay)

(define-minor-mode completion-preview-active-mode
  "Mode for when the completion preview is active."
  :interactive nil
  (if completion-preview-active-mode
      (add-hook 'completion-at-point-functions #'completion-preview-insert -1 t)
    (remove-hook 'completion-at-point-functions #'completion-preview-insert t)
    (completion-preview-hide)))

(defun completion-preview--exit-function (func)
  (lambda (&rest args)
    (completion-preview-active-mode -1)
    (when func (apply func args))))

(defun completion-preview--update ()
  "Update completion preview."
  (pcase (let ((completion-preview--skip t))
           (run-hook-with-args-until-success 'completion-at-point-functions))
    (`(,beg ,end ,table . ,plist)
     (let* ((pred (plist-get plist :predicate))
            (exit-fn (completion-preview--exit-function
                      (plist-get plist :exit-function)))
            (string (buffer-substring beg end))
            (md (completion-metadata string table pred))
            (sort-fn (or (completion-metadata-get md 'cycle-sort-function)
                         (completion-metadata-get md 'display-sort-function)
                         completion-preview-sort-function))
            (all (completion-all-completions string table pred
                                             (- (point) beg) md))
            (last (last all))
            (base (or (cdr last) 0))
            (bbeg (+ beg base))
            (prefix (substring string base)))
       (when last
         (setcdr last nil)
         (let* ((filtered
                 (seq-filter (apply-partially #'string-prefix-p prefix) all))
                (sorted (funcall sort-fn filtered))
                (multi (cadr sorted))   ; multiple candidates
                (cand (car sorted)))
           (when (and cand (not (and multi completion-preview-exact-match-only)))
             (let* ((face (if multi 'completion-preview 'completion-preview-exact))
                    (after (propertize (substring cand (length prefix)) 'face face)))
               (unless (string-empty-p after)
                 (overlay-put (completion-preview--make-overlay end after)
                              'completion-preview-data
                              (list bbeg end (list cand)
                                    :exit-function exit-fn))
                 (completion-preview-active-mode))))))))))

(defun completion-preview--show ()
  "Show completion preview."
  (when completion-preview-active-mode
    (let* ((data (overlay-get completion-preview--overlay 'completion-preview-data))
           (beg (car data))
           (cands (caddr data))
           (cand (car cands))
           (plist (cdddr data))
           (len (length cand))
           (end (+ beg len))
           (after (overlay-get completion-preview--overlay 'after-string))
           (face (get-text-property 0 'face after)))
      (if (and (< beg (point) end)
               (string-prefix-p (buffer-substring beg (point)) cand))
          (overlay-put
           (completion-preview--make-overlay
            (point) (propertize (substring cand (- (point) beg)) 'face face))
           'completion-preview-data (append (list beg (point) cands) plist))
        (completion-preview-active-mode -1))))
  (while-no-input (completion-preview--update)))

(defun completion-preview--post-command ()
  "Create, update or delete completion preview post last command."
  (if (run-hook-with-args-until-failure 'completion-preview-hook)
      (completion-preview--show)
    (completion-preview-active-mode -1)))

(defun completion-preview-insert ()
  "Completion at point function for inserting the current preview."
  (when (and completion-preview-active-mode (not completion-preview--skip))
    (overlay-get completion-preview--overlay 'completion-preview-data)))

;;;autoload
(define-minor-mode completion-preview-mode
  "Show in-buffer completion preview as you type."
  :lighter " CP"
  (if completion-preview-mode
      (add-hook 'post-command-hook #'completion-preview--post-command nil t)
    (remove-hook 'post-command-hook #'completion-preview--post-command t)
    (completion-preview-active-mode -1)))

(provide 'completion-preview)
;;; completion-preview.el ends here
