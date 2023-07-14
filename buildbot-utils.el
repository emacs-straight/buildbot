;;; buildbot-utils.el --- Commonly used utilities. -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Free Software Foundation, Inc.
;;
;; This file is part of buildbot.el.
;;
;; buildbot.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; buildbot.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public
;; License along with buildbot.el. If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Commonly used utilities.

;;; Code:
(require 'json)

(defvar buildbot-client-buffer-name "*buildbot api*"
  "Name of the buffer recording buildbot API calls.")

(defun buildbot-parse-http-header (text)
  "Parse the http header TEXT."
  (let ((status) (fields))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (re-search-forward "^HTTP.*\\([0-9]\\{3\\}\\).*$")
      (setq status (match-string 1))
      (while (re-search-forward "^\\(.*?\\): \\(.*\\)$" nil t)
        (push (cons (intern (match-string 1)) (match-string 2)) fields)))
    (list (cons 'status status) (cons 'fields fields))))

(defun buildbot-delete-http-header ()
  "Delete the http header from a response buffer."
  (save-excursion
    (goto-char (point-min))
    (kill-region (point) (progn (re-search-forward "\r?\n\r?\n")
                                (point)))))

(defun buildbot-url-fetch-internal (url processor &optional
                                        decompression with-header)
  "Fetch from URL and process the response payload using PROCESSOR.

PROCESSOR is a function that takes no argument and processes the
current buffer.
With non-nil DECOMPRESSION, decompress the response.
With non-nil WITH-HEADER, include the header in the result."
  (with-current-buffer (get-buffer-create buildbot-client-buffer-name)
    (goto-char (point-max))
    (insert "[" (current-time-string) "] Request: " url "\n"))
  (with-current-buffer (url-retrieve-synchronously url t)
    (let ((header) (status) (fields))
      (buildbot-delete-http-header)
      (goto-char (point-min))
      (setq header (buildbot-parse-http-header (car kill-ring))
            status (alist-get 'status header)
            fields (alist-get 'fields header))
      (with-current-buffer buildbot-client-buffer-name
        (insert "[" (current-time-string) "] Response: " status "\n"))
      (when decompression
        (call-process-region (point) (point-max) "gunzip" t t t)
        (goto-char (point-min)))
      (call-interactively 'delete-trailing-whitespace)
      (if (string= status "200")
          (unless (= (point) (point-max))
            (if with-header
                (list
                 (cons 'header fields)
                 (cons 'json (funcall processor)))
              (funcall processor)))
        (error "HTTP error: %s" (buffer-substring (point) (point-max)))))))

(defun buildbot-url-fetch-json (url &optional decompression with-header)
  "Fetch and parse a json object from URL.

With non-nil DECOMPRESSION, decompress the response.
With non-nil WITH-HEADER, include the header in the result."
  (buildbot-url-fetch-internal url 'json-read decompression with-header))

(defun buildbot-url-fetch-raw (url &optional decompression with-header)
  "Fetch from URL.

With non-nil DECOMPRESSION, decompress the response.
With non-nil WITH-HEADER, include the header in the result."
  (buildbot-url-fetch-internal url 'buffer-string decompression
                               with-header))

(defun buildbot-format-attr (attr)
  "Format an alist ATTR into a url query string."
  (string-join (mapcar (lambda (pair)
                         (format "%s=%s" (car pair) (cdr pair)))
                       attr)
               "&"))

(defun buildbot-format-epoch-time (epoch)
  "Format an EPOCH."
  (format-time-string "%Y-%m-%d %a %H:%M:%S %Z" (encode-time
                                                 (decode-time epoch))))


(defun buildbot-build-status (build)
  "Get the status of a BUILD."
  (let ((state (alist-get 'state_string build)))
    (cond ((equal state "build successful")
           'success)
          ((string-suffix-p "(failure)" state)
           'failure)
          (t 'pending))))

(defun buildbot-step-guess-status (step)
  "Guess the status of a STEP."
  (let ((state (alist-get 'state_string step)))
    (cond ((string-suffix-p "(warnings)" state)
           'pending)
          ((string-suffix-p "(failure)" state)
           'failure)
          ((string-suffix-p "done" state)
           'success)
          ((string-suffix-p "ing" state)
           'pending)
          ((string-suffix-p "finished" state)
           'success)
          (t 'success))))

(defun buildbot-status-face (status)
  "Get the face of STATUS."
  (pcase status
    ('success 'success)
    ('failure 'error)
    (_ 'warning)))

(defun buildbot-get-build-stats (builds)
  "Get the aggregated build stats of BUILDS."
  (let ((results (copy-tree '((success . 0)
                              (failure . 0)
                              (pending . 0))))
        (status))
    (seq-do
     (lambda (build)
       (setq status (buildbot-build-status build))
       (setf (alist-get status results)
             (1+ (alist-get status results))))
     builds)
    results))

(defun buildbot-get-revision-info-from-change (change)
  "Get the revision info from a CHANGE."
  (list
   (assq 'revision change)
   (assq 'author change)
   (cons 'created-at
         (buildbot-format-epoch-time
          (alist-get 'when_timestamp change)))
   (assq 'comments change)))

(defun buildbot-get-revision-and-changes-info (changes)
  "Get the revision-info and builds from a set of CHANGES.

The changes should be of the same revision."
  (let* ((first-change (elt changes 0))
         (revision-info (buildbot-get-revision-info-from-change first-change))
         (changes-info
          (mapcar (lambda (change)
                    (list
                     (assq 'branch change)
                     (assq 'builds change)
                     (cons 'build-stats
                           (buildbot-get-build-stats
                            (alist-get 'builds change)))
                     (assq 'revision first-change)))
                  changes)))
    `((revision-info . ,revision-info) (changes-info . ,changes-info))))

(provide 'buildbot-utils)
;;; buildbot-utils.el ends here
