;;; buildbot-view.el --- buildbot.el UI -*- lexical-binding: t -*-

;; Copyright (C) 2023  Free Software Foundation, Inc.

;; Author: Yuchen Pei <id@ypei.org>

;; This file is part of buildbot.el.

;; buildbot.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; buildbot.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public
;; License along with buildbot.el. If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; buildbot.el UI.

;;; Code:

(require 'buildbot-utils)
(require 'buildbot-client)
(require 'text-property-search)

(defvar buildbot-view-header-regex "^\\[.*\\]$")
(defvar buildbot-view-branch-change-limit 10)
(defvar buildbot-view-builder-build-limit 50)
;; 'revision, 'build, 'step, or 'log
(defvar-local buildbot-view-type nil)
(defvar-local buildbot-view-data nil)

(defvar buildbot-view-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap (kbd "M-n") #'buildbot-view-next-header)
    (define-key kmap "n" #'buildbot-view-next-failed-header)
    (define-key kmap "f" #'buildbot-view-next-header-same-thing)
    (define-key kmap (kbd "M-p") #'buildbot-view-previous-header)
    (define-key kmap "p" #'buildbot-view-previous-failed-header)
    (define-key kmap (kbd "b") #'buildbot-view-previous-header-same-thing)
    (define-key kmap "g" #'buildbot-view-reload)
    (define-key kmap (kbd "<return>") #'buildbot-view-open-thing-at-point)
    kmap)
  "Keymap for `buildbot-view-mode'.")

(define-derived-mode buildbot-view-mode special-mode "Buildbot"
  "A Buildbot client for Emacs.")

(defun buildbot-view-next-header (n)
  "Move forward N headers."
  (interactive "p")
  (dotimes (_ n)
    (end-of-line 1)
    (re-search-forward buildbot-view-header-regex)
    (beginning-of-line 1)))

(defun buildbot-view-next-failed-header (n)
  "Move forward N headers with failed states."
  (interactive "p")
  (dotimes (_ n)
    (end-of-line 1)
    (text-property-search-forward 'face 'error)
    (beginning-of-line 1)))

(defun buildbot-view-next-header-same-thing (n)
  "Move forward N headers of the same type."
  (interactive "p")
  (when-let
      ((type (get-text-property (point) 'type)))
    (dotimes (_ n)
      (buildbot-view-next-header 1)
      (while (not (eq (get-text-property (point) 'type) type))
        (buildbot-view-next-header 1)))))

(defun buildbot-view-previous-header (n)
  "Move backward N headers."
  (interactive "p")
  (beginning-of-line 1)
  (unless (looking-at buildbot-view-header-regex)
    (re-search-backward buildbot-view-header-regex))
  (dotimes (_ n)
    (re-search-backward buildbot-view-header-regex)))

(defun buildbot-view-previous-failed-header (n)
  "Move back N headers of failed states."
  (interactive "p")
  (beginning-of-line 1)
  (unless (looking-at buildbot-view-header-regex)
    (re-search-backward buildbot-view-header-regex))
  (dotimes (_ n)
    (text-property-search-backward 'face 'error))
  (beginning-of-line 1))

(defun buildbot-view-previous-header-same-thing (n)
  "Move back N headers of the same type."
  (interactive "p")
  (when-let
      ((type (get-text-property (point) 'type)))
    (dotimes (_ n)
      (buildbot-view-previous-header 1)
      (while (not (eq (get-text-property (point) 'type) type))
        (buildbot-view-previous-header 1)))))

(defun buildbot-view-format-revision-info (revision-info)
  "Format REVISION-INFO header in the view."
  (propertize
   (format
    "[Revision %s]\nAuthor: %s\nDate: %s\n\n%s"
    (alist-get 'revision revision-info)
    (alist-get 'author revision-info)
    (alist-get 'created-at revision-info)
    (alist-get 'comments revision-info))
   'revision (alist-get 'revision revision-info) 'type 'revision))

(defun buildbot-view-format-build-stats (stats)
  "Format build STATS in the view."
  (format "Build stats: Success - %d | Failure - %d | Pending - %d"
          (alist-get 'success stats)
          (alist-get 'failure stats)
          (alist-get 'pending stats)))

(defun buildbot-view-format-build (revision build &optional show-revision)
  "Format a BUILD header associated with REVISION in the view.

With a non-nil SHOW-REVISION, display REVISION instead of the
builder name of the build."
  (propertize
   (format "\n[%s | %s]\n%s"
           (if show-revision
               revision
             (buildbot-get-builder-name-by-id (alist-get 'builderid build)))
           (propertize (alist-get 'state_string build)
                       'face (buildbot-status-face
                              (buildbot-build-status build)))
           (string-join
            (mapcar (lambda (test) (alist-get 'test_name test))
                    (alist-get 'failed_tests build))
            "\n"))
   'revision revision 'build build 'type 'build))

(defun buildbot-view-format-change-info (change-info &optional no-branch)
  "Format a CHANGE-INFO in the view.

With a non-nil NO-BRANCH, do not show branch info."
  (let ((revision (alist-get 'revision change-info)))
    (concat
     (unless no-branch
       (concat (buildbot-view-format-branch (alist-get 'branch change-info))
               "\n"))
     (buildbot-view-format-build-stats (alist-get 'build-stats change-info))
     "\n"
     (string-join
      (mapcar
       (lambda (build)
         (buildbot-view-format-build revision build))
       (alist-get 'builds change-info))
      "\n"))))

(defun buildbot-view-format-step (step)
  "Format a STEP header in the view."
  (propertize
   (format "\n[%d. %s | %s]\n"
           (alist-get 'number step)
           (alist-get 'name step)
           (propertize
            (alist-get 'state_string step)
            'face (buildbot-status-face
                   (buildbot-step-guess-status step))))
   'step step 'type 'step))

(defun buildbot-view-format-log (log)
  "Format a LOG header in the view."
  (propertize
   (format "\n[%s]\n"
           (alist-get 'name log))
   'log log 'type 'log))

(defun buildbot-revision-format (revision-and-changes-info &optional no-branch)
  "Format a revision view with REVISION-AND-CHANGES-INFO.

With a non-nil NO-BRANCH, do not show branch info."
  (let ((revision-info (alist-get 'revision-info revision-and-changes-info)))
    (concat
     (buildbot-view-format-revision-info revision-info)
     "\n\n"
     (string-join
      (mapcar (lambda (change-info)
                (buildbot-view-format-change-info change-info no-branch))
              (alist-get 'changes-info revision-and-changes-info))
      "\n"))))

(defun buildbot-view-format-branch (branch)
  "Format a BRANCH header in the view."
  (propertize
   (format "[Branch %s]" branch)
   'branch branch
   'type 'branch))

(defun buildbot-branch-format (branch changes)
  "Format a branch view with BRANCH and CHANGES info."
  (concat
   (buildbot-view-format-branch branch)
   "\n\n"
   (string-join
    (mapcar (lambda (change)
              (buildbot-revision-format
               (buildbot-get-revision-and-changes-info (list change))
               t))
            changes)
    "\n\n")))

(defun buildbot-view-format-builder (builder)
  "Format a BUILDER header in the view."
  (propertize
   (format "[Builder %s]" (alist-get 'name builder))
   'builder builder 'type 'builder))

(defun buildbot-builder-format (builder builds-with-revisions)
  "Format a builder view with info from BUILDER and BUILDS-WITH-REVISIONS."
  (concat
   (buildbot-view-format-builder builder)
   "\n\n"
   (string-join
    (mapcar
     (lambda (build-with-revision)
       (buildbot-view-format-build
        (elt (alist-get 'revision
                        (alist-get 'properties build-with-revision))
             0)
        (assq-delete-all 'properties build-with-revision)
        t))
     builds-with-revisions)
    "\n\n")))

(defun buildbot-build-format (revision-info build steps)
  "Format a build view with REVISION-INFO, BUILD and STEPS info."
  (concat
   (buildbot-view-format-revision-info revision-info)
   "\n"
   (buildbot-view-format-build (alist-get 'revision revision-info) build)
   "\n"
   (string-join
    (mapcar 'buildbot-view-format-step steps)
    "\n")))

(defun buildbot-step-format (revision-info build step logs)
  "Format a step view with REVISION-INFO, BUILD, STEP and LOGS info."
  (concat
   (buildbot-view-format-revision-info revision-info)
   "\n"
   (buildbot-view-format-build (alist-get 'revision revision-info) build)
   "\n"
   (buildbot-view-format-step step)
   "\n"
   (string-join
    (mapcar 'buildbot-view-format-log logs)
    "\n")))

(defun buildbot-log-format (revision-info build step log log-text)
  "Format a log view with REVISION-INFO, BUILD, STEP, LOG and LOG-TEXT."
  (concat
   (buildbot-view-format-revision-info revision-info)
   "\n"
   (buildbot-view-format-build (alist-get 'revision revision-info) build)
   "\n"
   (buildbot-view-format-step step)
   "\n"
   (buildbot-view-format-log log)
   "\n"
   log-text))

(defun buildbot-get-id-from-build (build)
  "Get the build id from BUILD."
  (or (alist-get 'id build)
      (alist-get 'buildid build)))

(defun buildbot-view-buffer-name (type data)
  "Get the buffer name of a view of TYPE with DATA."
  (pcase type
    ('branch (format "*buildbot branch %s*" (alist-get 'branch data)))
    ('revision (format "*buildbot revision %s*"
                       (alist-get 'revision data)))
    ('builder (format "*buildbot builder %s*"
                      (alist-get 'name
                                 (alist-get 'builder data))))
    ('build (format "*buildbot build %d*"
                    (buildbot-get-id-from-build
                     (alist-get 'build data))))
    ('step (format "*buildbot step %d*"
                   (alist-get 'stepid (alist-get 'step data))))
    ('log (format "*buildbot log %d*"
                  (alist-get 'logid (alist-get 'log data))))))

(defun buildbot-view-open (type data &optional force)
  "Open a view of TYPE using DATA.

With a non-nil FORCE, reload the view buffer if exists."
  (let ((buffer-name (buildbot-view-buffer-name type data)))
    (when (or force (not (get-buffer buffer-name)))
      (with-current-buffer (get-buffer-create buffer-name)
        (buildbot-view-mode)
        (setq buildbot-view-type type
              buildbot-view-data data)
        (buildbot-view-update)))
    (switch-to-buffer buffer-name)))

(defun buildbot-view-reload ()
  "Reload a view buffer."
  (interactive)
  (buildbot-view-update))

;;;###autoload
(defun buildbot-revision-open (revision)
  "Open a REVISION view."
  (interactive "sRevision (e.g. commit hash): ")
  (buildbot-view-open 'revision `((revision . ,revision))))

;;;###autoload
(defun buildbot-branch-open (branch)
  "Open a BRANCH view."
  (interactive "sBranch name: ")
  (buildbot-view-open 'branch `((branch . ,branch))))

;;;###autoload
(defun buildbot-builder-open (builder-name)
  "Open a builder view of BUILDER-NAME."
  (interactive (list (completing-read
                      "Builder name: "
                      (mapcar
                       (lambda (builder) (alist-get 'name builder))
                       buildbot-builders))))
  (buildbot-view-open 'builder
                      (list (cons 'builder
                                  (buildbot-builder-by-name builder-name)))))

(defun buildbot-view-update ()
  "Refresh a view."
  (unless (derived-mode-p 'buildbot-view-mode)
    (error "Not in buildbot view mode"))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (pcase buildbot-view-type
      ('branch
       (insert (buildbot-branch-format
                (alist-get 'branch buildbot-view-data)
                (buildbot-get-changes-by-branch
                 (alist-get 'branch buildbot-view-data)
                 buildbot-view-branch-change-limit))))
      ('revision
       (let ((revision-and-changes-info
              (buildbot-get-revision-and-changes-info
               (buildbot-get-changes-by-revision
                (alist-get 'revision buildbot-view-data)))))
         (setf (alist-get 'revision-info buildbot-view-data)
               (alist-get 'revision-info revision-and-changes-info))
         (insert (buildbot-revision-format revision-and-changes-info))))
      ('builder
       (let* ((builder (alist-get 'builder buildbot-view-data))
              (builds
               (buildbot-get-recent-builds-by-builder
                (alist-get 'builderid builder)
                buildbot-view-builder-build-limit)))
         (insert (buildbot-builder-format builder builds))))
      ('build
       (let ((revision (alist-get 'revision buildbot-view-data)))
         (unless (equal (alist-get 'revision
                                   (alist-get 'revision-info buildbot-view-data))
                        revision)
           (setf (alist-get 'revision-info buildbot-view-data)
                 (buildbot-get-revision-info-from-change
                  (elt
                   (buildbot-get-changes-by-revision revision)
                   0)))))
       (insert (buildbot-build-format
                (alist-get 'revision-info buildbot-view-data)
                (alist-get 'build buildbot-view-data)
                (buildbot-get-steps-by-buildid
                 (buildbot-get-id-from-build
                  (alist-get 'build buildbot-view-data))))))
      ('step
       (insert (buildbot-step-format
                (alist-get 'revision-info buildbot-view-data)
                (alist-get 'build buildbot-view-data)
                (alist-get 'step buildbot-view-data)
                (buildbot-get-logs-by-stepid
                 (alist-get 'stepid
                            (alist-get 'step buildbot-view-data))))))
      ('log
       (insert (buildbot-log-format
                (alist-get 'revision-info buildbot-view-data)
                (alist-get 'build buildbot-view-data)
                (alist-get 'step buildbot-view-data)
                (alist-get 'log buildbot-view-data)
                (buildbot-api-log-raw
                 (alist-get 'logid
                            (alist-get 'log buildbot-view-data)))))))
    (goto-char (point-min))))

(defun buildbot-view-open-thing-at-point (force)
  "Open thing at point.

With a non-nil FORCE, refresh the opened buffer if exists."
  (interactive "P")
  (let ((data (copy-tree buildbot-view-data)))
    (pcase (get-text-property (point) 'type)
      ('branch
       (setf (alist-get 'branch data)
             (get-text-property (point) 'branch))
       (buildbot-view-open 'branch data force))
      ('revision
       (setf (alist-get 'revision data)
             (get-text-property (point) 'revision))
       (buildbot-view-open 'revision data force))
      ('build
       (setf (alist-get 'build data)
             (get-text-property (point) 'build)
             (alist-get 'revision data)
             (get-text-property (point) 'revision))
       (buildbot-view-open 'build data force))
      ('step
       (setf (alist-get 'step data)
             (get-text-property (point) 'step))
       (buildbot-view-open 'step data force))
      ('log
       (setf (alist-get 'log data)
             (get-text-property (point) 'log))
       (buildbot-view-open 'log data force)))))

(provide 'buildbot-view)
;;; buildbot-view.el ends here
