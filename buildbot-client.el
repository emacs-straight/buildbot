;;; buildbot-client.el --- Client code using buildbot api -*- lexical-binding: t; -*-

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

;; buildbot-client provides functions for buildbot.el to get stuff
;; from a buildbot server.

;;; Code:
(require 'buildbot-utils)
(require 'cl-seq)

(defvar buildbot-host nil "Buildbot instance host.")
(defvar buildbot-builders nil
  "Buildbot builders. Can be generated with `(buildbot-get-all-builders)'.")

(defun buildbot-api-change (attr)
  "Call the Changes API with ATTR."
  (buildbot-url-fetch-json
   (format
    "%s/api/v2/changes?%s"
    buildbot-host (buildbot-format-attr attr))))

(defun buildbot-api-logs (stepid)
  "Call the Logs API with STEPID."
  (buildbot-url-fetch-json
   (format
    "%s/api/v2/steps/%d/logs"
    buildbot-host stepid)))

(defun buildbot-api-builders ()
  "Call the Builders API to get all builders."
  (buildbot-url-fetch-json
   (format
    "%s/api/v2/builders"
    buildbot-host)))

(defun buildbot-api-builders-builds (builder-id attr)
  "Call the Builds API with BUILDER-ID and ATTR."
  (buildbot-url-fetch-json
   (format
    "%s/api/v2/builders/%d/builds?%s"
    buildbot-host builder-id (buildbot-format-attr attr))))

(defun buildbot-api-build (attr)
  "Call the Builds API with ATTR."
  (buildbot-url-fetch-json
   (format
    "%s/api/v2/builds?%s"
    buildbot-host (buildbot-format-attr attr))))

(defun buildbot-api-step (buildid)
  "Call the Steps API with BUILDID."
  (buildbot-url-fetch-json
   (format
    "%s/api/v2/builds/%s/steps"
    buildbot-host buildid)))

(defun buildbot-api-log-raw (logid)
  "Call the raw logs API with LOGID."
  (buildbot-url-fetch-raw
   (format "%s/api/v2/logs/%d/raw" buildbot-host logid)))

(defun buildbot-get-recent-builds-by-builder (builder-id limit)
  "Get LIMIT number of recent builds with BUILDER-ID."
  (alist-get 'builds
             (buildbot-api-builders-builds
              builder-id
              `((limit . ,limit) (order . "-number") (property . "revision")))))

(defun buildbot-get-recent-changes (limit)
  "Get LIMIT number of recent changes."
  (buildbot-api-change (list (cons 'order "-changeid") (cons 'limit limit))))

(defun buildbot-get-all-builders ()
  "Get all builders."
  (alist-get 'builders (buildbot-api-builders)))

(defun buildbot-builder-by-id (builderid)
  "Get a builder by its BUILDERID."
  (cl-find-if
   (lambda (builder)
     (= (alist-get 'builderid builder) builderid))
   buildbot-builders))

(defun buildbot-builder-by-name (name)
  "Get a builder by its NAME."
  (cl-find-if
   (lambda (builder)
     (equal (alist-get 'name builder) name))
   buildbot-builders))

(defun buildbot-get-logs-by-stepid (stepid)
  "Get logs of a step with STEPID."
  (alist-get 'logs (buildbot-api-logs stepid)))

(defun buildbot-get-builder-name-by-id (id)
  "Get a builder name with ID."
  (alist-get 'name (buildbot-builder-by-id id)))

(defun buildbot-get-changes-by-revision (revision)
  "Get the changes from a REVISION."
  (alist-get 'changes
             (buildbot-api-change (list (cons 'revision revision)))))

(defun buildbot-get-build-by-buildid (buildid)
  "Get a build with BUILDID."
  (buildbot-api-build (list (cons 'buildid buildid))))

(defun buildbot-get-steps-by-buildid (buildid)
  "Get the steps of a build with BUILDID."
  (alist-get 'steps (buildbot-api-step buildid)))

(defun buildbot-get-changes-by-branch (branch-name limit)
  "Get LIMIT number of changes of a branch with BRANCH-NAME."
  (alist-get 'changes
             (buildbot-api-change
              (cons `(branch . ,branch-name)
                    (when limit `((limit . ,limit)))))))

(provide 'buildbot-client)
;;; buildbot-client.el ends here
