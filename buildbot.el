;;; buildbot.el --- A Buildbot client for emacs -*- lexical-binding: t; -*-

;; Author: Yuchen Pei <id@ypei.org>
;; Maintainer: Yuchen Pei <id@ypei.org>
;; Created: 2023
;; Version: 0.0.1
;; Keywords: buildbot, continuous integration
;; Package-Requires: ((emacs "28"))
;; Package-Type: multi
;; Homepage: https://g.ypei.me/buildbot.el.git

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

;; `buildbot.el' is an Emacs interface for Buildbot, a widely used
;; continuous integration tool. It can be used to view build
;; information on a Buildbot instance.

;; Main entry points:
;;
;; `buildbot-revision-open' prompts for a revision id (e.g. commit
;; hash in git), and opens a view of the revision, including builds
;; associated with changes associated with the revision.
;;
;; `buildbot-branch-open' prompts for a branch name, and opens up a
;; view of revisions of this branch.
;;
;; `buildbot-builder-open' prompts for a builder name from a list of
;; available builders, and opens up a view of recent builds by this
;; builder.

;;; Code:

(require 'buildbot-client)
(require 'buildbot-view)

(provide 'buildbot)
;;; buildbot.el ends here
