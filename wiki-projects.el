;;; wiki-projects.el --- manage wiki projects

;; Copyright (C) 2001  Alex Schroeder <alex@gnu.org>

;; Version: 1.0.0
;; Keywords: hypermedia
;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: Alex Schroeder <alex@gnu.org>
;; URL: http://www.geocities.com/kensanata/wiki/WikiMode.html
;; Compatibility: Emacs20, XEmacs21

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; This package is an optional extension to wiki.el.

;; This used to be wiki-priv.el -- a package to maintain a public and a
;; private wiki.  This has now changed.  The public and the private wiki
;; are still the two default projects, but it is now very easy to add
;; more wiki projects.  A wiki project is a set of directories
;; containing pages and a set of publishing rules.

;; The default setup includes a public and a private project.  The
;; public project uses the ~/Wiki directory and publishes HTML files
;; into the ~/WebWiki directory.  The private project uses the ~/Wiki
;; and the ~/Notes directories and it disables publishing.

;; Use `wiki-project' to switch projects.  Customize `wiki-projects' to
;; set them up.  If you do not customize this variable, the "Public"
;; project will take the default values from the variables
;; `wiki-pub-directory', `wiki-directories' and `wiki-pub-rules'.  Once
;; you customize `wiki-projects', switching to a project will overwrite
;; any values stored in those three variables.

;; In order to install, put (require 'wiki-projects) somewhere in your
;; ~/.emacs file.

;;; Code:

(require 'wiki)

(defcustom wiki-projects
  `(("Public"
     ,wiki-directories
     ,wiki-pub-directory
     ,wiki-pub-rules)
    ("Private"
     ,(list (expand-file-name "~/Notes/")
            (expand-file-name "~/Wiki/"))
     nil
     nil))
  "A list of wiki project definitions.

Each definition has the form
\(NAME WIKI-DIRECTORIES PUBLISHING-DIRECTORY RULES)

NAME is a string naming the project.
WIKI-DIRECTORIES is the value to use for `wiki-directories'.
PUBLISHING-DIRECTORY is the value to use for `wiki-pub-directory'.
RULES is the value to use for `wiki-pub-rules'."
  :group 'wiki
  :type '(repeat
          (list
           :tag "Project"
           (string :tag "Name")
           (repeat :tag "Wiki directories" directory)
           (choice
            :tag "Publishing"
            directory
            (const :tag "Disable publishing" nil))
           (repeat
            :tag "Publishing rules"
            (choice :value ("regexp" . "newtext")
                    (cons :tag "Rule"
                          (choice
                           (regexp :tag "Search a regexp")
                           (function :tag "Call a function to place point"
                                     :value end-of-buffer))
                                 (choice
                                  (string :tag "Insert or replace a string"
                                          :value "newtext")
                                  (function :tag "Insert or replace a function"
                                            :value current-time-string)))
                    (function :tag "Function"
                              :value current-time-string))))))

(defun wiki-project ()
  "Switch wiki project.
See `wiki-projects' for a list of projects."
  (interactive)
  (let* ((name (completing-read "Project: " wiki-projects))
         (project (assoc name wiki-projects))
         (directories (nth 1 project))
         (pub-directory (nth 2 project))
         (pub-rules (nth 3 project)))
    (setq wiki-directories directories
          wiki-pub-directory pub-directory
          wiki-pub-rules pub-rules
          ;; reset cache
          wiki-last-update nil)
    (wiki-project-update-buffers)))

(defun wiki-project-update-buffers ()
  "Call `wiki-maybe' for all buffers."
  (let ((bufs (buffer-list))
        buf)
    (while bufs
      (setq buf (car bufs)
            bufs (cdr bufs))
      (set-buffer buf)
      (when buffer-file-name
        (wiki-maybe)))))

(provide 'wiki-projects)

;; wiki-projects.el ends here
