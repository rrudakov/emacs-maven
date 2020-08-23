;;; maven.el --- Emacs package with various helpers for maven projects  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Roman Rudakov

;; Author: Roman Rudakov <rrudakov@pm.me>
;; Keywords: languages, tools

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

(require 'seq)

(defgroup maven nil
  "Customization group for `maven'."
  :group 'java-mode)

(defcustom maven-mvn-executable (executable-find "mvn")
  "Path to maven executable."
  :group 'maven
  :type '(choice string))

(defvar maven-pom-file-name "pom.xml"
  "Name of pom file which indicates project root.")

(defun maven--find-pom-directory (file-name)
  "Find the project root directory with pom.xml file for given FILE-NAME."
  (let* ((dir (file-name-directory file-name))
         (files (directory-files dir))
         (current-contains-pom-p (seq-contains-p files maven-pom-file-name))
         (root-dir (locate-dominating-file
                    (file-name-directory (directory-file-name dir))
                    maven-pom-file-name)))
    (if (and current-contains-pom-p (not root-dir))
        dir
      (if root-dir
          (maven--find-pom-directory root-dir)
        (error "Unable to find maven project")))))

(defun maven-open-effective-pom ()
  "Open effective pom in the read-only buffer."
  (interactive)
  (let ((default-directory (maven--find-pom-directory (buffer-file-name)))
        (filename (make-temp-file "effective-pom" nil ".xml")))
    (call-process maven-mvn-executable
                  nil
                  nil
                  nil
                  "help:effective-pom"
                  (format "-Doutput=%s" filename))
    (find-file-read-only filename)))

(provide 'maven)
;;; maven.el ends here
