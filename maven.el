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
(require 'transient)

(defgroup maven nil
  "Customization group for `maven'."
  :group 'java-mode)

(defcustom maven-mvn-executable (executable-find "mvn")
  "Path to maven executable."
  :group 'maven
  :type '(choice string))

(defvar maven-pom-file-name "pom.xml"
  "Name of pom file which indicates project root.")

(defun maven--find-root-pom-directory ()
  "Find the project root directory with pom.xml file for given FILE-NAME."
  (let* ((dir (file-name-directory (buffer-file-name)))
         (files (directory-files dir))
         (current-contains-pom-p (seq-contains-p files maven-pom-file-name))
         (root-dir (locate-dominating-file
                    (file-name-directory (directory-file-name dir))
                    maven-pom-file-name)))
    (if (and current-contains-pom-p (not root-dir))
        dir
      (if root-dir
          (maven--find-root-pom-directory root-dir)
        (error "Unable to find maven project")))))

(defun maven-open-effective-pom ()
  "Open effective pom in the read-only buffer."
  (interactive)
  (let ((default-directory (maven--find-root-pom-directory))
        (filename (make-temp-file "effective-pom" nil ".xml")))
    (call-process maven-mvn-executable
                  nil
                  nil
                  nil
                  "help:effective-pom"
                  (format "-Doutput=%s" filename))
    (find-file-read-only filename)))

(defun maven--execute-compilation-goal (goal &optional args)
  "Execute maven GOAL in the project root path using `compilation-mode'.

Optional ARGS are passed to mvn command."
  (let ((default-directory (maven--find-root-pom-directory))
        (command (if args
                     (format "%s %s %s" maven-mvn-executable (string-join args " ") goal)
                   (format "%s %s" maven-mvn-executable goal))))
    (compilation-start command)))

(defun maven-run-test-goal (&optional args)
  "Test project with `test' goal.

Optional ARGS are passed to mvn command."
  (interactive
   (list (transient-args 'maven-unit-test)))
  (maven--execute-compilation-goal "test" args))

(defun maven-run-verify-goal (&optional args)
  "Test project with `verify' goal.

Optional ARGS are passed to mvn command."
  (interactive
   (list (transient-args 'maven-integration-test)))
  (maven--execute-compilation-goal verify args))

(defconst maven--profile-regex ".*Profile\sId:\s\\(.+?\\)\s.*Active:\sfalse.*"
  "Regex to extract exact profile name from maven output.")

(defun maven--extract-profile (line)
  "Extract profile name from given LINE."
  (when (string-match maven--profile-regex line)
    (match-string 1 line)))

(defun maven--get-profiles-list ()
  "Get list of profiles for current project."
  (let ((default-directory (maven--find-root-pom-directory)))
    (with-temp-buffer
      (call-process maven-mvn-executable
                    nil
                    t
                    nil
                    "help:all-profiles")
      (mapcar #'maven--extract-profile
              (delete-dups
               (seq-filter (lambda (s)
                             (string-match-p maven--profile-regex s))
                           (split-string (buffer-string) "\n" t)))))))

(defun maven--read-xml-file-dom ()
  "Read FILE-PATH and return content as string."
  (let ((root-dir (maven--find-root-pom-directory)))
    (with-temp-buffer
      (insert-file-contents (format "%spom.xml" root-dir))
      (libxml-parse-xml-region (point-min) (point-max)))))

(defun maven--multi-module-p ()
  "Check if parent pom contain `modules' section."
  (let ((all-nodes (mapcar 'dom-tag (dom-non-text-children (maven--read-xml-file-dom)))))
    (and (seq-contains-p all-nodes 'modules))))

(defun maven--modules-list ()
  "Return list of modules from parent pom for maven project."
  (when (maven--multi-module-p)
    (let ((all-nodes (maven--read-xml-file-dom)))
      (mapcar #'dom-text
              (dom-by-tag (dom-by-tag all-nodes 'modules)
                          'module)))))

(defun maven--not-implemented ()
  "Dummy function for non-implemented transient commands."
  (interactive)
  (message "Not implemented"))

;; Transient commands
(defun maven--read-module (prompt &optional initial-input history)
  "Select module using `completing-read' function with given PROMPT.

Optional parameters INITIAL-INPUT and HISTORY will be passed to
`completing-read' function as is."
  (let* ((modules (maven--modules-list))
         (choice (completing-read prompt modules nil nil initial-input history)))
    (set-text-properties 0 (length choice) nil choice)
    choice))

(defun maven--read-profile (prompt &optional initial-input history)
  "Select profile using `completing-read' function with given PROMPT.

Optional parameters INITIAL-INPUT and HISTORY will be passed to
`completing-read' function as is."
  (let* ((profiles (maven--get-profiles-list))
         (choice (completing-read prompt profiles nil nil initial-input history)))
    (set-text-properties 0 (length choice) nil choice)
    choice))

(transient-define-infix maven-module:-pl ()
  :description "Execute for module"
  :class 'transient-option
  :if 'maven--multi-module-p
  :shortarg "-m"
  :argument "-pl "
  :reader 'maven--read-module)

(transient-define-infix maven-profile:-P ()
  :description "Activate profile"
  :class 'transient-option
  :shortarg "-P"
  :argument "-P"
  :reader 'maven--read-profile)

(transient-define-prefix maven-unit-test ()
  "Execute maven goals related to unit testing plugins."
  ["Command line parameters"
   (maven-module:-pl)
   (maven-profile:-P)]
  ["Run tests"
   [("t" "Execute tests for project" maven-run-test-goal)
    ("c" "Execute tests for current class" maven--not-implemented)
    ("m" "Execute tests for current method" maven--not-implemented)]])

(transient-define-prefix maven-integration-test ()
  "Execute maven goals related to integration testing plugins."
  ["Command line parameters"
   (maven-module:-pl)
   (maven-profile:-P)]
  ["Run tests"
   [("t" "Execute tests for project" maven-run-verify-goal)
    ("c" "Execute tests for current class" maven--not-implemented)
    ("m" "Execute tests for current method" maven--not-implemented)]])

(transient-define-prefix maven ()
  "Execute some maven commands."
  ["Command line parameters"
   ("-p" "Specify maven module to run" "-pl")]
  ["Lifecycle"
   [("t" "Unit tests" maven-unit-test)
    ("v" "Integration tests" maven-integration-test)]]
  ["General actions"
   [("e" "Show effective pom" maven-open-effective-pom)
    ("c" "Execute clean goal" maven--not-implemented)]])

(provide 'maven)
;;; maven.el ends here
