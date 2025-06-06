;;; emacs-mcp.el --- MCP server for Emacs via stdio -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Michael Pontus <m.pontus@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, mcp, ai
;; URL: https://github.com/mpontus/emacs-mcp

;;; Commentary:

;; This package implements the Model Context Protocol (MCP) for Emacs, providng
;; read-only access to information about emacs environment and configuration,
;; aimed at providing capabilities of LLM to extend emacs configuration and
;; provision new modules.
;;
;; It works by serving MCP server through stdio, taking advantage of emacs
;; running in batch mode being able to read from stdin and write to stdout.

;;; Code:

(require 'json)
(require 'cl-lib)

(defgroup emacs-mcp nil
  "Model Context Protocol server for Emacs."
  :group 'tools
  :prefix "emacs-mcp-")

(defvar emacs-mcp--message-id 0
  "Counter for message IDs.")

(defvar emacs-mcp--tools nil
  "List of registered MCP tools.")

(defun emacs-mcp--next-id ()
  "Generate the next message ID."
  (setq emacs-mcp--message-id (1+ emacs-mcp--message-id))
  emacs-mcp--message-id)

(defun emacs-mcp--send-response (id result)
  "Send a JSON-RPC response with ID and RESULT."
  (let ((response (json-encode `((jsonrpc . "2.0")
                                 (id . ,id)
                                 (result . ,result)))))
    (princ (concat response "\n"))))

(defun emacs-mcp--send-error (id code message)
  "Send a JSON-RPC error with ID, error CODE and MESSAGE."
  (let ((response (json-encode `((jsonrpc . "2.0")
                                 (id . ,id)
                                 (error . ((code . ,code)
                                           (message . ,message)))))))
    (princ (concat response "\n"))))

(defun emacs-mcp--send-notification (method &optional params)
  "Send a JSON-RPC notification with METHOD and optional PARAMS."
  (let ((notification (json-encode `((jsonrpc . "2.0")
                                     (method . ,method)
                                     ,@(when params `((params . ,params)))))))
    (princ (concat notification "\n"))))

(defun emacs-mcp--handle-initialize (request)
  "Handle initialize request with REQUEST data."
  (let ((id (alist-get 'id request))
        (params (alist-get 'params request)))
    (emacs-mcp--send-response
     id
     `((protocolVersion . "2024-11-05")
       (capabilities . ((tools . ((listChanged . t)))))
       (serverInfo . ((name . "emacs-mcp")
                      (version . "0.1.0")))))))

(defun emacs-mcp--handle-list-tools (request)
  "Handle tools/list request with REQUEST data."
  (let ((id (alist-get 'id request)))
    (emacs-mcp--send-response
     id
     `((tools . ,(apply #'vector emacs-mcp--tools))))))

(defun emacs-mcp--handle-call-tool (request)
  "Handle tools/call request with REQUEST data."
  (let* ((id (alist-get 'id request))
         (params (alist-get 'params request))
         (tool-name (alist-get 'name params))
         (arguments (alist-get 'arguments params)))
    (dolist (tool emacs-mcp--tools)
      (when (string= (alist-get 'name tool) tool-name)
        (let ((result (apply (intern (concat "emacs-mcp--" tool-name "-tool"))
                             (mapcar (lambda (arg) (alist-get arg arguments))
                                     (mapcar 'car
                                             (alist-get 'properties
                                                        (alist-get 'inputSchema tool)))))))
          (condition-case err
              (emacs-mcp--send-response
               id
               `((content . [((type . "text")
                              (text . ,result))])
                 (isError . :json-false)))
            (error
             (emacs-mcp--send-response
              id
              `((content . [((type . "text")
                             (text . ,(error-message-string err)))])
                (isError . t))))))))))

(defun emacs-mcp--handle-request (request)
  "Handle REQUEST."
  (let ((method (alist-get 'method request)))
    (cond
     ((string= method "initialize")
      (emacs-mcp--handle-initialize request))
     ((string= method "tools/list")
      (emacs-mcp--handle-list-tools request))
     ((string= method "tools/call")
      (emacs-mcp--handle-call-tool request))
     (t
      (let ((id (alist-get 'id request)))
        (when id
          (emacs-mcp--send-error
           id -32601 (format "Method not found: %s" method))))))))

(defun emacs-mcp--handle-notification (notification)
  "Handle NOTIFICATION."
  (let ((method (alist-get 'method notification)))
    (cond
     ((string= method "notifications/initialized")
      ;; Client is initialized, nothing to do
      nil)
     (t
      ;; Unknown notification, ignore
      nil))))

(defun emacs-mcp--process-input (input)
  "Process MCP INPUT string."
  (condition-case err
      (let ((json-object-type 'alist)
            (json-array-type 'vector)
            (json-key-type 'symbol))
        (let ((message (json-read-from-string input)))
          (if (alist-get 'id message)
              (emacs-mcp--handle-request message)
            (emacs-mcp--handle-notification message))))
    (error
     (message "Error processing MCP input: %s" (error-message-string err)))))

;;;###autoload
(defun emacs-mcp-run-stdio ()
  "Run the MCP server using stdio.
This function is meant to be used in batch mode."
  (interactive)
  (while t
    (let ((input (read-from-minibuffer "")))
      (when (string= input "")
        (error "Empty input, exiting"))
      (emacs-mcp--process-input input))))

;;;###autoload
(defmacro define-mcp-tool (name args description &rest body)
  "Define an MCP tool with NAME, ARGS, DESCRIPTION and BODY.
NAME is a symbol that will be used directly as the tool name.
ARGS is a list of parameter names that will be extracted from the request.
DESCRIPTION is a string describing the tool's purpose.
BODY is the implementation of the tool."
  (declare (indent 2))
  (let ((symbol (intern (format "emacs-mcp--%s-tool" name)))
        (properties (mapcar (lambda (arg) `(,arg . ((type . "string")))) args))
        (required (mapcar 'symbol-name args)))
    `(progn
       (defun ,symbol ,args ,description ,@body)
       (add-to-list 'emacs-mcp--tools
                    '((name . ,(symbol-name name))
                      (description . ,description)
                      (inputSchema . ((type . "object")
                                      ,@(when properties
                                          `((properties . ,properties)))
                                      ,@(when required
                                          `((required . ,(vconcat required)))))))))))

;; Define tools
(define-mcp-tool get-docstring (function-name)
  "Get the docstring for an Emacs Lisp function.
Provides the raw documentation string for any Emacs Lisp function.
FUNCTION-NAME should be the name of the function as a string.
Returns the full docstring or an error if the function doesn't exist."
  (let* ((symbol (intern-soft function-name))
         (docstring (and symbol (documentation symbol))))
    (or docstring
        (error "No docstring found for function: %s" function-name))))

(define-mcp-tool describe-variable (variable-name)
  "Describe an Emacs Lisp variable in detail.
Provides comprehensive information about a variable including:
- Current value
- Documentation string
- Whether it is customizable
- Where it was defined
VARIABLE-NAME should be the name of the variable as a string."
  (save-window-excursion
    (describe-variable (intern variable-name))
    (with-current-buffer "*Help*"
      (let ((docstring (buffer-string)))
        (kill-buffer)
        docstring))))

(define-mcp-tool describe-key (key-sequence)
  "Display documentation of the function invoked by KEY-SEQUENCE.
Provides information about what command a key sequence runs and its documentation.
KEY-SEQUENCE should be in Emacs key notation as a string (e.g. \"C-x C-f\").
Returns details about the key binding and the function it calls."
  (save-window-excursion
    (let ((key (kbd key-sequence)))
      (describe-key key)
      (with-current-buffer "*Help*"
        (let ((result (buffer-string)))
          (kill-buffer)
          result)))))

(define-mcp-tool describe-mode ()
  "Display documentation of current major mode and minor modes.
Provides comprehensive information about:
- The current major mode and its purpose
- All enabled minor modes
- Key bindings specific to these modes
This helps understand the current editing environment and available commands."
  (save-window-excursion
    (describe-mode)
    (with-current-buffer "*Help*"
      (let ((result (buffer-string)))
        (kill-buffer)
        result))))

(define-mcp-tool describe-function (function-name)
  "Display the full documentation of FUNCTION.
Provides detailed information about an Emacs Lisp function including:
- Its argument list
- Full documentation string
- Where it was defined
- Key bindings that call this function
FUNCTION-NAME should be the name of the function as a string."
  (save-window-excursion
    (describe-function (intern function-name))
    (with-current-buffer "*Help*"
      (let ((result (buffer-string)))
        (kill-buffer)
        result))))

(define-mcp-tool describe-face (face-name)
  "Display the properties of face FACE.
Provides detailed information about a display face including:
- Its appearance attributes (color, weight, slant, etc.)
- Where it was defined
- How it's currently displayed
FACE-NAME should be the name of the face as a string.
This is useful for understanding text styling in Emacs."
  (save-window-excursion
    (describe-face (intern face-name))
    (with-current-buffer "*Help*"
      (let ((result (buffer-string)))
        (kill-buffer)
        result))))

(define-mcp-tool describe-package (package-name)
  "Display the full documentation of PACKAGE.
Provides comprehensive information about an installed package including:
- Version information
- Summary and description
- Dependencies
- Features provided
PACKAGE-NAME should be the name of the package as a string.
This helps understand what functionality a package provides."
  (save-window-excursion
    (require 'package)
    (describe-package (intern package-name))
    (with-current-buffer "*Help*"
      (let ((result (buffer-string)))
        (kill-buffer)
        result))))

(define-mcp-tool describe-bindings ()
  "Display a buffer showing a list of all defined keys, and their definitions.
Provides a comprehensive list of all currently active key bindings organized by prefix.
This gives a complete overview of available commands and their key shortcuts.
Useful for understanding what commands are available in the current context."
  (save-window-excursion
    (describe-bindings)
    (with-current-buffer "*Help*"
      (let ((result (buffer-string)))
        (kill-buffer)
        result))))

(define-mcp-tool describe-theme (theme-name)
  "Display a description of the Custom theme THEME.
Provides information about a specific Emacs theme including:
- Its settings and customizations
- Faces it defines or modifies
- Where it was defined
THEME-NAME should be the name of the theme as a string.
This helps understand how a theme affects Emacs appearance."
  (save-window-excursion
    (require 'custom)
    (describe-theme (intern theme-name))
    (with-current-buffer "*Help*"
      (let ((result (buffer-string)))
        (kill-buffer)
        result))))

(define-mcp-tool describe-syntax ()
  "Describe the syntax specifications in the current syntax table.
Provides detailed information about how Emacs interprets different characters
in the current buffer's major mode. This includes:
- Which characters are considered word constituents
- Which characters are considered punctuation
- How comment and string delimiters are defined
This is useful for understanding how Emacs parses text in different modes."
  (save-window-excursion
    (describe-syntax)
    (with-current-buffer "*Help*"
      (let ((result (buffer-string)))
        (kill-buffer)
        result))))

(define-mcp-tool apropos-command (pattern)
  "Search for commands matching PATTERN.
Finds and returns information about all Emacs commands whose names match PATTERN.
PATTERN can be a regular expression or a simple string.
Results include command names, key bindings, and brief descriptions.
This is useful for discovering commands related to a specific topic or feature."
  (require 'apropos)
  (save-window-excursion
    (apropos-command pattern)
    (with-current-buffer "*Apropos*"
      (let ((result (buffer-string)))
        (kill-buffer)
        result))))

(define-mcp-tool apropos-variable (pattern)
  "Search for variables matching PATTERN.
Finds and returns information about all Emacs variables whose names match PATTERN.
PATTERN can be a regular expression or a simple string.
Results include variable names, current values, and brief descriptions.
This is useful for discovering configuration options related to a specific feature."
  (require 'apropos)
  (save-window-excursion
    (apropos-variable pattern)
    (with-current-buffer "*Apropos*"
      (let ((result (buffer-string)))
        (kill-buffer)
        result))))

(define-mcp-tool apropos-value (pattern)
  "Search for variables with values matching PATTERN.
Finds and returns information about Emacs variables whose values match PATTERN.
PATTERN can be a regular expression or a simple string.
Results include variable names, matching values, and brief descriptions.
This is useful for finding variables set to specific values or containing certain data."
  (require 'apropos)
  (save-window-excursion
    (apropos-value pattern)
    (with-current-buffer "*Apropos*"
      (let ((result (buffer-string)))
        (kill-buffer)
        result))))

(define-mcp-tool apropos-documentation (pattern)
  "Search for symbols with documentation matching PATTERN.
Finds and returns information about Emacs symbols whose documentation contains PATTERN.
PATTERN can be a regular expression or a simple string.
Results include symbol names and the matching portions of their documentation.
This is useful for finding features described with specific terms in their documentation."
  (require 'apropos)
  (save-window-excursion
    (apropos-documentation pattern)
    (with-current-buffer "*Apropos*"
      (let ((result (buffer-string)))
        (kill-buffer)
        result))))

(define-mcp-tool apropos (pattern)
  "Search for symbols whose names match PATTERN.
Finds and returns information about all Emacs symbols whose names match PATTERN.
PATTERN can be a regular expression or a simple string.
Results include functions, variables, faces, and other symbols.
This is the most general search tool and useful for broad exploration of Emacs features."
  (require 'apropos)
  (save-window-excursion
    (apropos pattern)
    (with-current-buffer "*Apropos*"
      (let ((result (buffer-string)))
        (kill-buffer)
        result))))

;; Info documentation tools
(define-mcp-tool info-node (node-name)
  "Display the contents of an Info node.
Provides the full text content of a specific Info documentation node.
NODE-NAME should be the name of the node as a string (e.g. \"(emacs)Basic\").
Returns the text content of the specified Info node."
  (require 'info)
  (save-window-excursion
    (info node-name)
    (with-current-buffer "*info*"
      (let ((result (buffer-string)))
        (kill-buffer)
        result))))

(define-mcp-tool info-search (topic)
  "Search for TOPIC in the Info documentation.
Performs a search across Info documentation for the specified topic.
TOPIC should be a string to search for.
Returns a list of matching nodes and context around the matches."
  (require 'info)
  (save-window-excursion
    (info)
    (Info-search topic)
    (let ((result (format "Search results for '%s':\n\n" topic))
          (node-name (format "%s" Info-current-node))
          (file-name (format "%s" Info-current-file)))
      (setq result (concat result 
                           (format "Found in node: %s in file: %s\n\n" 
                                   node-name file-name)))
      ;; Get context around the match
      (let ((start (max (point-min) (- (point) 200)))
            (end (min (point-max) (+ (point) 500))))
        (setq result (concat result 
                             (buffer-substring start end))))
      (kill-buffer)
      result)))

(define-mcp-tool info-index (index-item)
  "Look up INDEX-ITEM in the indices of the Info documentation.
Finds entries in Info documentation indices that match the specified item.
INDEX-ITEM should be a string to look up in the indices.
Returns information about matching index entries and their locations."
  (require 'info)
  (save-window-excursion
    (info)
    (Info-index index-item)
    (with-current-buffer "*info*"
      (let ((result (format "Index results for '%s':\n\n" index-item))
            (node-name (format "%s" Info-current-node))
            (file-name (format "%s" Info-current-file)))
        (setq result (concat result 
                             (format "Found in node: %s in file: %s\n\n" 
                                     node-name file-name)))
        ;; Get the content of the node
        (setq result (concat result (buffer-substring (point-min) (point-max))))
        (kill-buffer)
        result))))

(define-mcp-tool info-toc (manual)
  "Display the table of contents for a specific Info MANUAL.
Provides the structure and organization of an Info manual.
MANUAL should be the name of the manual as a string (e.g. \"emacs\").
Returns the table of contents of the specified manual."
  (require 'info)
  (save-window-excursion
    (info (concat "(" manual ")"))
    (Info-directory)
    (with-current-buffer "*info*"
      (let ((result (format "Table of Contents for '%s':\n\n" manual)))
        (setq result (concat result (buffer-substring (point-min) (point-max))))
        (kill-buffer)
        result))))

(define-mcp-tool info-list-manuals ()
  "List all available Info manuals.
Provides a comprehensive list of all Info documentation manuals available in the system.
This helps discover what documentation is available for reference."
  (require 'info)
  (save-window-excursion
    (info)
    (Info-directory)
    (with-current-buffer "*info*"
      (let ((result "Available Info Manuals:\n\n"))
        (setq result (concat result (buffer-substring (point-min) (point-max))))
        (kill-buffer)
        result))))

;; Configuration and environment tools
(define-mcp-tool get-emacs-version ()
  "Get detailed information about the current Emacs version.
Provides version number, build details, and system configuration information.
This helps understand the capabilities and limitations of the current Emacs instance."
  (concat "Emacs Version:\n\n" (emacs-version)))

(define-mcp-tool list-loaded-features ()
  "List all features (libraries) that have been loaded in Emacs.
Provides a comprehensive list of all Emacs Lisp libraries currently loaded.
This helps understand what functionality is available in the current session."
  (let ((result "Loaded Features:\n\n"))
    (dolist (feature features)
      (setq result (concat result (format "- %s\n" feature))))
    result))

;; Package management tools
(define-mcp-tool list-installed-packages ()
  "List all installed packages with their status and version.
Provides a comprehensive overview of the user's package ecosystem."
  (require 'package)
  (package-initialize)
  (let ((result "Installed Packages:\n\n"))
    (dolist (pkg package-alist)
      (let* ((name (car pkg))
             (desc (cadr pkg))
             (version (package-desc-version desc))
             (status (if (package-installed-p name) "Installed" "Not Installed")))
        (setq result (concat result (format "- %s (%s): %s\n" 
                                          name version status)))))
    result))

;; Buffer and file management tools
(define-mcp-tool list-available-modes ()
  "List all available major modes in this Emacs instance.
Provides a comprehensive list of all major modes that can be used.
This helps understand what file types and editing modes are supported."
  (let ((result "Available Major Modes:\n\n")
        (modes '()))
    (mapatoms (lambda (sym)
                (when (and (functionp sym)
                           (string-match "-mode$" (symbol-name sym))
                           (not (string-match "-minor-mode$" (symbol-name sym))))
                  (push (symbol-name sym) modes))))
    (setq modes (sort modes 'string<))
    (dolist (mode modes)
      (setq result (concat result (format "- %s\n" mode))))
    result))

;; Customization and settings tools
(define-mcp-tool list-custom-variables ()
  "List all customized variables in the current Emacs session.
Shows variables that have been customized away from their default values.
This helps understand how the user has personalized their Emacs."
  (require 'cus-edit)
  (let ((result "Customized Variables:\n\n"))
    (dolist (theme custom-enabled-themes)
      (setq result (concat result (format "Theme: %s\n" theme))))
    (setq result (concat result "\nVariables:\n"))
    (mapatoms
     (lambda (symbol)
       (when (and (boundp symbol)
                  (get symbol 'saved-value))
         (setq result (concat result (format "- %s: %S\n" 
                                           symbol (symbol-value symbol)))))))
    result))

;; Hook and advice inspection tools
(define-mcp-tool describe-hooks (hook-pattern)
  "Describe hooks matching HOOK-PATTERN.
Lists all hooks whose names match the pattern and shows their values.
This helps understand what customizations are triggered at various points."
  (let ((result (format "Hooks matching \"%s\":\n\n" hook-pattern))
        (hooks '()))
    (mapatoms
     (lambda (symbol)
       (when (and (boundp symbol)
                  (string-match "-hook$" (symbol-name symbol))
                  (string-match hook-pattern (symbol-name symbol)))
         (push symbol hooks))))
    (setq hooks (sort hooks (lambda (a b) (string< (symbol-name a) (symbol-name b)))))
    (dolist (hook hooks)
      (setq result (concat result (format "- %s:\n" hook)))
      (let ((value (symbol-value hook)))
        (if (not value)
            (setq result (concat result "  (empty)\n"))
          (dolist (func value)
            (setq result (concat result (format "  - %s\n" func)))))))
    result))

;; Font and display tools
(define-mcp-tool list-available-fonts ()
  "List all available fonts in the current Emacs session.
Shows what fonts can be used for display customization.
This helps understand display capabilities and options."
  (let ((result "Available Fonts:\n\n")
        (fonts (font-family-list)))
    (setq fonts (sort fonts 'string<))
    (dolist (font fonts)
      (setq result (concat result (format "- %s\n" font))))
    result))

;; Keybinding analysis tools
(define-mcp-tool find-key-conflicts (prefix)
  "Find conflicting key bindings starting with PREFIX.
Identifies key sequences that might shadow or conflict with each other.
PREFIX should be a key prefix in string form (e.g. \"C-c\").
This helps diagnose keybinding issues and conflicts."
  (require 'help-fns)
  (let ((result (format "Key bindings starting with %s:\n\n" prefix))
        (key (kbd prefix))
        (map (current-global-map))
        bindings)
    (map-keymap
     (lambda (k v)
       (when v
         (let ((key-desc (key-description (vector k))))
           (push (cons key-desc v) bindings))))
     (lookup-key map key))
    (setq bindings (sort bindings (lambda (a b) (string< (car a) (car b)))))
    (dolist (binding bindings)
      (setq result (concat result (format "- %s%s: %s\n" 
                                        prefix
                                        (if (string= (car binding) "") "" " ")
                                        (car binding)
                                        (cdr binding)))))
    result))

(provide 'emacs-mcp)
;;; emacs-mcp.el ends here
