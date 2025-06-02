;;; emacs-mcp.el --- MCP server for Emacs docstrings -*- lexical-binding: t -*-

;; Author: Your Name
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (json "1.5"))
;; Keywords: tools, convenience
;; URL: https://github.com/yourusername/emacs-mcp

;;; Commentary:
;; This package implements an MCP (Model Context Protocol) server for Emacs,
;; allowing LLMs to access Emacs Lisp symbol docstrings.

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'subr-x)

(defgroup emacs-mcp nil
  "MCP server for Emacs docstrings."
  :group 'tools)

(defcustom emacs-mcp-debug nil
  "Whether to enable debug logging for MCP server."
  :type 'boolean
  :group 'emacs-mcp)

(defvar emacs-mcp--buffer "*emacs-mcp*"
  "Buffer for MCP server communication.")

(defvar emacs-mcp--request-id 0
  "Counter for MCP request IDs.")

(defun emacs-mcp--log (format-string &rest args)
  "Log a message if debug is enabled.
FORMAT-STRING and ARGS are passed to `message'."
  (when emacs-mcp-debug
    (apply #'message (concat "MCP: " format-string) args)))

(defun emacs-mcp--next-request-id ()
  "Get the next request ID."
  (cl-incf emacs-mcp--request-id))

(defun emacs-mcp--send-json (obj)
  "Send JSON object OBJ to the MCP client."
  (let ((json-str (concat (json-encode obj) "\n")))
    (emacs-mcp--log "Sending: %s" json-str)
    (princ json-str)))

(defun emacs-mcp--handle-initialize (id params)
  "Handle initialize request with ID and PARAMS."
  (emacs-mcp--log "Handling initialize request: %s" params)
  (emacs-mcp--send-json
   `((jsonrpc . "2.0")
     (id . ,id)
     (result . ((protocolVersion . "2025-03-26")
                (capabilities . ((resources . ((subscribe . t)
                                              (listChanged . t)))))
                (serverInfo . ((name . "emacs-mcp")
                              (version . "0.1.0"))))))))

(defun emacs-mcp--handle-resources-list (id _params)
  "Handle resources/list request with ID and _PARAMS."
  (emacs-mcp--log "Handling resources/list request")
  (emacs-mcp--send-json
   `((jsonrpc . "2.0")
     (id . ,id)
     (result . ((resources . [((uri . "elisp:///docstring")
                              (name . "Elisp Docstrings")
                              (description . "Access to Emacs Lisp symbol docstrings")
                              (mimeType . "text/plain"))])
                (nextCursor . nil))))))

(defun emacs-mcp--handle-resources-templates-list (id _params)
  "Handle resources/templates/list request with ID and _PARAMS."
  (emacs-mcp--log "Handling resources/templates/list request")
  (emacs-mcp--send-json

   `((jsonrpc . "2.0")
     (id . ,id)
     (result . ((resourceTemplates . [((uriTemplate . "elisp:///docstring/{symbol}")
                                      (name . "Symbol Docstring")
                                      (description . "Get docstring for an Emacs Lisp symbol")
                                      (mimeType . "text/plain"))]))))))

(defun emacs-mcp--get-docstring (symbol-name)
  "Get docstring for SYMBOL-NAME."
  (condition-case err
      (let* ((symbol (intern-soft symbol-name))
             (docstring nil))
        (cond
         ((null symbol)
          (format "Symbol '%s' not found" symbol-name))
         ((fboundp symbol)
          (setq docstring (documentation symbol t))
          (if docstring
              (format "Function: %s\n\n%s" symbol-name docstring)
            (format "Function '%s' has no documentation" symbol-name)))
         ((boundp symbol)
          (setq docstring (documentation-property symbol 'variable-documentation t))
          (if docstring
              (format "Variable: %s\n\n%s" symbol-name docstring)
            (format "Variable '%s' has no documentation" symbol-name)))
         (t
          (format "Symbol '%s' exists but has no documentation" symbol-name))))
    (error (format "Error retrieving docstring for '%s': %s"
                   symbol-name (error-message-string err)))))

(defun emacs-mcp--handle-resources-read (id params)
  "Handle resources/read request with ID and PARAMS."
  (emacs-mcp--log "Handling resources/read request: %s" params)
  (let* ((uri (alist-get 'uri params))
         (content nil))

    ;; Handle docstring requests
    (when (string-match "elisp:///docstring/\\(.+\\)" uri)
      (let ((symbol-name (match-string 1 uri)))
        (setq content (emacs-mcp--get-docstring symbol-name))))

    ;; Handle root resources
    (when (string= uri "elisp:///docstring")
      (setq content "Use elisp:///docstring/{symbol} to get docstring for a specific symbol"))

    ;; If no content was set, provide a helpful message
    (unless content
      (setq content "Unknown resource URI. Available resources:\n- elisp:///docstring/{symbol}"))

    (emacs-mcp--send-json
     `((jsonrpc . "2.0")
       (id . ,id)
       (result . ((contents . [((uri . ,uri)
                               (mimeType . "text/plain")
                               (text . ,content))]))))))
  nil)

(defun emacs-mcp--handle-message (message)
  "Handle incoming MCP MESSAGE."
  (emacs-mcp--log "Received: %s" message)
  (let* ((json-object-type 'alist)
         (json-array-type 'vector)
         (json-key-type 'symbol)
         (json-false nil)
         (json-null nil)
         (msg (json-read-from-string message))
         (method (alist-get 'method msg))
         (id (alist-get 'id msg))
         (params (alist-get 'params msg)))

    (cond
     ((string= method "initialize")
      (emacs-mcp--handle-initialize id params))

     ((string= method "resources/list")
      (emacs-mcp--handle-resources-list id params))

     ((string= method "resources/templates/list")
      (emacs-mcp--handle-resources-templates-list id params))

     ((string= method "resources/read")
      (emacs-mcp--handle-resources-read id params))

     ((string= method "notifications/initialized")
      (emacs-mcp--log "Received initialized notification"))

     ((string= method "ping")
      (emacs-mcp--send-json
       `((jsonrpc . "2.0")
         (id . ,id)
         (result . ,(make-hash-table)))))

     (t
      (emacs-mcp--log "Unknown method: %s" method)
      (when id
        (emacs-mcp--send-json
         `((jsonrpc . "2.0")
           (id . ,id)
           (error . ((code . -32601)
                     (message . ,(format "Method not found: %s" method)))))))))))

(defun emacs-mcp--process-input ()
  "Process a line of input from stdin."
  (condition-case err
      (let ((line (read-from-minibuffer "")))
        (when (not (string-empty-p line))
          (emacs-mcp--handle-message line)
          (emacs-mcp--process-input)))
    (error
     (emacs-mcp--log "Error processing input: %s" (error-message-string err)))))

;;;###autoload
(defun emacs-mcp-start-server ()
  "Start the MCP server using stdin/stdout directly."
  (interactive)
  (message "MCP server started. Use Ctrl-C to stop.")
  (emacs-mcp--process-input))

;;;###autoload
(defun emacs-mcp-start-server-batch ()
  "Start the MCP server in batch mode.
This function is meant to be called with emacs --batch."
  (setq debug-on-error t)
  (let ((standard-input (get-buffer-create "*stdin*")))
    (with-current-buffer standard-input
      (while t
        (let ((line (read-string "")))
          (when (not (string-empty-p line))
            (emacs-mcp--handle-message line)))))))

(provide 'emacs-mcp)
;;; emacs-mcp.el ends here
