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
     `((tools . [((name . "get_docstring")
                  (description . "Get the docstring for an Emacs Lisp function")
                  (inputSchema . ((type . "object")
                                  (properties . ((function_name . ((type . "string")
                                                                   (description . "Name of the Emacs Lisp function")))))
                                  (required . ["function_name"]))))])))))

(defun emacs-mcp--handle-call-tool (request)
  "Handle tools/call request with REQUEST data."
  (let* ((id (alist-get 'id request))
         (params (alist-get 'params request))
         (tool-name (alist-get 'name params))
         (arguments (alist-get 'arguments params)))
    (cond
     ((string= tool-name "get_docstring")
      (let* ((function-name (alist-get 'function_name arguments))
             (symbol (intern-soft function-name))
             (docstring (and symbol (documentation symbol))))
        (if docstring
            (emacs-mcp--send-response
             id
             `((content . [((type . "text")
                            (text . ,docstring))])
               (isError . :json-false)))
          (emacs-mcp--send-response
           id
           `((content . [((type . "text")
                          (text . ,(format "No docstring found for function: %s" function-name)))])
             (isError . t))))))
     (t
      (emacs-mcp--send-error
       id -32602 (format "Unknown tool: %s" tool-name))))))

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

(provide 'emacs-mcp)
;;; emacs-mcp.el ends here
