# Emacs MCP - Model Context Protocol for Emacs

This package implements the Model Context Protocol (MCP) for Emacs, allowing Emacs to serve as an MCP server that can provide context to LLMs (Large Language Models).

## Features

- Retrieve docstrings for Emacs Lisp functions
- Provide function documentation to LLMs via the MCP protocol

## Installation

Clone this repository and add it to your Emacs load path:

```elisp
(add-to-list 'load-path "/path/to/emacs-mcp")
(require 'emacs-mcp)
```

## Usage

Once loaded, the MCP server will start automatically and listen for requests.

To manually start the server:

```elisp
(emacs-mcp-start-server)
```

To stop the server:

```elisp
(emacs-mcp-stop-server)
```

## Configuration

You can customize the server port and other settings:

```elisp
(setq emacs-mcp-port 8080)
```

## License

MIT
