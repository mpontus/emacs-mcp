# Emacs MCP - Model Context Protocol for Emacs

This package implements the Model Context Protocol (MCP) for Emacs, extending the capabilities of LLMs  to access information about Emacs environment and configration. Emacs MCP provides tools with information about loaded packages, available functions, keybindings, documentation, and more. This enables LLMs to be used to extend Emacs configuration and author new packages without requiring user to manually specify contextually relevant files.

## Installation

Clone this repository and include the following code in your user init file (e.g., ~/.emacs.d/init.el or equivalent):

```elisp
(add-to-list 'load-path "/path/to/emacs-mcp")
(require 'emacs-mcp)
```

Test your configuration using model context protocol inspector:

```shell
npx @modelcontextprotocol/inspector emacs --batch --load '<path to your init file>' --eval '(emacs-mcp-run-stdio)'
```

### Doom Emacs Configuration

Doom Emacs does not have an init file. To load user configuration before running in batch mode, you need to create a doomscript file which bootstraps doom emacs environment and loads your configuration files.

You can create a file called `~/.config/doom/bin/emacs-mcp` with the following content:

```elisp
#!/usr/bin/env doomscript
(defcli! emacs-mcp ()
  (doom-modules-initialize)      ; initialize the module system
  (doom-initialize t)            ; bootstrap Doom as if this were an interactive session
  (doom-startup)                 ; load your modules and user config
  (emacs-mcp-run-stdio))         ; run the MCP server
```

This will ensure that the emacs instance running MCP server will access the same environment and modules as your interactive Doom Emacs instance.

You can then test your configuration using the following command:

```shell
npx @modelcontextprotocol/inspector ~/.config/emacs/bin/doomscript ~/.config/doom/bin/emacs-mcp
```

## Usage

Following sections will describe specific configuration for different LLMs that support Model Context Protocol. The list is not exhaustive - if you want to enable support for a new LLM you can consult the official documentation and adapt the examples below. 

### Amazon Q Developer CLI

To configure MCP server in Amazon Q Developer CLI, you need to change the `~/.amazonq/mcp.json` file to include the following contents:

```json

{
  "mcpServers": {
    "emacs": {
      "command": "emacs",
      "args": [
        "--batch",
        "--load",
        "<path to your init file>",
        "--eval",
        "(emacs-mcp-run-stdio)"
      ]
    }
  }
}
```

Or if you are using Doom Emacs, you can use the following command:

```json
{
  "mcpServers": {
    "emacs": {
      "command": "bash",
      "args": [
        "-c",
        "~/.config/emacs/bin/doomscript",
        "~/.config/doom/bin/emacs-mcp"
      ]
    }
  }
}
```

### Claude Code

<I have no idea how to configure MCP servers in claude code, I'm lost>

## License

MIT
