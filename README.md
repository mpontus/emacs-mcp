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

(run! "emacs-mcp")
```

This will ensure that the emacs instance running MCP server will access the same environment and modules as your interactive Doom Emacs instance.

You can then test your configuration using the following command:

```shell
npx @modelcontextprotocol/inspector ~/.config/emacs/bin/doomscript ~/.config/doom/bin/emacs-mcp
```

## Usage

Configure MCP server for your LLM client, by changing the `mcp.json` file as prescribed by your LLM client's documentation, such as [Claude Code](https://modelcontextprotocol.io/quickstart/user#2-add-the-filesystem-mcp-server) or [Amazon Q Developer CLI](https://docs.aws.amazon.com/amazonq/latest/qdeveloper-ug/command-line-mcp-configuration.html).

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

Or if you are using Doom Emacs, you can use the following configuration:

```json
{
  "mcpServers": {
    "emacs": {
      "command": "~/.config/emacs/bin/doomscript",
      "args": ["~/.config/doom/bin/emacs-mcp"]
    }
  }
}
```

## License

MIT
