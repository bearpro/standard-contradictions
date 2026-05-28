"use strict";

const vscode = require("vscode");
const { LanguageClient } = require("vscode-languageclient/node");

let client;

function getConfig() {
  const config = vscode.workspace.getConfiguration("mdl");
  return {
    enabled: config.get("lsp.enabled", true),
    command: config.get("lsp.command", "mdl"),
    args: config.get("lsp.args", ["lsp"]),
    cwd: config.get("lsp.cwd", null),
  };
}

function createClient() {
  const config = getConfig();
  if (!config.enabled) {
    return undefined;
  }

  const serverOptions = {
    command: config.command,
    args: config.args,
    options: config.cwd ? { cwd: config.cwd } : undefined,
  };

  const clientOptions = {
    documentSelector: [
      { scheme: "file", language: "mdl" },
      { scheme: "untitled", language: "mdl" },
    ],
    synchronize: {
      fileEvents: vscode.workspace.createFileSystemWatcher("**/*.mdl"),
    },
  };

  return new LanguageClient("mdlLanguageServer", "MDL Language Server", serverOptions, clientOptions);
}

async function stopClient() {
  if (!client) {
    return;
  }
  const current = client;
  client = undefined;
  await current.stop();
}

async function restartClient() {
  await stopClient();
  client = createClient();
  if (client) {
    await client.start();
  }
}

function startClient() {
  return restartClient().catch((error) => {
    const message = error && error.message ? error.message : String(error);
    vscode.window.showErrorMessage(`Failed to start MDL language server: ${message}`);
  });
}

function activate(context) {
  context.subscriptions.push(
    vscode.commands.registerCommand("mdl.restartLanguageServer", startClient),
    vscode.workspace.onDidChangeConfiguration((event) => {
      if (event.affectsConfiguration("mdl.lsp")) {
        startClient();
      }
    })
  );

  startClient();
}

function deactivate() {
  return stopClient();
}

module.exports = {
  activate,
  deactivate,
};
