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

async function requestModelSummary() {
  if (!client) {
    vscode.window.showErrorMessage("MDL language server is not running.");
    return undefined;
  }
  const editor = vscode.window.activeTextEditor;
  if (!editor || editor.document.languageId !== "mdl") {
    vscode.window.showErrorMessage("Open an MDL document first.");
    return undefined;
  }
  return client.sendRequest("mdl/modelSummary", {
    textDocument: { uri: editor.document.uri.toString() },
  });
}

async function showJsonDocument(title, payload) {
  if (payload === undefined) {
    return;
  }
  const document = await vscode.workspace.openTextDocument({
    language: "json",
    content: JSON.stringify(payload, null, 2),
  });
  await vscode.window.showTextDocument(document, { preview: false });
}

async function showModelSummary() {
  const summary = await requestModelSummary();
  await showJsonDocument("MDL Model Summary", summary);
}

async function showCoreTraceability() {
  const summary = await requestModelSummary();
  await showJsonDocument("MDL Core Traceability", summary ? summary.core : undefined);
}

async function showAlignments() {
  const summary = await requestModelSummary();
  await showJsonDocument("MDL Alignments", summary && summary.core ? summary.core.alignments : undefined);
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
    vscode.commands.registerCommand("mdl.showModelSummary", showModelSummary),
    vscode.commands.registerCommand("mdl.showCoreTraceability", showCoreTraceability),
    vscode.commands.registerCommand("mdl.showAlignments", showAlignments),
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
