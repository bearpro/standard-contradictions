from __future__ import annotations

import json
import re
import sys
from dataclasses import dataclass
from typing import Any


RULE_BODY_CALL_RE = re.compile(
    r"(?P<prefix>\brule\s+\w+\s+\w+\s*=\s*)(?P<predicate>[A-Za-z_][A-Za-z0-9_]*)\s*\(",
)

NL18_MESSAGE = (
    "NL18 Predicates used in rule bodies must start with the word satisfies"
)


@dataclass
class Document:
    uri: str
    text: str


documents: dict[str, Document] = {}
shutdown_requested = False


def read_message() -> dict[str, Any] | None:
    headers: dict[str, str] = {}
    while True:
        line = sys.stdin.buffer.readline()
        if line == b"":
            return None
        if line in {b"\r\n", b"\n"}:
            break
        name, value = line.decode("ascii").split(":", 1)
        headers[name.lower()] = value.strip()

    length = int(headers["content-length"])
    body = sys.stdin.buffer.read(length)
    return json.loads(body)


def send(payload: dict[str, Any]) -> None:
    body = json.dumps(payload, separators=(",", ":")).encode("utf-8")
    sys.stdout.buffer.write(f"Content-Length: {len(body)}\r\n\r\n".encode("ascii"))
    sys.stdout.buffer.write(body)
    sys.stdout.buffer.flush()


def respond(message: dict[str, Any], result: Any = None) -> None:
    if "id" not in message:
        return
    send({"jsonrpc": "2.0", "id": message["id"], "result": result})


def publish_diagnostics(uri: str, text: str) -> None:
    diagnostics: list[dict[str, Any]] = []
    for line_no, line in enumerate(text.splitlines()):
        match = RULE_BODY_CALL_RE.search(line)
        if match is None:
            continue
        predicate = match.group("predicate")
        if predicate.startswith("satisfies"):
            continue
        start = match.start("predicate")
        end = match.end("predicate")
        diagnostics.append(
            {
                "range": {
                    "start": {"line": line_no, "character": start},
                    "end": {"line": line_no, "character": end},
                },
                "severity": 2,
                "code": "NL18",
                "source": "mdl-lsp",
                "message": NL18_MESSAGE,
            }
        )

    send(
        {
            "jsonrpc": "2.0",
            "method": "textDocument/publishDiagnostics",
            "params": {"uri": uri, "diagnostics": diagnostics},
        }
    )


def handle_notification(message: dict[str, Any]) -> None:
    method = message.get("method")
    params = message.get("params", {})

    if method == "textDocument/didOpen":
        item = params["textDocument"]
        document = Document(item["uri"], item["text"])
        documents[document.uri] = document
        publish_diagnostics(document.uri, document.text)
        return

    if method == "textDocument/didChange":
        uri = params["textDocument"]["uri"]
        text = params["contentChanges"][-1]["text"]
        document = Document(uri, text)
        documents[uri] = document
        publish_diagnostics(uri, text)


def handle_request(message: dict[str, Any]) -> bool:
    global shutdown_requested

    method = message.get("method")
    if method == "initialize":
        respond(
            message,
            {
                "capabilities": {
                    "textDocumentSync": {
                        "openClose": True,
                        "change": 1,
                    },
                },
                "serverInfo": {"name": "mdl-demo-lsp", "version": "0.1.0"},
            },
        )
        return True

    if method == "shutdown":
        shutdown_requested = True
        respond(message, None)
        return True

    if method == "exit":
        return False

    respond(message, None)
    return True


def main() -> None:
    while True:
        message = read_message()
        if message is None:
            break
        if "id" in message:
            if not handle_request(message):
                break
        else:
            handle_notification(message)
        if shutdown_requested and message.get("method") == "exit":
            break


if __name__ == "__main__":
    main()
