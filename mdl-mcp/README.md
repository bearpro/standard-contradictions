# MDL MCP Server

`mdl-mcp` is a Model Context Protocol server for LLM-assisted MDL verification
workflows. It is a separate adapter package on top of the core `mprokazin-mdl`
toolkit, so installing the core language package does not pull MCP or HTTP
server dependencies.

It accepts generated MDL source or a restricted Python DSL that uses
`mdl.builder`, converts Python DSL output to canonical MDL, then runs the core
parser, linter, formatter and optional bounded solver.

## Run

```bash
mdl-mcp --transport stdio
mdl-mcp --transport streamable-http --host 127.0.0.1 --port 8000
```

## MCP tools

- `mdl_verify`: verify MDL source with optional virtual import documents;
- `mdl_verify_python_dsl`: execute restricted `mdl.builder` code with a timeout
  and verify the generated MDL;
- `mdl_format`: return canonical MDL source.

## MCP resources

- `mdl://docs/language-reference`: MDL language reference;
- `mdl://docs/quickstart`: placeholder for the MDL quickstart guide;
- `mdl://grammar/antlr`: raw ANTLR grammar for MDL.

The Python DSL verifier is intentionally limited. It allows imports from
`mdl.builder` and `mdl.ast`, executes in a subprocess with a timeout, and should
be treated as verifier isolation rather than a full security sandbox.
