FROM python:3.12-slim AS builder
COPY --from=ghcr.io/astral-sh/uv:latest /uv /uvx /bin/
ENV UV_LINK_MODE=copy \
    UV_COMPILE_BYTECODE=1
WORKDIR /workspace/mdl-mcp
COPY mdl/ /workspace/mdl/
COPY mdl-mcp/pyproject.toml mdl-mcp/uv.lock ./
RUN --mount=type=cache,target=/root/.cache/uv \
    uv sync --frozen --no-dev --no-install-project
COPY mdl-mcp/ ./
RUN --mount=type=cache,target=/root/.cache/uv \
    uv sync --frozen --no-dev

FROM python:3.12-slim
WORKDIR /app
COPY --from=builder /workspace/mdl-mcp /app
ENV PATH="/app/.venv/bin:$PATH" \
    PYTHONUNBUFFERED=1
ENTRYPOINT ["mdl-mcp"]
