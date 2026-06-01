FROM python:3.12-slim AS build
COPY --from=ghcr.io/astral-sh/uv:latest /uv /uvx /bin/
ENV UV_LINK_MODE=copy \
    UV_COMPILE_BYTECODE=1
WORKDIR /app/mdl-mcp
COPY mdl/ /app/mdl/
COPY mdl-mcp/pyproject.toml mdl-mcp/uv.lock ./
RUN --mount=type=cache,target=/root/.cache/uv \
    uv sync --frozen --no-dev --no-editable --no-install-project
COPY mdl-mcp/ ./
RUN --mount=type=cache,target=/root/.cache/uv \
    uv sync --frozen --no-dev --no-editable

FROM python:3.12-slim AS runtime
WORKDIR /app/mdl-mcp
COPY --from=build /app/mdl-mcp /app/mdl-mcp
ENV PATH="/app/mdl-mcp/.venv/bin:$PATH" \
    PYTHONUNBUFFERED=1
ENTRYPOINT ["mdl-mcp"]
