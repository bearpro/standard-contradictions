# Mprokazin.DdlLtlf.OnlineDemo

A minimal ddl-ltlf playground that lets anonymous visitors manage multi-file projects, trigger stubbed **Convert**/**Solve** operations, and observe quota limits.

## Prerequisites

* .NET 8 SDK
* PostgreSQL 15+ (or compatible)
* Optional: `dotnet-ef` tool for database migrations

## Configuration

The application reads `ConnectionStrings:Default` from configuration. The bundled `appsettings.json` points to a local PostgreSQL instance:

```
Host=localhost;Port=5432;Database=ddl_onlinedemo;Username=ddl_user;Password=ddl_pass
```

Override this value via environment variable when running locally:

```bash
export ConnectionStrings__Default="Host=localhost;Port=5432;Database=ddl_onlinedemo;Username=postgres;Password=secret"
```

## Database setup

Create or update the schema using EF Core migrations:

```bash
cd ddl-ltlf/src/Mprokazin.DdlLtlf.OnlineDemo
dotnet ef database update
```

This command creates the database (if missing) and applies the latest migration.

## Run the web app

```bash
cd ddl-ltlf/src/Mprokazin.DdlLtlf.OnlineDemo
ConnectionStrings__Default="Host=localhost;Port=5432;Database=ddl_onlinedemo;Username=postgres;Password=secret" \
  dotnet run
```

The app listens on `https://localhost:5001` by default. On first visit it issues an anonymous cookie, seeds an introductory project, and enforces the daily token budget.

## Testing

Unit tests covering quota enforcement, save-before-run logic, and data constraints live in `Mprokazin.DdlLtlf.OnlineDemo.Tests`:

```bash
cd ddl-ltlf/src
ConnectionStrings__Default="Host=localhost;Port=5432;Database=ddl_onlinedemo;Username=postgres;Password=secret" \
  dotnet test
```

> **Note:** Some existing solution tests require the Z3 native library. If it is not installed, those tests will fail. The new web demo tests do not depend on Z3.
