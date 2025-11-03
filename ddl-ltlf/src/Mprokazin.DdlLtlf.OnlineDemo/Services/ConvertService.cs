using System.Net.Http.Headers;
using System.Text;
using System.Text.Json;
using System.Linq;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.Options;
using Mprokazin.DdlLtlf.Language.Ast;
using Mprokazin.DdlLtlf.Language;
using Mprokazin.DdlLtlf.OnlineDemo.Data;
using Mprokazin.DdlLtlf.OnlineDemo.Models;
using Mprokazin.DdlLtlf.OnlineDemo.Options;

namespace Mprokazin.DdlLtlf.OnlineDemo.Services;

public class ConvertService(
    DemoDb db,
    TokenService tokenService,
    RequestUserContext userContext,
    IHttpClientFactory httpClientFactory,
    IOptions<OpenAiOptions> optionsAccessor)
{
    private static readonly JsonSerializerOptions SerializerOptions = new()
    {
        PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
        DefaultIgnoreCondition = System.Text.Json.Serialization.JsonIgnoreCondition.WhenWritingNull,
    };

    private const string ResponsesPath = "responses";

    private static readonly string SystemPrompt = """
---

# SYSTEM / INSTRUCTIONS

You translate natural-language regulatory/technical text into compilable **DDL-LTLf** code.

## Goal

Produce a **complete program** that parses with the grammar below and type-checks in the target toolchain. No explanations, no diffs—**output code only**. If you uncertain about code generated - you can use line comments starting from '#'

## What you can generate (per grammar)

* **type** definitions (product/sum; optional field types).
* **predicate** definitions (with optional parameter/field type annotations).
* **deontic statements**: `obligated|permitted|forbidden|suggested <predicateBody> [when <predicateBody>]`.
* Expressions: constants (`int|rational|bool`), algebra (`+ - * / %`), comparisons (`< <= = >= > <>`), logical (`not and or`), tuples, constructor calls, name references (`a.b.c`), pattern checks with `is`.
* Optional expression type annotations: `expr : typeDescription`.

> Do **not** use features not present in the grammar (e.g., temporal/defeasible operators, attributes, `fact`, comments, or any custom syntax).

## Grammar (authoritative)

Use exactly these tokens and forms:

```
<PASTE THE USER’S GRAMMAR HERE OR ASSUME IT IS AVAILABLE TO THE MODEL>
```

(If the grammar text is provided separately by the caller, treat it as authoritative.)

## Naming & style

* `NAME` → `[a-zA-Z_][a-zA-Z_0-9]*`. Prefer `snake_case`.
* Use consistent, domain-meaningful names (e.g., `tube`, `wall_thickness`, `is_tube`, `big_enough`).
* Keep numbers and units in predicates/field names (e.g., encode units in the name, not in syntax).

## Mapping from natural language → DSL

1. **Entities / data** → `type` definitions (product or sum).

   * Product example: `type Tube = (length: rational, diameter: rational)`.
   * Sum example: `type Shape = (Tube of (length: rational, diameter: rational) | Cube of (side: rational))`.
2. **Classifications** → predicates (possibly via patterns):

   * `predicate is_tube(x) = x.shape is Tube(_, _)`.
3. **Quantitative constraints** → predicates using algebra & comparisons:

   * `predicate big_enough(t: Tube) = t.length > 55`.
4. **Normative statements** → deontic statements with optional `when`:

   * `obligated big_enough(t) when is_tube(t)`.
5. If a constraint is used only once, you **may** inline it directly inside a deontic statement.
6. When ambiguous types occur, add annotations (`: int|rational|bool` or richer types).
7. Prefer pattern-matching with `is` for sum-type checks; use field access for product types.

## Output rules (very important)

* Output **only** valid DDL-LTLf code (no prose, no markdown fences).
* Emit a sequence of `topLevelDefinition`s (any order is fine).
* No comments, no placeholders, no TODOs.
* If the caller provides parse/type errors, **fix them and re-emit the entire program only.**

## Quality checklist before emitting

* [ ] Every referenced `NAME` is declared or is a parameter.
* [ ] Field accesses match declared product fields.
* [ ] Constructors used only for declared sum/product variants.
* [ ] Operators/keywords exactly match grammar (`and`, `or`, `not`, `=`, `<>`, etc.).
* [ ] Numbers are `int` or `rational` per intended math; annotate if needed.
* [ ] No unsupported features (temporal/defeasible/facts/attributes/comments).
* [ ] Program is non-empty and includes at least one **deontic statement**.

---

# EXECUTION

Wait for natural-language input. Produce only the DDL-LTLf program per the rules above. If errors are returned later, correct and re-emit the full program only.
""";

    private readonly IHttpClientFactory _httpClientFactory = httpClientFactory;
    private readonly OpenAiOptions _options = optionsAccessor.Value;

    public async Task<Output> ConvertAsync(int inputId, DateTimeOffset clientSavedUtc, CancellationToken cancellationToken)
    {
        var input = await db.Inputs
            .Include(i => i.Output)
            .Include(i => i.Project)
            .FirstOrDefaultAsync(i => i.Id == inputId && i.Project.UserId == userContext.UserId, cancellationToken)
            ?? throw new InvalidOperationException("Input not found");

        EnsureProjectFresh(input.Project, clientSavedUtc);
        await tokenService.EnsureWithinBudgetAsync(TokenService.ConvertCost, cancellationToken);

        var generation = await GenerateAsync(input.Text, cancellationToken);
        ValidateGeneratedCode(generation.Output);

        var now = DateTimeOffset.UtcNow;
        if (input.Output is null)
        {
            input.Output = new Output
            {
                Text = generation.Output,
                ConvertedAt = now,
            };
        }
        else
        {
            input.Output.Text = generation.Output;
            input.Output.ConvertedAt = now;
        }

        await db.SaveChangesAsync(cancellationToken);

        await tokenService.RecordUsageAsync(
            input.ProjectId,
            input.Id,
            generation.InputTokens,
            generation.OutputTokens,
            generation.Model,
            cancellationToken);

        return input.Output!;
    }

    private async Task<GenerationResult> GenerateAsync(string inputText, CancellationToken cancellationToken)
    {
        if (string.IsNullOrWhiteSpace(_options.ApiKey))
        {
            throw new ConvertFailedException("OpenAI API key is not configured.");
        }

        if (string.IsNullOrWhiteSpace(_options.Model))
        {
            throw new ConvertFailedException("OpenAI model is not configured.");
        }

        var baseUrl = string.IsNullOrWhiteSpace(_options.BaseUrl)
            ? "https://api.openai.com/v1"
            : _options.BaseUrl!;

        var requestUri = $"{baseUrl.TrimEnd('/')}/{ResponsesPath}";

        var requestPayload = new ResponseRequest(
            _options.Model,
            new[]
            {
                new ResponseMessage("system", new[] { new ResponseContent("text", SystemPrompt) }),
                new ResponseMessage("user", new[] { new ResponseContent("text", inputText) }),
            },
            _options.MaxOutputTokens > 0 ? _options.MaxOutputTokens : null,
            _options.Temperature);

        using var request = new HttpRequestMessage(HttpMethod.Post, requestUri)
        {
            Content = new StringContent(JsonSerializer.Serialize(requestPayload, SerializerOptions), Encoding.UTF8, "application/json"),
        };

        request.Headers.Authorization = new AuthenticationHeaderValue("Bearer", _options.ApiKey);
        request.Headers.Accept.Add(new MediaTypeWithQualityHeaderValue("application/json"));
        request.Headers.TryAddWithoutValidation("OpenAI-Beta", "assistants=v2");

        HttpResponseMessage response;
        try
        {
            var client = _httpClientFactory.CreateClient(nameof(ConvertService));
            response = await client.SendAsync(request, cancellationToken);
        }
        catch (Exception ex) when (ex is HttpRequestException or TaskCanceledException)
        {
            throw new ConvertFailedException($"Failed to reach OpenAI: {ex.Message}");
        }

        var responseText = await response.Content.ReadAsStringAsync(cancellationToken);

        if (!response.IsSuccessStatusCode)
        {
            var message = TryExtractError(responseText) ?? response.ReasonPhrase ?? "unknown error";
            throw new ConvertFailedException($"OpenAI request failed: {message}");
        }

        try
        {
            using var document = JsonDocument.Parse(responseText);
            var output = ExtractOutput(document)
                ?? throw new ConvertFailedException("OpenAI response did not contain any text output.");

            var usage = ExtractUsage(document);

            return new GenerationResult(
                output.Trim(),
                usage.InputTokens,
                usage.OutputTokens,
                _options.Model);
        }
        catch (JsonException ex)
        {
            throw new ConvertFailedException($"Failed to parse OpenAI response: {ex.Message}");
        }
    }

    private static string? ExtractOutput(JsonDocument document)
    {
        if (!document.RootElement.TryGetProperty("output", out var outputArray))
        {
            return null;
        }

        foreach (var item in outputArray.EnumerateArray())
        {
            if (!item.TryGetProperty("content", out var contentArray))
            {
                continue;
            }

            foreach (var content in contentArray.EnumerateArray())
            {
                if (content.TryGetProperty("text", out var textProp) && textProp.ValueKind == JsonValueKind.String)
                {
                    return textProp.GetString();
                }

                if (content.TryGetProperty("value", out var valueProp) && valueProp.ValueKind == JsonValueKind.String)
                {
                    return valueProp.GetString();
                }
            }
        }

        return null;
    }

    private static (int InputTokens, int OutputTokens) ExtractUsage(JsonDocument document)
    {
        if (!document.RootElement.TryGetProperty("usage", out var usageElement))
        {
            return (TokenService.ConvertCost, 0);
        }

        var inputTokens = usageElement.TryGetProperty("input_tokens", out var inputProp) && inputProp.TryGetInt32(out var input)
            ? input
            : TokenService.ConvertCost;

        var outputTokens = usageElement.TryGetProperty("output_tokens", out var outputProp) && outputProp.TryGetInt32(out var output)
            ? output
            : 0;

        return (inputTokens, outputTokens);
    }

    private static string? TryExtractError(string responseText)
    {
        try
        {
            using var document = JsonDocument.Parse(responseText);
            if (document.RootElement.TryGetProperty("error", out var errorElement))
            {
                if (errorElement.TryGetProperty("message", out var messageProp) && messageProp.ValueKind == JsonValueKind.String)
                {
                    return messageProp.GetString();
                }

                if (errorElement.ValueKind == JsonValueKind.String)
                {
                    return errorElement.GetString();
                }
            }
        }
        catch (JsonException)
        {
            // Ignore parsing errors when extracting error details.
        }

        return null;
    }

    private static void ValidateGeneratedCode(string code)
    {
        if (string.IsNullOrWhiteSpace(code))
        {
            throw new ConvertFailedException("OpenAI response was empty.");
        }

        Microsoft.FSharp.Collections.FSharpList<Definition> program;
        try
        {
            program = Parser.parse(code);
        }
        catch (Exception ex)
        {
            throw new ConvertFailedException($"Failed to parse generated code: {ex.Message}");
        }

        var typeResult = Typing.inferTypes(program);
        if (!typeResult.IsOk)
        {
            var errors = typeResult.ErrorValue;
            var message = string.Join(
                Environment.NewLine,
                errors.Select(FormatTypeError));
            throw new ConvertFailedException($"Type checking failed:{Environment.NewLine}{message}");
        }
    }

    private static string FormatTypeError(Mprokazin.DdlLtlf.Language.Typing.TypeError error)
    {
        var range = error.Range;
        var location = range.StartLine > 0
            ? $" (line {range.StartLine}, column {range.StartChar})"
            : string.Empty;
        return $"- {error.Message}{location}";
    }

    private static void EnsureProjectFresh(Project project, DateTimeOffset clientSavedUtc)
    {
        if (project.UpdatedAt < clientSavedUtc)
        {
            throw new ProjectOutOfDateException();
        }
    }

    private sealed record ResponseRequest(
        string Model,
        IReadOnlyList<ResponseMessage> Input,
        int? MaxOutputTokens,
        double Temperature);

    private sealed record ResponseMessage(string Role, IReadOnlyList<ResponseContent> Content);

    private sealed record ResponseContent(string Type, string Text);

    private sealed record GenerationResult(string Output, int InputTokens, int OutputTokens, string Model);
}

public class ProjectOutOfDateException : Exception;

public class ConvertFailedException(string message) : Exception(message);
