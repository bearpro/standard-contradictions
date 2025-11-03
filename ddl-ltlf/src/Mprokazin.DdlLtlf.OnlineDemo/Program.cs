using System.Threading.RateLimiting;
using Microsoft.EntityFrameworkCore;
using Mprokazin.DdlLtlf.OnlineDemo.Data;
using Mprokazin.DdlLtlf.OnlineDemo.Middleware;
using Mprokazin.DdlLtlf.OnlineDemo.Services;
using Mprokazin.DdlLtlf.OnlineDemo.ViewModels;

var builder = WebApplication.CreateBuilder(args);

builder.Configuration.AddUserSecrets<Program>();

var connectionString = builder.Configuration.GetConnectionString("Default")
    ?? throw new InvalidOperationException("Connection string 'Default' not found.");

builder.Services.AddDbContext<DemoDb>(options =>
    options.UseNpgsql(connectionString));

builder.Services.AddRazorPages();
builder.Services.AddHttpContextAccessor();

builder.Services.AddScoped<RequestUserContext>();
builder.Services.AddScoped<ProjectService>();
builder.Services.AddScoped<TokenService>();
builder.Services.AddScoped<ConvertService>();
builder.Services.AddScoped<SolveService>();

builder.Services.AddRateLimiter(options =>
{
    options.AddPolicy("ConvertSolve", context =>
    {
        var uid = context.Request.Cookies.TryGetValue("ddl_uid", out var value)
            ? value
            : context.Connection.RemoteIpAddress?.ToString() ?? "anonymous";

        return RateLimitPartition.GetTokenBucketLimiter(uid, _ => new TokenBucketRateLimiterOptions
        {
            TokenLimit = 5,
            QueueProcessingOrder = QueueProcessingOrder.OldestFirst,
            QueueLimit = 0,
            ReplenishmentPeriod = TimeSpan.FromMinutes(1),
            TokensPerPeriod = 50,
            AutoReplenishment = true,
        });
    });
});

var app = builder.Build();

using (var scope = app.Services.CreateScope())
{
    var db = scope.ServiceProvider.GetRequiredService<DemoDb>();
    await db.Database.MigrateAsync();
}

if (!app.Environment.IsDevelopment())
{
    app.UseExceptionHandler("/Error");
    app.UseHsts();
}

app.UseHttpsRedirection();
app.UseStaticFiles();

app.UseRouting();
app.UseRateLimiter();

app.UseMiddleware<UserBootstrapMiddleware>();
app.UseMiddleware<CsrfMiddleware>();

app.MapGet("/", () => Results.Redirect("/project"));

app.MapRazorPages();

var api = app.MapGroup("/api");

api.MapPost("/users/bootstrap", (RequestUserContext userContext) =>
{
    return Results.Ok(new
    {
        uid = userContext.UserId,
        dailyBudget = userContext.User.DailyTokenBudget,
        dailySpent = userContext.DailySpentTokens,
        lastResetUtc = userContext.User.LastResetUtc,
    });
});

api.MapGet("/users/quota", (RequestUserContext userContext) =>
{
    return Results.Ok(new QuotaResponse(userContext.User.DailyTokenBudget, userContext.DailySpentTokens, userContext.User.LastResetUtc));
});

api.MapGet("/projects", async (ProjectService projects, RequestUserContext userContext, CancellationToken cancellationToken) =>
{
    var summaries = await projects.GetProjectsAsync(userContext.UserId, cancellationToken);
    return Results.Ok(summaries);
});

api.MapPost("/projects", async (CreateProjectRequest request, ProjectService projects, RequestUserContext userContext, CancellationToken cancellationToken) =>
{
    var project = await projects.CreateProjectAsync(userContext.UserId, request.Name, cancellationToken);
    return Results.Ok(ProjectDtoMapper.ToDto(project));
});

api.MapGet("/projects/{id:int}", async (int id, ProjectService projects, RequestUserContext userContext, CancellationToken cancellationToken) =>
{
    var project = await projects.GetProjectAsync(userContext.UserId, id, cancellationToken);
    return Results.Ok(ProjectDtoMapper.ToDto(project));
});

api.MapPut("/projects/{id:int}", async (int id, SaveProjectRequest request, ProjectService projects, RequestUserContext userContext, CancellationToken cancellationToken) =>
{
    try
    {
        var project = await projects.SaveProjectAsync(userContext.UserId, id, request, cancellationToken);
        return Results.Ok(ProjectDtoMapper.ToDto(project));
    }
    catch (InvalidOperationException ex) when (ex.Message == "Project not found")
    {
        return Results.NotFound(new { error = ex.Message });
    }
    catch (InvalidOperationException ex)
    {
        return Results.BadRequest(new { error = ex.Message });
    }
});

api.MapPost("/projects/{id:int}/saveAs", async (int id, SaveAsProjectRequest request, ProjectService projects, RequestUserContext userContext, CancellationToken cancellationToken) =>
{
    try
    {
        var project = await projects.DuplicateProjectAsync(userContext.UserId, id, request.Name, cancellationToken);
        return Results.Ok(ProjectDtoMapper.ToDto(project));
    }
    catch (InvalidOperationException ex)
    {
        return Results.NotFound(new { error = ex.Message });
    }
});

api.MapPost("/projects/{id:int}/inputs", async (int id, CreateInputRequest request, ProjectService projects, RequestUserContext userContext, CancellationToken cancellationToken) =>
{
    try
    {
        var input = await projects.CreateInputAsync(userContext.UserId, id, request.Name, request.Text, cancellationToken);
        return Results.Ok(new { id = input.Id });
    }
    catch (InvalidOperationException ex)
    {
        return Results.BadRequest(new { error = ex.Message });
    }
});

api.MapPut("/inputs/{id:int}", async (int id, UpdateInputRequest request, ProjectService projects, RequestUserContext userContext, CancellationToken cancellationToken) =>
{
    try
    {
        var input = await projects.UpdateInputAsync(userContext.UserId, id, request, cancellationToken);
        return Results.Ok(new { id = input.Id });
    }
    catch (InvalidOperationException ex)
    {
        return Results.NotFound(new { error = ex.Message });
    }
});

api.MapDelete("/inputs/{id:int}", async (int id, ProjectService projects, RequestUserContext userContext, CancellationToken cancellationToken) =>
{
    try
    {
        await projects.DeleteInputAsync(userContext.UserId, id, cancellationToken);
        return Results.NoContent();
    }
    catch (InvalidOperationException ex)
    {
        return Results.NotFound(new { error = ex.Message });
    }
});

api.MapPost("/inputs/{id:int}/convert", async (HttpRequest request, int id, ConvertService convert, CancellationToken cancellationToken) =>
{
    if (!request.Headers.TryGetValue("X-Client-LastSavedUtc", out var lastSavedValue) ||
        !DateTimeOffset.TryParse(lastSavedValue, out var lastSavedUtc))
    {
        return Results.BadRequest(new { error = "missing_last_saved" });
    }

    try
    {
        var output = await convert.ConvertAsync(id, lastSavedUtc, cancellationToken);
        return Results.Ok(new ConvertResponse(id, output.Text, output.ConvertedAt));
    }
    catch (ProjectOutOfDateException)
    {
        return Results.Json(new { error = "stale_project" }, statusCode: StatusCodes.Status409Conflict);
    }
    catch (TokenBudgetExceededException)
    {
        return Results.Json(new { error = "budget_exceeded" }, statusCode: StatusCodes.Status429TooManyRequests);
    }
    catch (InvalidOperationException ex)
    {
        return Results.NotFound(new { error = ex.Message });
    }
}).RequireRateLimiting("ConvertSolve");

api.MapPost("/projects/{id:int}/solve", async (HttpRequest request, int id, SolveService solver, CancellationToken cancellationToken) =>
{
    if (!request.Headers.TryGetValue("X-Client-LastSavedUtc", out var lastSavedValue) ||
        !DateTimeOffset.TryParse(lastSavedValue, out var lastSavedUtc))
    {
        return Results.BadRequest(new { error = "missing_last_saved" });
    }

    try
    {
        var conflicts = await solver.SolveAsync(id, lastSavedUtc, cancellationToken);
        return Results.Ok(new SolveResponse(conflicts));
    }
    catch (ProjectOutOfDateException)
    {
        return Results.Json(new { error = "stale_project" }, statusCode: StatusCodes.Status409Conflict);
    }
    catch (TokenBudgetExceededException)
    {
        return Results.Json(new { error = "budget_exceeded" }, statusCode: StatusCodes.Status429TooManyRequests);
    }
    catch (InvalidOperationException ex)
    {
        return Results.NotFound(new { error = ex.Message });
    }
}).RequireRateLimiting("ConvertSolve");

app.Run();
