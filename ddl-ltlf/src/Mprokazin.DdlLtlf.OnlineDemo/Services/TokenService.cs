using Mprokazin.DdlLtlf.OnlineDemo.Data;
using Mprokazin.DdlLtlf.OnlineDemo.Models;

namespace Mprokazin.DdlLtlf.OnlineDemo.Services;

public class TokenService(DemoDb db, RequestUserContext userContext)
{
    public const int ConvertCost = 64;
    public const int SolveCost = 256;

    public int DailyBudget => userContext.User.DailyTokenBudget;

    public int DailySpent => userContext.DailySpentTokens;

    public Task EnsureWithinBudgetAsync(int tokens, CancellationToken cancellationToken)
    {
        if (DailySpent + tokens > DailyBudget)
        {
            throw new TokenBudgetExceededException();
        }

        return Task.CompletedTask;
    }

    public async Task RecordUsageAsync(int projectId, int inputId, int inputTokens, int outputTokens, string model, CancellationToken cancellationToken)
    {
        var run = new GenerateRun
        {
            UserId = userContext.UserId,
            ProjectId = projectId,
            InputId = inputId,
            InputTokens = inputTokens,
            OutputTokens = outputTokens,
            Model = model,
            GeneratedAt = DateTimeOffset.UtcNow,
        };

        db.GenerateRuns.Add(run);
        await db.SaveChangesAsync(cancellationToken);
        userContext.DailySpentTokens += inputTokens + outputTokens;
    }
}

public class TokenBudgetExceededException : Exception;
