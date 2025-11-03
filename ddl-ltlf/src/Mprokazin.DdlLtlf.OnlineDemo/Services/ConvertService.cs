using Microsoft.EntityFrameworkCore;
using Mprokazin.DdlLtlf.OnlineDemo.Data;
using Mprokazin.DdlLtlf.OnlineDemo.Models;

namespace Mprokazin.DdlLtlf.OnlineDemo.Services;

public class ConvertService(DemoDb db, TokenService tokenService, RequestUserContext userContext)
{
    private const string StubModel = "stub-convert";

    public async Task<Output> ConvertAsync(int inputId, DateTimeOffset clientSavedUtc, CancellationToken cancellationToken)
    {
        var input = await db.Inputs
            .Include(i => i.Output)
            .Include(i => i.Project)
            .FirstOrDefaultAsync(i => i.Id == inputId && i.Project.UserId == userContext.UserId, cancellationToken)
            ?? throw new InvalidOperationException("Input not found");

        EnsureProjectFresh(input.Project, clientSavedUtc);
        await tokenService.EnsureWithinBudgetAsync(TokenService.ConvertCost, cancellationToken);

        var now = DateTimeOffset.UtcNow;
        if (input.Output is null)
        {
            input.Output = new Output
            {
                Text = "obligated true",
                ConvertedAt = now,
            };
        }
        else
        {
            input.Output.Text = "obligated true";
            input.Output.ConvertedAt = now;
        }

        await db.SaveChangesAsync(cancellationToken);
        await tokenService.RecordUsageAsync(input.ProjectId, input.Id, TokenService.ConvertCost, 0, StubModel, cancellationToken);
        return input.Output!;
    }

    private static void EnsureProjectFresh(Project project, DateTimeOffset clientSavedUtc)
    {
        if (project.UpdatedAt < clientSavedUtc)
        {
            throw new ProjectOutOfDateException();
        }
    }
}

public class ProjectOutOfDateException : Exception;
