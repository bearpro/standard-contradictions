using System.Text.Json;
using Microsoft.EntityFrameworkCore;
using Mprokazin.DdlLtlf.OnlineDemo.Data;
using Mprokazin.DdlLtlf.OnlineDemo.Models;
using Mprokazin.DdlLtlf.OnlineDemo.ViewModels;

namespace Mprokazin.DdlLtlf.OnlineDemo.Services;

public class SolveService(DemoDb db, TokenService tokenService, RequestUserContext userContext)
{
    private const string StubModel = "stub-solve";

    public async Task<IReadOnlyList<SolveConflict>> SolveAsync(int projectId, DateTimeOffset clientSavedUtc, CancellationToken cancellationToken)
    {
        var project = await db.Projects
            .Include(p => p.Inputs)
            .ThenInclude(i => i.Output)
            .FirstOrDefaultAsync(p => p.Id == projectId && p.UserId == userContext.UserId, cancellationToken)
            ?? throw new InvalidOperationException("Project not found");

        EnsureProjectFresh(project, clientSavedUtc);
        await tokenService.EnsureWithinBudgetAsync(TokenService.SolveCost, cancellationToken);

        var conflicts = new List<SolveConflict>();
        foreach (var input in project.Inputs.OrderBy(i => i.Order))
        {
            if (input.Output is null)
            {
                conflicts.Add(new SolveConflict(
                    string.IsNullOrWhiteSpace(input.Name) ? null : input.Name,
                    "MissingOutput",
                    "Run Convert first"
                ));
            }
        }

        if (conflicts.Count == 0)
        {
            conflicts.Add(new SolveConflict(null, "Info", "No conflicts (stub)"));
        }

        var run = new SolveRun
        {
            ProjectId = projectId,
            RanAt = DateTimeOffset.UtcNow,
            Ok = conflicts.All(c => c.Kind == "Info"),
            DiagnosticsJson = JsonSerializer.Serialize(conflicts),
        };

        db.SolveRuns.Add(run);
        await db.SaveChangesAsync(cancellationToken);
        await tokenService.RecordUsageAsync(projectId, 0, TokenService.SolveCost, 0, StubModel, cancellationToken);
        return conflicts;
    }

    private static void EnsureProjectFresh(Project project, DateTimeOffset clientSavedUtc)
    {
        if (project.UpdatedAt < clientSavedUtc)
        {
            throw new ProjectOutOfDateException();
        }
    }
}
