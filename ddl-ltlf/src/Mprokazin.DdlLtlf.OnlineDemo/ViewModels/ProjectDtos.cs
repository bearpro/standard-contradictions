using System.ComponentModel.DataAnnotations;
using Mprokazin.DdlLtlf.OnlineDemo.Models;

namespace Mprokazin.DdlLtlf.OnlineDemo.ViewModels;

public record ProjectSummaryDto(int Id, string Name, DateTimeOffset UpdatedUtc);

public record OutputDto(int? Id, string Text, DateTimeOffset? ConvertedUtc);

public record InputDto(int Id, string Name, string Text, int Order, OutputDto? Output);

public record ProjectDto(int Id, string Name, DateTimeOffset UpdatedUtc, IReadOnlyList<InputDto> Inputs);

public record CreateProjectRequest([property: Required, MaxLength(200)] string Name);

public record SaveProjectRequest(
    [property: Required] string Name,
    [property: Required] IReadOnlyList<SaveProjectInput> Inputs
);

public record SaveProjectInput(
    int? Id,
    [property: Required, MaxLength(200)] string Name,
    [property: Required, MaxLength(64 * 1024)] string Text,
    int Order
);

public record SaveAsProjectRequest([property: Required, MaxLength(200)] string Name);

public record CreateInputRequest([property: Required, MaxLength(200)] string Name, [property: MaxLength(64 * 1024)] string Text);

public record UpdateInputRequest(string? Name, string? Text, int? Order);

public record ConvertResponse(int InputId, string Output, DateTimeOffset ConvertedUtc);

public record SolveConflict(string? File, string Kind, string Message);

public record SolveResponse(IReadOnlyList<SolveConflict> Conflicts);

public record QuotaResponse(int DailyBudget, int DailySpent, DateTimeOffset LastResetUtc);

public static class ProjectDtoMapper
{
    public static ProjectDto ToDto(Project project) =>
        new(project.Id, project.Name, project.UpdatedAt,
            project.Inputs
                .OrderBy(i => i.Order)
                .ThenBy(i => i.Id)
                .Select(i => new InputDto(
                    i.Id,
                    i.Name,
                    i.Text,
                    i.Order,
                    i.Output is null ? null : new OutputDto(i.Output.Id, i.Output.Text, i.Output.ConvertedAt)
                ))
                .ToList());
}
