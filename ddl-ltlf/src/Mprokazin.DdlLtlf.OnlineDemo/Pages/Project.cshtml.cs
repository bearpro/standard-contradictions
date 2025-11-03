using System.Text.Json;
using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Mvc.RazorPages;
using Mprokazin.DdlLtlf.OnlineDemo.Services;
using Mprokazin.DdlLtlf.OnlineDemo.ViewModels;

namespace Mprokazin.DdlLtlf.OnlineDemo.Pages;

public class ProjectModel(ProjectService projectService, RequestUserContext userContext) : PageModel
{
    private static readonly JsonSerializerOptions JsonOptions = new(JsonSerializerDefaults.Web)
    {
        WriteIndented = false,
    };

    public string InitialProjectJson { get; private set; } = "{}";

    public string InitialQuotaJson { get; private set; } = "{}";

    public string CsrfToken => userContext.UserId.ToString();

    public string UserDisplayName => userContext.UserId.ToString()[..8];

    public DateTimeOffset LastSavedUtc { get; private set; }

    public async Task<IActionResult> OnGetAsync(int? id, CancellationToken cancellationToken)
    {
        ProjectDto project;
        try
        {
            project = await LoadProjectAsync(id, cancellationToken);
        }
        catch (InvalidOperationException)
        {
            return NotFound();
        }

        InitialProjectJson = JsonSerializer.Serialize(project, JsonOptions);
        LastSavedUtc = project.UpdatedUtc;

        var quota = new QuotaResponse(userContext.User.DailyTokenBudget, userContext.DailySpentTokens, userContext.User.LastResetUtc);
        InitialQuotaJson = JsonSerializer.Serialize(quota, JsonOptions);

        ViewData["ProjectName"] = project.Name;
        ViewData["UserDisplayName"] = UserDisplayName;
        ViewData["CsrfToken"] = CsrfToken;
        ViewData["LastSavedUtc"] = LastSavedUtc.ToString("O");
        return Page();
    }

    private async Task<ProjectDto> LoadProjectAsync(int? id, CancellationToken cancellationToken)
    {
        if (id.HasValue)
        {
            return ProjectDtoMapper.ToDto(await projectService.GetProjectAsync(userContext.UserId, id.Value, cancellationToken));
        }

        var summaries = await projectService.GetProjectsAsync(userContext.UserId, cancellationToken);
        if (summaries.Count == 0)
        {
            throw new InvalidOperationException("Project not found");
        }

        var project = await projectService.GetProjectAsync(userContext.UserId, summaries[0].Id, cancellationToken);
        return ProjectDtoMapper.ToDto(project);
    }
}
