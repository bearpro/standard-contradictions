using Microsoft.EntityFrameworkCore;
using Mprokazin.DdlLtlf.OnlineDemo.Data;
using Mprokazin.DdlLtlf.OnlineDemo.Models;
using Mprokazin.DdlLtlf.OnlineDemo.ViewModels;

namespace Mprokazin.DdlLtlf.OnlineDemo.Services;

public class ProjectService(DemoDb db)
{
    private const int MaxInputsPerProject = 20;

    private static readonly (string Name, string Text)[] DefaultInputs =
    {
        ("Tubes.txt", "Tubes provide structure and routes for the flow of components."),
        ("Pipes.txt", "Pipes connect distant parts of the system and must be sealed."),
    };

    public async Task EnsureDefaultProjectAsync(Guid userId, CancellationToken cancellationToken)
    {
        var hasProjects = await db.Projects.AnyAsync(p => p.UserId == userId, cancellationToken);
        if (hasProjects)
        {
            return;
        }

        var now = DateTimeOffset.UtcNow;
        var project = new Project
        {
            UserId = userId,
            Name = "Intro to ddl-ltlf",
            CreatedAt = now,
            UpdatedAt = now,
            Inputs = DefaultInputs.Select((tuple, index) => new Input
            {
                Name = tuple.Name,
                Text = tuple.Text,
                Order = index,
            }).ToList(),
        };

        db.Projects.Add(project);
        await db.SaveChangesAsync(cancellationToken);
    }

    public Task<List<ProjectSummaryDto>> GetProjectsAsync(Guid userId, CancellationToken cancellationToken) =>
        db.Projects.Where(p => p.UserId == userId)
            .OrderByDescending(p => p.UpdatedAt)
            .Select(p => new ProjectSummaryDto(p.Id, p.Name, p.UpdatedAt))
            .ToListAsync(cancellationToken);

    public async Task<Project> GetProjectAsync(Guid userId, int projectId, CancellationToken cancellationToken)
    {
        var project = await db.Projects
            .Include(p => p.Inputs)
            .ThenInclude(i => i.Output)
            .FirstOrDefaultAsync(p => p.Id == projectId && p.UserId == userId, cancellationToken);

        if (project is null)
        {
            throw new InvalidOperationException("Project not found");
        }

        return project;
    }

    public async Task<Project> CreateProjectAsync(Guid userId, string name, CancellationToken cancellationToken)
    {
        var now = DateTimeOffset.UtcNow;
        var project = new Project
        {
            UserId = userId,
            Name = name,
            CreatedAt = now,
            UpdatedAt = now,
        };

        db.Projects.Add(project);
        await db.SaveChangesAsync(cancellationToken);
        return project;
    }

    public async Task<Project> SaveProjectAsync(Guid userId, int projectId, SaveProjectRequest request, CancellationToken cancellationToken)
    {
        var project = await db.Projects
            .Include(p => p.Inputs)
            .ThenInclude(i => i.Output)
            .FirstOrDefaultAsync(p => p.Id == projectId && p.UserId == userId, cancellationToken)
            ?? throw new InvalidOperationException("Project not found");

        if (request.Inputs.Count > MaxInputsPerProject)
        {
            throw new InvalidOperationException($"Projects can contain at most {MaxInputsPerProject} inputs.");
        }

        var now = DateTimeOffset.UtcNow;
        project.Name = request.Name;
        project.UpdatedAt = now;

        var incomingById = request.Inputs.Where(i => i.Id.HasValue).ToDictionary(i => i.Id!.Value, i => i);

        // Update existing inputs
        foreach (var input in project.Inputs.ToList())
        {
            if (!incomingById.TryGetValue(input.Id, out var incoming))
            {
                db.Inputs.Remove(input);
                project.Inputs.Remove(input);
                continue;
            }

            if (incoming.Text != input.Text && input.Output is not null)
            {
                db.Outputs.Remove(input.Output);
                input.Output = null;
            }

            input.Name = incoming.Name;
            input.Text = incoming.Text;
            input.Order = incoming.Order;
        }

        // Add new inputs
        var nextOrder = project.Inputs.Count;
        foreach (var incoming in request.Inputs.Where(i => !i.Id.HasValue))
        {
            var newInput = new Input
            {
                Name = incoming.Name,
                Text = incoming.Text,
                Order = incoming.Order >= 0 ? incoming.Order : nextOrder++,
            };
            project.Inputs.Add(newInput);
        }

        await db.SaveChangesAsync(cancellationToken);
        return project;
    }

    public async Task<Project> DuplicateProjectAsync(Guid userId, int projectId, string name, CancellationToken cancellationToken)
    {
        var source = await db.Projects
            .Include(p => p.Inputs)
            .ThenInclude(i => i.Output)
            .FirstOrDefaultAsync(p => p.Id == projectId && p.UserId == userId, cancellationToken)
            ?? throw new InvalidOperationException("Project not found");

        var now = DateTimeOffset.UtcNow;
        var project = new Project
        {
            UserId = userId,
            Name = name,
            CreatedAt = now,
            UpdatedAt = now,
            Inputs = source.Inputs.Select(i => new Input
            {
                Name = i.Name,
                Text = i.Text,
                Order = i.Order,
                Output = i.Output is null ? null : new Output
                {
                    Text = i.Output.Text,
                    ConvertedAt = i.Output.ConvertedAt,
                }
            }).ToList(),
        };

        db.Projects.Add(project);
        await db.SaveChangesAsync(cancellationToken);
        return project;
    }

    public async Task<Input> CreateInputAsync(Guid userId, int projectId, string name, string text, CancellationToken cancellationToken)
    {
        var project = await db.Projects
            .Include(p => p.Inputs)
            .FirstOrDefaultAsync(p => p.Id == projectId && p.UserId == userId, cancellationToken)
            ?? throw new InvalidOperationException("Project not found");

        if (project.Inputs.Count >= MaxInputsPerProject)
        {
            throw new InvalidOperationException($"Projects can contain at most {MaxInputsPerProject} inputs.");
        }

        var input = new Input
        {
            Name = name,
            Text = text,
            Order = project.Inputs.Count == 0 ? 0 : project.Inputs.Max(i => i.Order) + 1,
        };

        project.Inputs.Add(input);
        project.UpdatedAt = DateTimeOffset.UtcNow;
        await db.SaveChangesAsync(cancellationToken);
        return input;
    }

    public async Task<Input> UpdateInputAsync(Guid userId, int inputId, UpdateInputRequest request, CancellationToken cancellationToken)
    {
        var input = await db.Inputs
            .Include(i => i.Project)
            .Include(i => i.Output)
            .FirstOrDefaultAsync(i => i.Id == inputId && i.Project.UserId == userId, cancellationToken)
            ?? throw new InvalidOperationException("Input not found");

        if (request.Name is not null)
        {
            input.Name = request.Name;
        }

        if (request.Text is not null)
        {
            if (request.Text != input.Text && input.Output is not null)
            {
                db.Outputs.Remove(input.Output);
                input.Output = null;
            }

            input.Text = request.Text;
        }

        if (request.Order is not null)
        {
            input.Order = request.Order.Value;
        }

        input.Project.UpdatedAt = DateTimeOffset.UtcNow;
        await db.SaveChangesAsync(cancellationToken);
        return input;
    }

    public async Task DeleteInputAsync(Guid userId, int inputId, CancellationToken cancellationToken)
    {
        var input = await db.Inputs
            .Include(i => i.Project)
            .FirstOrDefaultAsync(i => i.Id == inputId && i.Project.UserId == userId, cancellationToken)
            ?? throw new InvalidOperationException("Input not found");

        db.Inputs.Remove(input);
        input.Project.UpdatedAt = DateTimeOffset.UtcNow;
        await db.SaveChangesAsync(cancellationToken);
    }
}
