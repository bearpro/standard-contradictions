namespace Mprokazin.DdlLtlf.OnlineDemo.Models;

public class Project
{
    public int Id { get; set; }

    public Guid UserId { get; set; }

    public User User { get; set; } = default!;

    public string Name { get; set; } = string.Empty;

    public DateTimeOffset CreatedAt { get; set; }

    public DateTimeOffset UpdatedAt { get; set; }

    public ICollection<Input> Inputs { get; set; } = new List<Input>();

    public ICollection<SolveRun> SolveRuns { get; set; } = new List<SolveRun>();
}
