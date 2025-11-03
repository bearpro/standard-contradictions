namespace Mprokazin.DdlLtlf.OnlineDemo.Models;

public class SolveRun
{
    public int Id { get; set; }

    public int ProjectId { get; set; }

    public Project Project { get; set; } = default!;

    public DateTimeOffset RanAt { get; set; }

    public bool Ok { get; set; }

    public string DiagnosticsJson { get; set; } = "[]";
}
