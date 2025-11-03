using System.ComponentModel.DataAnnotations;

namespace Mprokazin.DdlLtlf.OnlineDemo.Models;

public class User
{
    [Key]
    public Guid Id { get; set; }

    public DateTimeOffset CreatedAt { get; set; }

    public DateTimeOffset LastResetUtc { get; set; }

    public int DailyTokenBudget { get; set; } = 25_000;

    public ICollection<Project> Projects { get; set; } = new List<Project>();

    public ICollection<GenerateRun> GenerateRuns { get; set; } = new List<GenerateRun>();
}
