namespace Mprokazin.DdlLtlf.OnlineDemo.Models;

public class GenerateRun
{
    public Guid UserId { get; set; }

    public int ProjectId { get; set; }

    public int InputId { get; set; }

    public int InputTokens { get; set; }

    public int OutputTokens { get; set; }

    public string Model { get; set; } = string.Empty;

    public DateTimeOffset GeneratedAt { get; set; }
}
