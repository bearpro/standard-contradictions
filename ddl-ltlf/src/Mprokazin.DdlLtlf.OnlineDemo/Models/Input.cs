namespace Mprokazin.DdlLtlf.OnlineDemo.Models;

public class Input
{
    public int Id { get; set; }

    public int ProjectId { get; set; }

    public Project Project { get; set; } = default!;

    public string Name { get; set; } = string.Empty;

    public string Text { get; set; } = string.Empty;

    public int Order { get; set; }

    public Output? Output { get; set; }
}
