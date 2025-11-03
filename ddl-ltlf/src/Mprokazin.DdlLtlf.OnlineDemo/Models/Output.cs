namespace Mprokazin.DdlLtlf.OnlineDemo.Models;

public class Output
{
    public int Id { get; set; }

    public int InputId { get; set; }

    public Input Input { get; set; } = default!;

    public string Text { get; set; } = string.Empty;

    public DateTimeOffset ConvertedAt { get; set; }
}
