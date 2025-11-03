namespace Mprokazin.DdlLtlf.OnlineDemo.Options;

public class OpenAiOptions
{
    public string ApiKey { get; set; } = string.Empty;

    public string Model { get; set; } = "gpt-4.1-mini";

    public string? BaseUrl { get; set; }
        = "https://api.openai.com/v1";

    public int MaxOutputTokens { get; set; } = 1024;

    public double Temperature { get; set; } = 0.2;
}
