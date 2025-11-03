using Mprokazin.DdlLtlf.OnlineDemo.Models;

namespace Mprokazin.DdlLtlf.OnlineDemo.Services;

public class RequestUserContext
{
    public Guid UserId { get; set; }

    public User User { get; set; } = default!;

    public int DailySpentTokens { get; set; }
}
