using Microsoft.EntityFrameworkCore;
using Mprokazin.DdlLtlf.OnlineDemo.Data;
using Mprokazin.DdlLtlf.OnlineDemo.Models;
using Mprokazin.DdlLtlf.OnlineDemo.Services;

namespace Mprokazin.DdlLtlf.OnlineDemo.Middleware;

public class UserBootstrapMiddleware(RequestDelegate next)
{
    public async Task InvokeAsync(HttpContext context, DemoDb db, RequestUserContext userContext, ProjectService projectService)
    {
        var uid = EnsureUidCookie(context.Response, context.Request);

        var user = await db.Users.Include(u => u.Projects)
            .FirstOrDefaultAsync(u => u.Id == uid, context.RequestAborted);

        if (user is null)
        {
            user = new User
            {
                Id = uid,
                CreatedAt = DateTimeOffset.UtcNow,
                LastResetUtc = DateTimeOffset.UtcNow,
            };
            db.Users.Add(user);
            await db.SaveChangesAsync(context.RequestAborted);
        }

        await EnsureDailyResetAsync(db, user, context.RequestAborted);

        userContext.UserId = user.Id;
        userContext.User = user;

        userContext.DailySpentTokens = await db.GenerateRuns
            .Where(gr => gr.UserId == user.Id && gr.GeneratedAt.Date == DateTimeOffset.UtcNow.Date)
            .SumAsync(gr => gr.InputTokens + gr.OutputTokens, context.RequestAborted);

        await projectService.EnsureDefaultProjectAsync(user.Id, context.RequestAborted);

        await next(context);
    }

    private static Guid EnsureUidCookie(HttpResponse response, HttpRequest request)
    {
        const string CookieName = "ddl_uid";

        if (request.Cookies.TryGetValue(CookieName, out var value) && Guid.TryParse(value, out var guid))
        {
            return guid;
        }

        guid = Guid.NewGuid();
        var options = new CookieOptions
        {
            HttpOnly = true,
            IsEssential = true,
            MaxAge = TimeSpan.FromDays(365),
            SameSite = SameSiteMode.Lax,
        };
        response.Cookies.Append(CookieName, guid.ToString(), options);
        return guid;
    }

    private static async Task EnsureDailyResetAsync(DemoDb db, User user, CancellationToken cancellationToken)
    {
        var now = DateTimeOffset.UtcNow;
        if (user.LastResetUtc.Date >= now.Date)
        {
            return;
        }

        user.LastResetUtc = now;
        await db.SaveChangesAsync(cancellationToken);
    }
}
