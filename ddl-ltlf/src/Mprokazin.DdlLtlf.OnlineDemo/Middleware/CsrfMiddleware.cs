using System.Text.Json;
using Mprokazin.DdlLtlf.OnlineDemo.Services;

namespace Mprokazin.DdlLtlf.OnlineDemo.Middleware;

public class CsrfMiddleware(RequestDelegate next)
{
    public async Task InvokeAsync(HttpContext context, RequestUserContext userContext)
    {
        if (HttpMethods.IsPost(context.Request.Method) ||
            HttpMethods.IsPut(context.Request.Method) ||
            HttpMethods.IsDelete(context.Request.Method))
        {
            if (!context.Request.Headers.TryGetValue("X-CSRF-Token", out var tokenValue) ||
                !Guid.TryParse(tokenValue, out var token) ||
                token != userContext.UserId)
            {
                context.Response.StatusCode = StatusCodes.Status400BadRequest;
                context.Response.ContentType = "application/json";
                await context.Response.WriteAsync(JsonSerializer.Serialize(new { error = "invalid_csrf" }), context.RequestAborted);
                return;
            }
        }

        await next(context);
    }
}
