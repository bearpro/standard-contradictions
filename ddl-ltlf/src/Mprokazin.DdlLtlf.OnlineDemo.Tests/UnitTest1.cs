using Microsoft.Data.Sqlite;
using Microsoft.EntityFrameworkCore;
using Mprokazin.DdlLtlf.OnlineDemo.Data;
using Mprokazin.DdlLtlf.OnlineDemo.Models;
using Mprokazin.DdlLtlf.OnlineDemo.Services;

namespace Mprokazin.DdlLtlf.OnlineDemo.Tests;

public class DemoTests
{
    [Fact]
    public async Task TokenService_PreventsOverspending()
    {
        await using var db = CreateInMemoryDb();
        var now = DateTimeOffset.UtcNow;
        var user = new User
        {
            Id = Guid.NewGuid(),
            CreatedAt = now,
            LastResetUtc = now,
            DailyTokenBudget = 64,
        };
        db.Users.Add(user);
        await db.SaveChangesAsync();

        var context = new RequestUserContext
        {
            UserId = user.Id,
            User = user,
            DailySpentTokens = 64,
        };

        var service = new TokenService(db, context);
        await Assert.ThrowsAsync<TokenBudgetExceededException>(() => service.EnsureWithinBudgetAsync(1, CancellationToken.None));
    }

    [Fact]
    public async Task ConvertAndSolveRequireFreshSave()
    {
        await using var db = CreateInMemoryDb();
        var now = DateTimeOffset.UtcNow;
        var user = new User
        {
            Id = Guid.NewGuid(),
            CreatedAt = now,
            LastResetUtc = now,
            DailyTokenBudget = 1024,
        };
        var project = new Project
        {
            User = user,
            UserId = user.Id,
            Name = "Test",
            CreatedAt = now,
            UpdatedAt = now,
            Inputs =
            {
                new Input { Name = "a", Text = "x", Order = 0 },
            },
        };

        db.Users.Add(user);
        db.Projects.Add(project);
        await db.SaveChangesAsync();

        var context = new RequestUserContext
        {
            UserId = user.Id,
            User = user,
            DailySpentTokens = 0,
        };

        var convertService = new ConvertService(db, new TokenService(db, context), context);
        var solveService = new SolveService(db, new TokenService(db, context), context);
        var staleTimestamp = project.UpdatedAt.AddMinutes(1);

        await Assert.ThrowsAsync<ProjectOutOfDateException>(() => convertService.ConvertAsync(project.Inputs.First().Id, staleTimestamp, CancellationToken.None));
        await Assert.ThrowsAsync<ProjectOutOfDateException>(() => solveService.SolveAsync(project.Id, staleTimestamp, CancellationToken.None));
    }

    [Fact]
    public async Task OutputIsUniquePerInput()
    {
        using var connection = new SqliteConnection("Filename=:memory:");
        await connection.OpenAsync();

        var options = new DbContextOptionsBuilder<DemoDb>()
            .UseSqlite(connection)
            .Options;

        await using var db = new DemoDb(options);
        await db.Database.EnsureCreatedAsync();

        var now = DateTimeOffset.UtcNow;
        var user = new User
        {
            Id = Guid.NewGuid(),
            CreatedAt = now,
            LastResetUtc = now,
            DailyTokenBudget = 1024,
        };
        var project = new Project
        {
            User = user,
            UserId = user.Id,
            Name = "Test",
            CreatedAt = now,
            UpdatedAt = now,
            Inputs =
            {
                new Input { Name = "input", Text = "text", Order = 0 },
            },
        };

        db.Users.Add(user);
        db.Projects.Add(project);
        await db.SaveChangesAsync();

        var input = project.Inputs.First();
        db.Outputs.Add(new Output { InputId = input.Id, Text = "first", ConvertedAt = now });
        await db.SaveChangesAsync();

        db.Outputs.Add(new Output { InputId = input.Id, Text = "second", ConvertedAt = now });
        await Assert.ThrowsAsync<DbUpdateException>(() => db.SaveChangesAsync());
    }

    private static DemoDb CreateInMemoryDb()
    {
        var options = new DbContextOptionsBuilder<DemoDb>()
            .UseInMemoryDatabase(Guid.NewGuid().ToString())
            .Options;
        return new DemoDb(options);
    }
}
