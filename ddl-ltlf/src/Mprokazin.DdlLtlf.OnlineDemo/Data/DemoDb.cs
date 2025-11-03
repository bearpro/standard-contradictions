using Microsoft.EntityFrameworkCore;
using Mprokazin.DdlLtlf.OnlineDemo.Models;

namespace Mprokazin.DdlLtlf.OnlineDemo.Data;

public class DemoDb(DbContextOptions<DemoDb> options) : DbContext(options)
{
    public DbSet<User> Users => Set<User>();
    public DbSet<Project> Projects => Set<Project>();
    public DbSet<Input> Inputs => Set<Input>();
    public DbSet<Output> Outputs => Set<Output>();
    public DbSet<SolveRun> SolveRuns => Set<SolveRun>();
    public DbSet<GenerateRun> GenerateRuns => Set<GenerateRun>();

    protected override void OnModelCreating(ModelBuilder modelBuilder)
    {
        base.OnModelCreating(modelBuilder);

        modelBuilder.Entity<User>(entity =>
        {
            entity.HasMany(u => u.Projects)
                .WithOne(p => p.User)
                .HasForeignKey(p => p.UserId)
                .OnDelete(DeleteBehavior.Cascade);
        });

        modelBuilder.Entity<Project>(entity =>
        {
            entity.HasMany(p => p.Inputs)
                .WithOne(i => i.Project)
                .HasForeignKey(i => i.ProjectId)
                .OnDelete(DeleteBehavior.Cascade);

            entity.Property(p => p.Name).HasMaxLength(200);
        });

        modelBuilder.Entity<Input>(entity =>
        {
            entity.Property(i => i.Name).HasMaxLength(200);
            entity.Property(i => i.Text).HasMaxLength(64 * 1024);

            entity.HasOne(i => i.Output)
                .WithOne(o => o.Input)
                .HasForeignKey<Output>(o => o.InputId)
                .OnDelete(DeleteBehavior.Cascade);
        });

        modelBuilder.Entity<Output>(entity =>
        {
            entity.Property(o => o.Text).HasMaxLength(64 * 1024);
            entity.HasIndex(o => o.InputId).IsUnique();
        });

        modelBuilder.Entity<GenerateRun>(entity =>
        {
            entity.HasKey(gr => new { gr.UserId, gr.ProjectId, gr.InputId, gr.GeneratedAt });
        });

        modelBuilder.Entity<SolveRun>(entity =>
        {
            entity.HasOne(sr => sr.Project)
                .WithMany(p => p.SolveRuns)
                .HasForeignKey(sr => sr.ProjectId);
        });
    }
}
