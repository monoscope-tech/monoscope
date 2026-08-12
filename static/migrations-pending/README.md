# Prepared migrations, deliberately not applied

The migration runner is pointed at `static/migrations/` and applies everything it finds there
on start-up. Anything in this directory is written and reviewed but must **not** run yet,
because applying it early would break a currently-deployed release.

Each file states its own precondition and the `git mv` that activates it. Nothing here is
picked up by the runner, the test harness template build, or CI.
