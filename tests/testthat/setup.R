# Force sequential execution in tests to avoid parallelization issues
# This prevents FutureInterruptError from MultisessionFuture workers
future::plan(future::sequential)
