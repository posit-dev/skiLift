# Development notebooks — not shipped

These are **manually-run development harnesses**, not examples. They were moved here from
`inst/notebooks/` on 8 September 2026 because they installed into every user's package
library, where they read as example content they are not.

`tests/notebooks/` is excluded from the built tarball via `.Rbuildignore`, matching the
existing convention for `tests/integration/` and `tests/python/`. They are still in git and
still runnable — nothing was deleted.

**To run one:** these are Snowflake Workspace Notebooks (Python kernel + `%%R` magic). They
call `setup_notebook()` from `sfnb_setup.py`, which now lives in `inst/notebooks/` rather
than beside them — upload `sfnb_setup.py` (and `r_helpers.py`) alongside the notebook in
Workspace, as you would have before the move. Each notebook's paired `*_EAI.sql` sets up the
external access integrations it needs.

They are deliberately **not** part of the notebook-to-Quarto conversion: that work targets
example and demo content for the Posit runtime, and a perf or write-path harness is neither.
