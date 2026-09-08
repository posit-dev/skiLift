# skiLift <img src="man/figures/logo.png" align="right" height="139" />

> **Community Project -- Not Officially Supported**
> This is a community-developed project from
> [Snowflake Labs](https://github.com/Snowflake-Labs), not an official
> Snowflake offering. It is provided **as-is** without warranty or official
> Snowflake support. Use it for prototyping and experimentation; production
> use is at your own risk. Feedback, bug reports, and contributions are
> welcome via [GitHub Issues](https://github.com/posit-dev/skiLift/issues).

> **Author:** [Simon Field](https://www.linkedin.com/in/fieldy6961) — SnowCAT

A DBI-compliant R interface to [Snowflake](https://www.snowflake.com) that
connects directly via the Snowflake SQL API over HTTPS. No dependency on
ODBC, JDBC, or Python.

> **Companion package:** For Snowflake ML features (Model Registry, Feature
> Store, Datasets, SPCS model serving), see
> [**skiPatrol**](https://github.com/posit-dev/skiPatrol).

## Features

- Full [DBI](https://dbi.r-dbi.org/) compliance -- works with any package
  that speaks DBI
- **dbplyr** integration -- write dplyr pipelines that execute as Snowflake SQL,
  with 17 Snowflake-specific translations (semi-structured data, arrays,
  approximate aggregates) beyond what dbplyr provides out of the box
- **RStudio / Positron Connections Pane** -- browse databases, schemas, tables,
  and columns visually
- **Authentication**: JWT key-pair, Programmatic Access Token (PAT), OAuth,
  and Workspace session tokens
- **`connections.toml`** support -- share profiles with the Snowflake Python
  connector and CLI
- **Arrow interface** -- `dbGetQueryArrow()` / `dbFetchArrow()` via `nanoarrow`
  for DBI Arrow method compatibility
- **Optional ADBC backend** -- install `adbcsnowflake` + `adbcdrivermanager`
  for true server-side Arrow reads (zero serialization overhead) and high-performance
  bulk writes (`PUT` + `COPY INTO`). When ADBC is available, `dbGetQueryArrow()` and
  `dbWriteTable()` automatically use it; otherwise they fall back transparently to
  the SQL API
- **Snowflake Workspace Notebooks** -- auto-detects the session token for
  zero-config auth

## Installation

```r
# install.packages("pak")
pak::pak("posit-dev/skiLift")
```

## Vignettes

After installation, browse locally with `browseVignettes("skiLift")` or open:

```r
vignette("getting-started", package = "skiLift")
vignette("workspace-skilift", package = "skiLift")
```

**getting-started** covers DBI usage, authentication (JWT, PAT, Workspace),
bulk writes, ADBC options, dbplyr, and Arrow helpers. **workspace-skilift**
focuses on Workspace Notebooks: Python/`%%R` bootstrap order, EAI and installs
for optional ADBC, the internal `SNOWFLAKE_HOST` gateway, and links to
`WORKSPACE_ADBC.md` on GitHub.

## Quick Start

```r
library(DBI)
library(skiLift)

# Connect using a connections.toml profile
con <- dbConnect(Snowflake(), name = "my_profile")

# Run a query
df <- dbGetQuery(con, "SELECT * FROM my_table LIMIT 10")

# Write a data.frame
dbWriteTable(con, "iris_copy", iris)

# dbplyr
library(dplyr)
tbl(con, "my_table") |>
  filter(score > 90) |>
  collect()

# Disconnect
dbDisconnect(con)
```

## Authentication

### connections.toml (recommended)

```toml
# ~/.snowflake/connections.toml
[default]
account   = "myaccount"
user      = "myuser"
authenticator = "SNOWFLAKE_JWT"
private_key_path = "/path/to/rsa_key.p8"
database  = "MY_DB"
schema    = "PUBLIC"
warehouse = "MY_WH"
```

```r
con <- dbConnect(Snowflake())                    # uses [default]
con <- dbConnect(Snowflake(), name = "staging")  # uses [staging]
```

### Programmatic Access Token (PAT)

```r
con <- dbConnect(
  Snowflake(),
  account = "myaccount",
  token   = Sys.getenv("SNOWFLAKE_PAT")
)
```

### Snowflake Workspace Notebooks

Inside a Workspace Notebook, authentication is automatic via the built-in
SPCS OAuth token. If you use `setup_notebook()` from `sfnb_setup.py`, it
exports the necessary environment variables (`SNOWFLAKE_ACCOUNT`,
`SNOWFLAKE_USER`, etc.) so that `dbConnect(Snowflake())` works with zero
configuration:

```r
con <- dbConnect(Snowflake())
```

For PAT details, the **`SNOWFLAKE_*`** variables `setup_notebook()` exports,
`%%R` / Snowpark prerequisites, and **ADBC** behaviour in Workspace versus
local R, see **`vignette("getting-started", package = "skiLift")`**.

## Identifier Case Handling

By default, skiLift uppercases unquoted identifiers in DDL/DML
operations to match Snowflake's native behaviour and the ODBC driver:

```r
dbWriteTable(con, "my_table", data.frame(id = 1))
dbListFields(con, "my_table")
#> [1] "ID"
```

Set `options(skiLift.identifier_case = "preserve")` to keep the
original case (useful for DBItest or when lowercase column names are
required).

## Using with skiPatrol

If you use the companion `skiPatrol` package for ML workflows, you can
obtain a skiLift connection from an existing `sfr_connection`:

```r
library(skiPatrol)
conn    <- sfr_connect()
dbi_con <- sfr_dbi_connection(conn)

DBI::dbGetQuery(dbi_con, "SELECT 1")
```

## Requirements

- R >= 4.1.0
- A Snowflake account

Optional R packages: `openssl` + `jose` (JWT auth), `RcppTOML`
(connections.toml), `nanoarrow` (Arrow interface), `adbcsnowflake` +
`adbcdrivermanager` (ADBC backend for bulk Arrow reads/writes),
`dbplyr` + `dplyr`, `snowflakeauth`.

## License

Apache License 2.0
