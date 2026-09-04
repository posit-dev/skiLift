# ADBC in Snowflake Workspace Notebooks

skiLift optionally accelerates reads and writes via the ADBC Snowflake
Go driver (`adbcsnowflake`). In Workspace Notebooks (SPCS containers),
ADBC connects via the internal SPCS gateway using the container's OAuth
token -- no PAT, public endpoint, or authentication policy configuration
is required.

## Architecture (updated March 2026)

```
Workspace container (SPCS)
  ├── ADBC Go driver ──► internal SPCS gateway ──► Snowflake
  │     ALL SQL (reads, writes, DDL, metadata) via SNOWFLAKE_HOST
  │     uses SPCS OAuth token (auth_oauth)
  │     ** Preferred -- Arrow-native, no public endpoint needed **
  │
  ├── REST API v2 ──► internal SPCS gateway ──► Snowflake
  │     uses Bearer + SPCS OAuth token on SNOWFLAKE_HOST
  │     ** Used when ADBC not installed -- no public endpoint needed **
  │
  └── Snowpark write_pandas ──► internal SPCS gateway ──► Snowflake
        uses SPCS OAuth token (Snowpark Python client)
```

Both ADBC and REST API v2 connect via the internal SPCS gateway
(`SNOWFLAKE_HOST`). The public endpoint (`<account>.snowflakecomputing.com`)
is not required for R connectivity from Workspace.

**Key finding:** All official Snowflake drivers (Go/ADBC, JDBC, ODBC,
Python connector, .NET) accept the SPCS OAuth token at
`/snowflake/session/token` when connecting via `SNOWFLAKE_HOST`. The
token must be passed as a connection **property** (not embedded in a URL
string) with authenticator set to `oauth`. This was confirmed via the
`spcs_snowdriver_lib` reference implementations (Go, Java, Python, .NET)
and validated for ADBC, JDBC, and ODBC in our Workspace testing.

### Corrected understanding (retracted constraints)

The earlier version of this document stated that:
1. The SPCS token was "only accepted by blessed clients" -- **incorrect**.
   All drivers work when connecting via `SNOWFLAKE_HOST` with `auth_oauth`.
2. `SNOWFLAKE_HOST` "rejects non-blessed OAuth clients" -- **incorrect**.
   The internal host accepts OAuth from all drivers.
3. PAT + authentication policy was required for ADBC -- **no longer needed**.
   PAT remains available as a fallback for external (non-Workspace) use.

The real issue was that earlier testing connected ADBC to the **public
endpoint** (`<account>.snowflakecomputing.com`) instead of the internal
SPCS gateway (`SNOWFLAKE_HOST`), and embedded the OAuth token in a URL
string where special characters were mangled.

## Constraint 4: Go Compiler Required

`adbcsnowflake` compiles a Go-based driver from source. The Go compiler
must be installed before `install.packages("adbcsnowflake")`.

**Error**: `The Go compiler is required to install this package.`

**Resolution**: Use the setup script with `--adbc` flag, which installs
Go via `micromamba install -n r_env -c conda-forge go`.

Do NOT try to install Go from within a `%%R` cell -- the R process
spawned by rpy2 has a minimal PATH that won't find micromamba.

## Constraint 5: EAI Must Whitelist All Redirect Chains

Snowflake network rules apply at each HTTP redirect hop. If any host in
a chain is missing, the download fails silently.

Key chains for ADBC installation:
| Chain | Hosts |
|---|---|
| ADBC R packages | `community.r-multiverse.org` -> `cdn.r-universe.dev` |
| Go module downloads | `proxy.golang.org` -> `storage.googleapis.com` |
| Go checksums | `sum.golang.org` (direct) |

**Note:** Snowflake connectivity hosts (`<account>.snowflakecomputing.com`)
are **not required** for R connectivity from Workspace. Both ADBC and REST
API v2 route through the internal SPCS gateway (`SNOWFLAKE_HOST`).

See `internal/prd_eng/workspace_notebooks_eai_requirements.md` for the
complete host list and example SQL.

## Constraint 6: Large Result Sets -- Stage Hosts (Under Review)

When ADBC connected via the **public endpoint**, large result sets required
stage host access for Arrow chunk downloads from cloud storage. With ADBC
now connecting via the **internal SPCS gateway**, chunked results may be
handled internally without external stage access.

**Status:** Under investigation. Keep stage hosts in your EAI until
confirmed unnecessary. Query them with:

```sql
SELECT t.value:type::VARCHAR AS type,
       t.value:host::VARCHAR AS host,
       t.value:port          AS port
FROM TABLE(FLATTEN(input => PARSE_JSON(SYSTEM$ALLOWLIST()))) AS t
WHERE t.value:type::VARCHAR = 'STAGE';
```

## Validated Benchmarks (March 2026)

Tested on Workspace Notebook with ADBC via internal SPCS host + OAuth:

| Operation | Path | Time | Notes |
|---|---|---|---|
| Read 50K rows | ADBC (internal host) | **6.4-7.5s** | Down from ~21s via public endpoint (2.8-3.3x faster) |
| Read 50K rows | REST API v2 (public endpoint) | 2.6s | JSON transport, lower overhead for moderate result sets |
| Write 5K rows | ADBC CREATE + INSERT (internal host) | 16.1s | Server-side generation via GENERATOR() |
| Write 50K rows | Snowpark write_pandas | ~2.5s | Fastest, but requires reticulate dependency |

ADBC scales better than REST for very large result sets (100K+ rows) due to
Arrow-native columnar transport. REST API v2 is lighter for small/moderate
queries due to lower setup overhead.

### REST API v2 on the Internal SPCS Gateway

The SQL API v2 endpoint (`/api/v2/statements`) on the internal SPCS gateway
(`SNOWFLAKE_HOST`) accepts the SPCS OAuth token via standard Bearer auth:

```
Authorization: Bearer <SPCS_TOKEN>
X-Snowflake-Authorization-Token-Type: OAUTH
```

This was validated in March 2026 for SELECT, CREATE TABLE, DROP TABLE, and
INSERT operations from both Python (`requests`) and R (`httr2`).

**Note:** Earlier testing (Feb 2026) observed HTTP 401 errors, but those
were caused by the R client still hitting the **public** endpoint due to a
stale package load, not by an internal gateway limitation. Once `sf_host()`
was updated to route OAuth to `SNOWFLAKE_HOST`, Bearer auth works correctly.

## Constraint 7: Bulk Write Threshold and Routing

skiLift v0.5+ auto-routes bulk writes based on environment:

| Environment | Auto-route (above threshold) | Fallback |
|---|---|---|
| **Workspace** | ADBC bulk ingest (internal host) | Snowpark `write_pandas`, then literal |
| **External** | ADBC bulk ingest | literal INSERT |

The threshold is `skiLift.bulk_write_threshold` (200,000 cells in
Workspace, 50,000 cells externally). The old name
`skiLift.adbc_write_threshold` is kept as a backwards-compatible alias.

**Routing change (March 2026):** In Workspace, ADBC is now preferred over
Snowpark `write_pandas` for auto writes. ADBC via the internal SPCS host
achieves ~7s for 50K rows -- competitive with Snowpark (~2.5s) but without
the reticulate/Python cross-language dependency. Snowpark remains available
as an explicit opt-in (`upload_method = "snowpark"`) and as a fallback when
ADBC packages are not installed.

### Explicit method overrides

| `upload_method` | Behavior |
|---|---|
| `"auto"` (default) | ADBC > Snowpark (Workspace only) > literal |
| `"snowpark"` | Force Snowpark write_pandas (falls back to literal if unavailable) |
| `"adbc"` | Force ADBC (falls back to literal if unavailable) |
| `"literal"` | SQL INSERT only |

### Snowpark write_pandas in Workspace

The Snowpark path uses `get_active_session()` from the Workspace Python
kernel. The R data.frame is converted in-memory via `reticulate::r_to_py()`
(~0.2s for 50K rows), then written via `session$write_pandas()`.

### Snowpark write_pandas outside Workspace

Requires `snowflake-snowpark-python` and `pandas` in the Python environment.
skiLift creates a Snowpark session from the connection's credentials
(key-pair JWT or PAT). Set `options(skiLift.upload_method = "snowpark")`
to opt in.

## PAT Authentication (Fallback / External Use)

PAT remains supported for external (non-Workspace) connections and as a
fallback if the SPCS token is unavailable. Configuration:

- Short-term bypass:
  `ALTER USER ... ADD PROGRAMMATIC ACCESS TOKEN ... MINS_TO_BYPASS_NETWORK_POLICY_REQUIREMENT = 60`
- Permanent auth policy:
  ```sql
  CREATE AUTHENTICATION POLICY pat_no_network_policy
    PAT_POLICY = (NETWORK_POLICY_EVALUATION = ENFORCED_NOT_REQUIRED);
  ALTER USER <user> SET AUTHENTICATION POLICY = pat_no_network_policy;
  ```

## Discovery History

These constraints were discovered iteratively during Workspace testing
(Feb-Mar 2026). The original "blessed client" assumption was corrected
after discovering the `spcs_snowdriver_lib` reference implementations,
which showed that all official Snowflake drivers work with SPCS OAuth
when connecting via `SNOWFLAKE_HOST`. The full conversation transcript
is preserved at `internal/WorkspaceNotebooksR_ADBC_Setup_GleanConversation.md`.
