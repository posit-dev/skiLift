-- =========================================================================
-- EAI Setup: workspace_perf_test
-- =========================================================================
-- Network Rule and External Access Integration for this notebook.
-- Run as a role with CREATE INTEGRATION privileges (e.g. ACCOUNTADMIN).
--
-- After running, attach the EAI to your notebook in Snowsight:
--   Notebook settings > External access > WORKSPACE_PERF_TEST_EAI
-- =========================================================================

CREATE OR REPLACE NETWORK RULE WORKSPACE_PERF_TEST_NR
  MODE = EGRESS
  TYPE = HOST_PORT
  VALUE_LIST = (
    'api.anaconda.org',
    'api.github.com',
    'binstar-cio-packages-prod.s3.amazonaws.com',
    'bioconductor.org',
    'cdn.r-universe.dev',
    'cloud.r-project.org',
    'codeload.github.com',
    'community.r-multiverse.org',
    'conda.anaconda.org',
    'files.pythonhosted.org',
    'github.com',
    'micro.mamba.pm',
    'objects.githubusercontent.com',
    'proxy.golang.org',
    'pypi.org',
    'release-assets.githubusercontent.com',
    'repo.anaconda.com',
    'storage.googleapis.com',
    'sum.golang.org'
  );

CREATE OR REPLACE EXTERNAL ACCESS INTEGRATION WORKSPACE_PERF_TEST_EAI
  ALLOWED_NETWORK_RULES = (WORKSPACE_PERF_TEST_NR)
  ENABLED = TRUE;

-- Grant usage to your notebook role (uncomment and adjust):
-- GRANT USAGE ON INTEGRATION WORKSPACE_PERF_TEST_EAI TO ROLE <YOUR_ROLE>;
