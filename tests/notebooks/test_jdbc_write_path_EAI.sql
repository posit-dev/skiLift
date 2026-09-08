-- =========================================================================
-- EAI Setup: test_jdbc_write_path
-- =========================================================================
-- Network Rule and External Access Integration for this notebook.
-- Run as a role with CREATE INTEGRATION privileges (e.g. ACCOUNTADMIN).
--
-- After running, attach the EAI to your notebook in Snowsight:
--   Notebook settings > External access > TEST_JDBC_WRITE_PATH_EAI
-- =========================================================================

CREATE OR REPLACE NETWORK RULE TEST_JDBC_WRITE_PATH_NR
  MODE = EGRESS
  TYPE = HOST_PORT
  VALUE_LIST = (
    'api.anaconda.org',
    'api.github.com',
    'binstar-cio-packages-prod.s3.amazonaws.com',
    'bioconductor.org',
    'cloud.r-project.org',
    'codeload.github.com',
    'conda.anaconda.org',
    'files.pythonhosted.org',
    'github.com',
    'micro.mamba.pm',
    'objects.githubusercontent.com',
    'pypi.org',
    'release-assets.githubusercontent.com',
    'repo.anaconda.com'
  );

CREATE OR REPLACE EXTERNAL ACCESS INTEGRATION TEST_JDBC_WRITE_PATH_EAI
  ALLOWED_NETWORK_RULES = (TEST_JDBC_WRITE_PATH_NR)
  ENABLED = TRUE;

-- Grant usage to your notebook role (uncomment and adjust):
-- GRANT USAGE ON INTEGRATION TEST_JDBC_WRITE_PATH_EAI TO ROLE <YOUR_ROLE>;
