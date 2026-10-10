ALTER TABLE projects.git_credentials
  DROP CONSTRAINT git_credentials_project_id_host_account_key,
  ADD CONSTRAINT git_credentials_project_host_origin_account_key
    UNIQUE NULLS NOT DISTINCT (project_id, host, api_base, account);
