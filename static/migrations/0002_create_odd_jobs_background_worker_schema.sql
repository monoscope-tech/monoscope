CREATE TABLE IF NOT EXISTS background_jobs 
  ( id serial primary key 
  , created_at timestamp with time zone default now() not null 
  , updated_at timestamp with time zone default now() not null 
  , run_at timestamp with time zone default now() not null 
  , status text not null 
  , payload jsonb not null 
  , last_error jsonb null 
  , attempts int not null default 0 
  , locked_at timestamp with time zone null 
  , locked_by text null 
  , constraint incorrect_locking_info CHECK ((status <> 'locked' and locked_at is null and locked_by is null) or (status = 'locked' and locked_at is not null and locked_by is not null)) 
  ); 

  create index if not exists idx_background_jobs_created_at on background_jobs(created_at); 
  create index if not exists idx_background_jobs_updated_at on background_jobs(updated_at); 
  create index if not exists idx_background_jobs_locked_at on background_jobs(locked_at); 
  create index if not exists idx_background_jobs_locked_by on background_jobs(locked_by); 
  create index if not exists idx_background_jobs_status on background_jobs(status); 
  create index if not exists idx_background_jobs_run_at on background_jobs(run_at);


  create or replace function notify_job_monitor_for_background_jobs() returns trigger as $$ 
  begin  
    perform pg_notify('background_jobs',  
      json_build_object('id', new.id, 'run_at', new.run_at, 'locked_at', new.locked_at)::text);  
    return new;  
  end;  
  $$ language plpgsql; 
  drop trigger if exists trg_notify_job_monitor_for_background_jobs on background_jobs; 
  create trigger trg_notify_job_monitor_for_background_jobs after insert on background_jobs for each row execute procedure notify_job_monitor_for_background_jobs();
