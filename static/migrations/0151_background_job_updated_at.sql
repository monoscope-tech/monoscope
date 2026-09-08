-- Job claims, retries, and terminal status changes must advance updated_at.
-- Use the same change-sensitive trigger as the other application tables.
CREATE TRIGGER set_updated_at
BEFORE UPDATE ON public.background_jobs
FOR EACH ROW EXECUTE FUNCTION public.set_updated_at();
