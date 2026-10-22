-- 0249 folded the fresh-install demo project (seeded 'Startup' in 0001, no subscription) into Free,
-- capping it and its monitor frequencies. Restore it; prod's demo is on another plan, so a no-op there.
UPDATE projects.projects SET payment_plan = 'Startup'
 WHERE id = '00000000-0000-0000-0000-000000000000' AND payment_plan = 'Free';
