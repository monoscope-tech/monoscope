-- Give the rows that predate 0132 an honest starting size.
--
-- 0132 added first_member_count with a default of 0, so every verdict recorded
-- before it reads as having grown from nothing — which makes the growth test,
-- the one condition that actually separates an open set of ids from a closed
-- set of route names, trivially true for all of them.
--
-- Setting it to the size recorded at the time means growth is measured from
-- here on. Those groups have to genuinely gain a member before they qualify,
-- same as any new one.
UPDATE apis.endpoint_group_reviews
   SET first_member_count = member_count
 WHERE first_member_count = 0 AND applied_at IS NULL;
