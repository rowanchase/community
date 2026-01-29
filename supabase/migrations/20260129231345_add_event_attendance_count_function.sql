-- View to show aggregate RSVP counts per event
-- Uses security_invoker = false to bypass RLS on rsvps table,
-- but only exposes aggregate counts (privacy-preserving)
CREATE VIEW event_rsvp_counts
WITH (security_invoker = false)
AS
SELECT
  event_id,
  COUNT(*) as rsvp_count,
  COALESCE(SUM(adults), 0) as adult_count,
  COALESCE(SUM(children), 0) as children_count,
  COALESCE(SUM(adults + children), 0) as people_count
FROM rsvps
GROUP BY event_id;

-- Grant SELECT permission to anonymous and authenticated users
GRANT SELECT ON event_rsvp_counts TO anon, authenticated;
