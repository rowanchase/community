-- Add CHECK constraint to ensure external_rsvp events have a URL
ALTER TABLE events
ADD CONSTRAINT external_rsvp_requires_url
CHECK (
  rsvp_type != 'external_rsvp' OR external_rsvp_url IS NOT NULL
);
