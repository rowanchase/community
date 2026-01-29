-- Add RSVP type column to events
ALTER TABLE events
  ADD COLUMN rsvp_type text NOT NULL DEFAULT 'no_rsvp'
  CHECK (rsvp_type IN ('no_attendance', 'no_rsvp', 'with_rsvp', 'external_rsvp'));

-- Add external RSVP URL column (only used when rsvp_type = 'external_rsvp')
ALTER TABLE events
  ADD COLUMN external_rsvp_url text;

-- Create RSVPs table for storing individual RSVP records
CREATE TABLE rsvps (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  event_id uuid NOT NULL REFERENCES events(id) ON DELETE CASCADE,
  full_name text NOT NULL,
  phone text,
  email text,
  adults integer NOT NULL DEFAULT 0,
  children integer NOT NULL DEFAULT 0,
  created_at timestamptz NOT NULL DEFAULT now()
);

-- Add index on event_id for faster lookups
CREATE INDEX idx_rsvps_event_id ON rsvps(event_id);
