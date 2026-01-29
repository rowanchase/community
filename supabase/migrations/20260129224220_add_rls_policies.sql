-- Enable Row Level Security on both tables
ALTER TABLE events ENABLE ROW LEVEL SECURITY;
ALTER TABLE rsvps ENABLE ROW LEVEL SECURITY;

-- Events table policies
-- Policy 1: Anyone can read events
CREATE POLICY "Events are publicly readable"
  ON events
  FOR SELECT
  TO anon, authenticated
  USING (true);

-- Policy 2: Only authenticated users can insert events
CREATE POLICY "Authenticated users can insert events"
  ON events
  FOR INSERT
  TO authenticated
  WITH CHECK (true);

-- Policy 3: Only authenticated users can update events
CREATE POLICY "Authenticated users can update events"
  ON events
  FOR UPDATE
  TO authenticated
  USING (true)
  WITH CHECK (true);

-- Policy 4: Only authenticated users can delete events
CREATE POLICY "Authenticated users can delete events"
  ON events
  FOR DELETE
  TO authenticated
  USING (true);

-- RSVPs table policies
-- Policy 1: Anyone can insert RSVPs
CREATE POLICY "Anyone can insert RSVPs"
  ON rsvps
  FOR INSERT
  TO anon, authenticated
  WITH CHECK (true);

-- Policy 2: Only authenticated users can read RSVPs
CREATE POLICY "Only authenticated users can read RSVPs"
  ON rsvps
  FOR SELECT
  TO authenticated
  USING (true);
