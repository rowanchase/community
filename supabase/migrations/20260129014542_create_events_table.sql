CREATE TABLE events (
    id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
    title text NOT NULL,
    description text NOT NULL,
    start_time timestamp NOT NULL,
    end_time timestamp NOT NULL,
    location text NOT NULL,
    image_url text,
    created_at timestamptz NOT NULL DEFAULT now(),
    updated_at timestamptz
);
