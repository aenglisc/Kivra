-- Create content table
-- :up
CREATE TYPE PAYMENT_STATUS AS ENUM
    ('unpaid', 'paid');

CREATE TABLE content(
  id serial NOT NULL,
  sender_id character varying(255) NOT NULL,
  receiver_id character varying(255) NOT NULL,
  file_name character varying(255) NOT NULL,
  file_type character varying(255) NOT NULL,
  is_payable boolean NOT NULL DEFAULT FALSE,
  payment_status PAYMENT_STATUS NOT NULL DEFAULT 'unpaid',
  file_content bytea NOT NULL,
  md5 bytea NOT NULL,
  CONSTRAINT content_pkey PRIMARY KEY (id)
);

-- create index
CREATE INDEX index_content_on_receiver_id
  ON content
  USING btree
  (receiver_id COLLATE pg_catalog."default");

CREATE UNIQUE INDEX unique_content
  ON content
  (sender_id, receiver_id, md5);

-- Drop content in downgrade
-- :down
DROP TABLE content;
