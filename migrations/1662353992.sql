-- :up
CREATE TABLE content(
  id serial NOT NULL,
  payable boolean NOT NULL,
  sender_id character varying(255) NOT NULL,
  receiver_id character varying(255) NOT NULL,
  file_type character varying(255) NOT NULL,
  mds character varying(255) NOT NULL,
  data bytea NOT NULL

  UNIQUE (sender_id, receiver_id, file_type, md5)
  CONSTRAINT content_pkey PRIMARY KEY (id)
);

CREATE INDEX index_content_on_receiver_id
  ON 
  USING btree
  (receiver_id);

-- :down
DROP TABLE content;
