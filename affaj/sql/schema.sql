-- To execute this file from SQL REPL:
-- \i sql/schema.sql
-- for hashing passwords
CREATE EXTENSION IF NOT EXISTS pgcrypto;
CREATE TABLE IF NOT EXISTS users (
  id TEXT NOT NULL,
  email TEXT NOT NULL,
  name TEXT NOT NULL,
  pwd_hash TEXT NOT NULL,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
);
CREATE TABLE IF NOT EXISTS snippets (
  id TEXT NOT NULL,
  title text NOT NULL,
  description text NOT NULL,
  content text NOT NULL,
  author BIGINT NOT NULL REFERENCES users (id),
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
);
ALTER TABLE ONLY users
ADD CONSTRAINT pk_users PRIMARY KEY (id);
ALTER TABLE ONLY snippets
ADD CONSTRAINT pk_snippets PRIMARY KEY (id);