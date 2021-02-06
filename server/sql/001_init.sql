DROP TABLE IF EXISTS users;
DROP TABLE IF EXISTS snippets;
DROP TABLE IF EXISTS likes;
DROP TABLE IF EXISTS comments;
CREATE TABLE users (
    id bigserial PRIMARY KEY,
    username text NOT NULL UNIQUE,
    email text NOT NULL UNIQUE,
    "password" text NOT NULL,
    created_on timestamptz NOT NULL DEFAULT now()
);
CREATE TABLE snippets (
    id bigserial PRIMARY KEY,
    title text NOT NULL,
    description text,
    content text NOT NULL,
    created_on timestamptz NOT NULL DEFAULT now()
);
CREATE TABLE likes();
CREATE TABLE comments();