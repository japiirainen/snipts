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
    creator BIGINT NOT NULL REFERENCES users (id),
    created_on timestamptz NOT NULL DEFAULT now()
);
CREATE TABLE likes (
    id bigserial PRIMARY KEY,
    snippet BIGINT NOT NULL REFERENCES snippets (id),
    creator BIGINT NOT NULL REFERENCES users (id),
    created_on timestamptz NOT NULL DEFAULT now(),
    direction INT NOT NULL
);
CREATE TABLE comments (
    id bigserial PRIMARY KEY,
    snippet BIGINT NOT NULL REFERENCES snippets (id),
    creator BIGINT NOT NULL REFERENCES users (id),
    created_on timestamptz NOT NULL DEFAULT now()
);