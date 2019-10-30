CREATE TABLE user (email text not null, username text unique not null, passwordHash text not null, salt text not null, bio text, image text);
