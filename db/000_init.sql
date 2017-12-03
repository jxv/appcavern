create table if not exists "user"
  ( "id" serial primary key
  , "ident" text not null
  , "password" text
  , constraint "unique_user" unique ("ident")
  );

create table if not exists "email"
  ( "id" serial primary key
  , "email" text not null
  , "user_id" integer not null
  , "verkey" text
  , constraint "unique_email" unique ("email")
  );

create table if not exists "app"
  ( "id" serial primary key
  , "ident" text not null
  , "created" timestamp not null
  , "name" text not null
  , "device" text not null
  , "subtitle" text not null
  , "released" timestamp not null
  , "info" text not null
  , "author" text not null
  , "porters" text not null
  , "page" text
  , "img" text not null
  , "link" text not null
  , constraint "unique_a" unique ("ident")
  );
