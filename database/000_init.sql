create table if not exists "user"
  ( "id" serial primary key
  , "public" text not null
  , "password" text
  , constraint "unique_user" unique ("public")
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
  , "public" text not null
  , "created" timestamp not null
  , "name" text not null
  , "device" text not null
  , "subtitle" text not null
  , "released" timestamp not null
  , "info" text not null
  , "page" text
  , "img" text not null
  , "link" text not null
  , constraint "unique_a" unique ("public")
  );

create table if not exists "app_x_author"
  ( "id" serial primary key
  , "app_public" text not null
  , "author_name" text /* Either auther_name or user_public must exist */
  , "user_public" text
  , foreign key ("app_public") references "app" ("public")
  , foreign key ("user_public") references "user" ("public")
  );

create table if not exists "app_x_porter"
  ( "id" serial primary key
  , "app_public" text not null
  , "porter_name" text /* Either porter_name or user_public must exist */
  , "user_public" text
  , foreign key ("app_public") references "app" ("public")
  , foreign key ("user_public") references "user" ("public")
  );
