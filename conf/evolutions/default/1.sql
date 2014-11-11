# --- Created by Slick DDL
# To stop Slick DDL generation, remove this comment and start using Evolutions

# --- !Ups

create table "PARTS" ("name" VARCHAR NOT NULL PRIMARY KEY,"description" VARCHAR NOT NULL);

# --- !Downs

drop table "PARTS";

