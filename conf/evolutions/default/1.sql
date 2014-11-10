# --- Created by Slick DDL
# To stop Slick DDL generation, remove this comment and start using Evolutions

# --- !Ups

create table "PARTS" ("name" VARCHAR NOT NULL PRIMARY KEY,"description" VARCHAR NOT NULL);
create table "RATES" ("partName" VARCHAR NOT NULL,"wagesTo" DOUBLE NOT NULL,"contributionEmployer" DOUBLE,"contributionEmployee" DOUBLE);

# --- !Downs

drop table "RATES";
drop table "PARTS";

