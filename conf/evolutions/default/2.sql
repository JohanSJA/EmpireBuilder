# --- !Ups

insert into "PARTS" values
    ('A', 'About A'),
    ('B', 'About B'),
    ('C', 'About C'),
    ('D', 'About D');

# --- !Downs

delete from "PARTS";
