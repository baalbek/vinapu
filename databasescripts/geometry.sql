create schema geometry;

create domain geometry.coord as double precision;

create domain geometry.dof as smallint;

create domain geometry.element_desc as character varying(20);

create domain geometry.system_name as character varying(20);

create domain geometry.project_name as character varying(30);

create domain geometry.location_name as character varying(30);

create domain geometry.created_date as date default now();

create domain geometry.floor_plan as smallint;

create table geometry.projects (
 oid serial primary key,
 pn geometry.project_name not null,
 created_date geometry.created_date not null
);

create table geometry.locations (
 oid serial primary key,
 project_id integer NOT NULL references geometry.projects(oid),
 loc_name geometry.location_name not null 
);

create table geometry.nodes (
 oid serial primary key,
 loc_id integer NOT NULL references geometry.locations(oid),
 x geometry.coord NOT NULL,
 y geometry.coord DEFAULT 0.0 NOT NULL,
 z geometry.coord DEFAULT 0.0 NOT NULL,
 dofx geometry.dof DEFAULT 0 NOT NULL,
 dofz geometry.dof DEFAULT 0 NOT NULL,
 dofm geometry.dof DEFAULT 1 NOT NULL,
 dsc geometry.element_desc
);

create table geometry.systems (
 oid serial primary key,
 loc_id integer NOT NULL references geometry.locations(oid),
 sys_name geometry.system_name not null,
 created_date geometry.created_date not null
);

create table geometry.system_floor_plans (
    sys_id integer not null references geometry.systems(oid),
    floor_plan geometry.floor_plan not null 
);


set search_path=geometry;

create domain geometry.angle as double precision default 0.0;
