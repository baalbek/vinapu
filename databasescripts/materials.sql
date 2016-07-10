create schema materials;

set search_path=materials;

create domain materials.beam_name as character varying(20);

create domain materials.dim1 as smallint;

create domain materials.dim2 as integer;

create domain materials.dim3 as numeric(8,1);

create domain materials.kg_pr_m as numeric(4,1);

create domain materials.moment_of_inertia_2 as numeric(12,2);

create domain materials.section_modulus as numeric(12,2);

create domain materials.dim4 as double precision;

create domain materials.kn as double precision default 0.0;

create domain materials.load_category as smallint default 1;

create domain materials.element_desc as character varying(20);

CREATE TABLE materials.steel_beams (
    oid serial primary key, 
    name materials.beam_name NOT NULL,
    h materials.dim1 NOT NULL,
    b materials.dim1 NOT NULL,
    area materials.dim2 NOT NULL,
    flange materials.dim3 NOT NULL,
    web materials.dim3 NOT NULL,
    weight materials.kg_pr_m NOT NULL,
    i_y materials.moment_of_inertia_2 NOT NULL,
    w_el_y materials.section_modulus NOT NULL,
    w_pl_y materials.section_modulus NOT NULL,
    i_z materials.moment_of_inertia_2 NOT NULL,
    w_el_z materials.section_modulus NOT NULL,
    w_pl_z materials.section_modulus NOT NULL
);

CREATE TABLE materials.loads (
    oid serial primary key,
    dsc materials.element_desc NOT NULL,
    lcat materials.load_category NOT NULL,
    q materials.kn NOT NULL 
);

