create schema vinapu;

set search_path=vinapu;

create domain vinapu.element_type as smallint default 1;

create domain vinapu.load_factor as double precision default 0.5;

create domain vinapu.element_desc as character varying(20);

CREATE TABLE vinapu.elements (
    oid serial primary key,
    sys_id integer NOT NULL references geometry.systems(oid),
    dsc vinapu.element_desc NOT NULL,
    n1 integer NOT NULL references geometry.nodes(oid),
    n2 integer NOT NULL references geometry.nodes(oid),
    plw vinapu.load_factor NOT NULL,
    w1 materials.dim4 not null,
    w2 materials.dim4,
    angle geometry.angle NOT NULL,
    element_type vinapu.element_type NOT NULL
);


CREATE TABLE vinapu.element_loads (
    el_id integer NOT NULL references vinapu.elements(oid),
    ld_id integer NOT NULL references materials.loads(oid),
    load_factor load_factor NOT NULL,
    form_factor load_factor NOT NULL
);


create view vinapu.v_element_loads as 
select e.oid,e.sys_id,e.dsc as e_dsc,e.n1,nx1.dsc as n1_dsc,e.n2,nx2.dsc as n2_dsc,e.plw,e.w1,e.w2,e.angle,e.element_type,
lx.ld_id,lx.load_factor,lx.form_factor,m.dsc as m_dsc,m.lcat,m.q,lx.form_factor*m.q as service_limit,
lx.load_factor*lx.form_factor*m.q as ultimate_limit 
from vinapu.elements e 
join vinapu.element_loads lx on lx.el_id=e.oid 
join geometry.nodes nx1 on nx1.oid=e.n1
join geometry.nodes nx2 on nx2.oid=e.n2
join materials.loads m on m.oid=lx.ld_id;

