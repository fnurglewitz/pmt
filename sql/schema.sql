create table public.auth (
    user_id bigint,
    request_ts timestamp with time zone,
    enabled boolean,
    max_price_checks integer,
    max_track_requests integer,
    username varchar(1024),
    constraint pk_auth primary key (user_id)
);

create table public.track_request (
    request_id bigserial,
    user_id bigint not null,
    name varchar(2048),
    pod_url text,
    tracked boolean,
    search_query jsonb,
    last_track_ts timestamp with time zone not null,
    created_at timestamp with time zone not null,
    constraint pk_track_request primary key (request_id),
    constraint uq_tr_user_id_name unique (user_id, name)
);

create table public.hit (
    trade_id varchar(256) not null,
    note varchar(256) not null,
    hit jsonb,
    created_at timestamp with time zone not null,
    constraint pk_hit primary key(trade_id, note)
);

create table public.track_request_hit (
    request_id integer not null,
    trade_id varchar(256) not null,
    note varchar(256) not null,
    constraint pk_track_request_trade_listing primary key (request_id, trade_id, note),
    constraint fk_hit foreign key (trade_id, note) references hit(trade_id, note)
);

create table public.price_check (
    pc_id bigserial,
    user_id bigint not null,
    name varchar(256) not null,
    url varchar(2048) not null,
    search_query jsonb not null,
    config varchar(256),
    created_at timestamp with time zone not null,
    constraint price_check_pk primary key(pc_id),
    constraint uq_pc_user_id_name unique (user_id, name)
);