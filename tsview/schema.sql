create table tsview.horizon (
    id serial primary key,
    label text not null unique deferrable initially deferred,
    fromdate text not null,
    todate text not null,
    rank int not null unique deferrable initially deferred
);

create index if not exists tsview_horizon_id_idx on tsview.horizon(id);
