-- create or replace function graphpage(connectionstart integer[], connectionend integer[]) returns setof integer as $$
-- declare
--     queue Integer[] := array[start];
-- begin
--     create temporary table visited (id integer NOT NULL) on commit drop;
--     create unique index on visited (id);

--     WHILE array_length(queue,1) > 0 LOOP
--         insert into visited (select unnest(queue)) on conflict do nothing;
--         queue := array(
--             select b
--             from (select unnest(queue) as id) as q
--             join edges on a = q.id
--             left outer join visited on b = visited.id
--             where visited.id is NULL
--         );
--     END LOOP;
--     return query (select id from visited);
-- end;
-- $$ language plpgsql;

-- induced subgraph: postids -> ajacency list
DROP FUNCTION induced_subgraph(integer[]);
create or replace function induced_subgraph(postids integer[]) returns table(postid integer, title text, targetids integer[]) as $$
begin
    return query (
        select post.id, post.title, array_agg(c.targetid)
        from post
        left join connection c on c.sourceid = post.id AND c.targetid = ANY(postids)
        where post.id = ANY(postids)
        group by post.id, post.title
    );
end;
$$ language plpgsql;
select * from induced_subgraph('{11,12,13,14}'::int[]);
