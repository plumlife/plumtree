%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 Helium Systems, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-module(plumtree_peer_service_manager).

-define(TBL, cluster_state).

-export([init/0, get_local_state/0, get_actor/0, update_state/1, delete_state/0]).

init() ->
    %% Setup ETS table for cluster_state
    DBPath = data_root(),
    case filelib:is_dir(DBPath) of
        true  -> initialize_lets(DBPath);
        false ->
            initialize_lets(DBPath),
            add_self()
    end,

    migrate_old_flatfile(),

    ok.

initialize_lets(Path) ->
    lets:new( cluster_state
            , [ set
              , compressed
              , public
              , named_table
              , {db, [ {filter_policy, {bloom, 16}}
                     , {create_if_missing, true}
                     , {path, Path}
                     ]}
              , {db_write, [ {sync, true} ]}
              ]),
    gen_actor().

%% @doc return local node's view of cluster membership
get_local_state() ->
   case hd(lets:lookup(?TBL, cluster_state)) of
       {cluster_state, State} ->
           {ok, State};
       _Else ->
           {error, _Else}
   end.

%% @doc return local node's current actor
get_actor() ->
    try
        case hd(lets:lookup(?TBL, actor)) of
            {actor, Actor} ->
                {ok, Actor};
            _Else ->
                {error, _Else}
        end
    catch
        _:E -> {error, E}
    end.

%% @doc update cluster_state
update_state(State) ->
    lets:insert(?TBL, {cluster_state, State}).

delete_state() ->
    delete_state_from_disk().

%%% ------------------------------------------------------------------
%%% internal functions
%%% ------------------------------------------------------------------

%% @doc initialize singleton cluster
add_self() ->
    Initial = riak_dt_orswot:new(),

    Actor = case get_actor() of
        {ok, A}    -> A;
        {error, _} -> gen_actor()
    end,

    {ok, LocalState} = riak_dt_orswot:update({add, node()}, Actor, Initial),
    update_state(LocalState). 

%% @doc generate an actor for this node while alive
gen_actor() ->
    Node = atom_to_list(node()),
    {M, S, U} = now(),
    TS = integer_to_list(M * 1000 * 1000 * 1000 * 1000 + S * 1000 * 1000 + U),
    Term = Node ++ TS,
    Actor = crypto:hash(sha, Term),
    lets:insert(?TBL, {actor, Actor}),
    Actor.

data_root() ->
    case application:get_env(plumtree, plumtree_data_dir) of
        {ok, PRoot} -> 
            Dir = filename:join(PRoot, "peer_service"),
            filelib:ensure_dir(Dir),
            Dir;
        undefined   -> 
            {ok, Dir} = file:get_cwd(),
            NewDir = filename:join(Dir, "peer_service"),
            filelib:ensure_dir(NewDir),
            NewDir
    end.

delete_state_from_disk() ->
    os:cmd("rm -rf " ++ data_root() ++ " && sync").

%%% ------------------------------------------------------------------
%%% OLD FUNCTIONS FOR BACKWARDS COMPAT
%%% ------------------------------------------------------------------

migrate_old_flatfile() ->
    case old_data_root() of
        undefined ->
            void;
        Dir ->
            case filelib:is_regular(filename:join(Dir, "cluster_state")) of
                true ->
                    {ok, Bin} = file:read_file(filename:join(Dir,
                                                             "cluster_state")),
                    {ok, State} = riak_dt_orswot:from_binary(Bin),
                    lager:info("read state from file, migrating...", [State]),

                    %% Remove the old cluster state file
                    os:cmd("rm -f " ++ filename:join(Dir, "cluster_state") ++ " && sync"),

                    %% Update our leveldb backed cluster state with what we imported
                    update_state(State);
                false ->
                    void
            end
    end.

old_data_root() ->
    case application:get_env(plumtree, plumtree_data_dir) of
        {ok, PRoot} -> filename:join(PRoot, "peer_service");
        undefined -> undefined
    end.
