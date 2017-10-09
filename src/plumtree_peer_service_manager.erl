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

-behaviour(gen_server).

%% API
-export([start_link/0,
         get_local_state/0,
         get_actor/0,
         update_state/1,
         members/0,
         delete_state/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Same as start_link([]).
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Return local node's view of cluster membership.
get_local_state() ->
    gen_server:call(?MODULE, get_local_state, infinity).

%% @doc Return local node's current actor.
get_actor() ->
    gen_server:call(?MODULE, get_actor, infinity).

%% @doc Update cluster state.
update_state(State) ->
    gen_server:call(?MODULE, {update_state, State}, infinity).

%% @doc Delete state.
delete_state() ->
    gen_server:call(?MODULE, delete_state, infinity).

%% @doc Fetch local member set.
members() ->
    gen_server:call(?MODULE, members, infinity).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
-spec init([]) -> {ok, #state{}}.
init([]) ->
    %% Initialize lets table for cluster_state
    DBPath = data_root(),
    case filelib:is_dir(DBPath) of
        true  -> initialize_lets(DBPath);
        false ->
            initialize_lets(DBPath),
            empty_membership()
    end,

    migrate_old_flatfile(),

    {ok, #state{}}.

%% @private
-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}}.
handle_call(get_local_state, _From, State) ->
    Result = fetch_state(),
    {reply, Result, State};
handle_call(get_actor, _From, State) ->
    Result = get_actor_from_state(),
    {reply, Result, State};
handle_call({update_state, NewState}, _From, State) ->
    update_local_state(NewState),
    {reply, ok, State};
handle_call(delete_state, _From, State) ->
    delete_state_from_disk(),
    {reply, ok, State};
handle_call(members, _From, State) ->
    {ok, Result} = fetch_state(),
    Set = riak_dt_orswot:value(Result),
    {reply, Set, State};
handle_call(Msg, _From, State) ->
    lager:warning("Unhandled messages: ~p", [Msg]),
    {reply, ok, State}.

%% @private
-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(Msg, State) ->
    lager:warning("Unhandled messages: ~p", [Msg]),
    {noreply, State}.

%% @private
-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(Msg, State) ->
    lager:warning("Unhandled messages: ~p", [Msg]),
    {noreply, State}.

%% @private
-spec terminate(term(), #state{}) -> term().
terminate(_Reason, _State) ->
    ok.

%% @private
-spec code_change(term() | {down, term()}, #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

fetch_state() ->
    try
        case hd(lets:lookup(?TBL, cluster_state)) of
            {cluster_state, State} ->
                {ok, State};
            _Else ->
                {error, _Else}
        end
    catch
        _:_ -> {ok, riak_dt_orswot:new()}
    end.

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

%%%===================================================================
%%% internal functions
%%%===================================================================

update_local_state(NewState) ->
    %% Ensure that incoming state is always merged with the state
    %% stored at the peer service; otherwise, you run the risk of a
    %% slow client reading and writing and dropping an update if
    %% clients race to update state.
    %% 
    %% This ensures a partial order relationship between updates at a
    %% given node where all updates subsume previous updates; partial
    %% order induced by <= over set containment.

    {ok, LocalState} = fetch_state(),
    riak_dt_orswot:merge(LocalState, NewState),
    lets:insert(?TBL, {cluster_state, NewState}),
    ok.

%% @doc initialize singleton cluster
empty_membership() ->
    Initial = riak_dt_orswot:new(),

    Actor = case get_actor_from_state() of
        {ok, A}    -> A;
        {error, _} -> gen_actor()
    end,

    {ok, LocalState} = riak_dt_orswot:update({add, node()}, Actor, Initial),
    update_local_state(LocalState). 

get_actor_from_state() ->
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

%% @doc generate an actor for this node while alive
gen_actor() ->
    Node = atom_to_list(node()),
    Unique = erlang:unique_integer([positive]),
    TS = integer_to_list(Unique),
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

%%%===================================================================
%%% OLD FUNCTIONS FOR BACKWARDS COMPAT
%%%===================================================================
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
                    lager:info("read state from file, migrating..."),

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
