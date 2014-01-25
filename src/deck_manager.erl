%%%-------------------------------------------------------------------
%%% @author simon <simon@tail-f.com>
%%% @copyright (C) 2014, simon
%%% @doc
%%%
%%% @end
%%% Created : 23 Jan 2014 by simon <simon@tail-f.com>
%%%-------------------------------------------------------------------
-module(deck_manager).

-behaviour(gen_server).

%% API
-export([start_link/0,
         show_deck/0,
         shuffle_deck/0,
         deck_size/0,
         draw_top_card/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("include/deck.hrl").


-define(SERVER, ?MODULE).

-record(state, {deck}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

show_deck() ->
    gen_server:call(?MODULE, {show_deck}).

shuffle_deck() ->
    gen_server:cast(?MODULE, {shuffle_deck}).

deck_size() ->
    gen_server:call(?MODULE, {deck_size}).

draw_top_card() ->
    gen_server:call(?MODULE, {draw_top_card}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{ deck = ?DECK}}.

handle_call({show_deck}, _From, State) ->
    Reply = deck_util:return_deck(State#state.deck),
    {reply, Reply, State};
handle_call({deck_size}, _From, State) ->
    Reply = deck_util:deck_size(State#state.deck),
    {reply, Reply, State};
handle_call({draw_top_card}, _From, State) ->
    {TopCard, NewDeck} = deck_util:draw_top_card(State#state.deck),
    {reply, TopCard, #state{ deck = NewDeck }}.

handle_cast({shuffle_deck}, State) ->
    ShuffledDeck = deck_util:shuffle_deck(State#state.deck),
    {noreply, #state{ deck = ShuffledDeck }}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
