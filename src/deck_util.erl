-module(deck_util).

-include("include/deck.hrl").

-export([new_deck/0,
         deck_size/1,
         draw_top_card/1,
         draw_N_cards/2,
         shuffle_deck/1,
         sort_deck/1
        ]).

new_deck() ->
    ?DECK.

deck_size(Deck) ->
    length(Deck).

draw_top_card([Firstcard | RestDeck]) ->
    {Firstcard, RestDeck}.

draw_N_cards(N, Deck) ->
    lists:split(N, Deck).

sort_deck(Deck) ->
    %% Hearts = extract_suit(hearts, Deck),
    %% Spades = extract_suit(spades, Deck),
    %% Diamonds = extract_suit(diamonds, Deck),
    %% Clubs = extract_suit(clubs, Deck),
    %% lists:sort(Hearts) ++ lists:sort(Spades) ++ lists:sort(Diamonds)
    %% ++ lists:sort(Clubs).
    lists:sort(fun is_card_lower/2, Deck).

%% extract_suit(Suit, Deck) ->
%%     [ Card || Card <- Deck, element(1, Card) =:= Suit ].

is_card_lower({Suit, Value}, {Suit, Value2}) ->
    Value =< Value2;
is_card_lower({hearts, _}, _) ->
    true;
is_card_lower({clubs, _}, _) ->
    false;
is_card_lower({_,_}, {hearts,_}) ->
    false;
is_card_lower({_,_}, {clubs, _}) ->
    true;
is_card_lower({spades,_}, _) ->
    true;
is_card_lower(_,_) ->
    false.

shuffle_deck(List) ->
    randomize(round(math:log(length(List)) + 0.5), List).

randomize(1, List) ->
    randomize(List);
randomize(T, List) ->
   lists:foldl(fun(_E, Acc) ->
                  randomize(Acc)
               end, randomize(List), lists:seq(1, (T - 1))).

randomize(List) ->
   D = lists:map(fun(A) ->
                    {random:uniform(), A}
             end, List),
   {_, D1} = lists:unzip(lists:keysort(1, D)),
   D1.
