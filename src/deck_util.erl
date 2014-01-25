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
    Hearts = [ Card || Card <- Deck, element(1, Card) =:= hearts ],
    Spades = [ Card || Card <- Deck, element(1, Card) =:= spades ],
    Diamonds = [ Card || Card <- Deck, element(1, Card) =:= diamonds ],
    Clubs = [ Card || Card <- Deck, element(1, Card) =:= clubs ],
    lists:sort(Hearts) ++ lists:sort(Spades) ++ lists:sort(Diamonds)
    ++ lists:sort(Clubs).

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
