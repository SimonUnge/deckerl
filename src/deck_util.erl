-module(deck_util).

-include("include/deck.hrl").

-export([return_deck/1,
         deck_size/1,
         draw_top_card/1,
         draw_N_cards/2,
         shuffle_deck/1,
         sort_deck/1
        ]).

return_deck(Deck) ->
    Deck.

deck_size(Deck) ->
    length(Deck).

draw_top_card([Firstcard | RestDeck]) ->
    {Firstcard, RestDeck}.

draw_N_cards(N, Deck) ->
    lists:split(N, Deck).

sort_deck(_Deck) ->
    ?DECK.

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
