-module(deck_util_tests).

-include("include/deck.hrl").
-include_lib("eunit/include/eunit.hrl").

deck_test_() ->
    Deck = ?DECK,
    [return_new_deck(Deck),
     return_full_deck_size(Deck),
     draw_first_card(Deck),
     draw_N_cards(Deck),
     shuffle_deck(Deck),
     sort_deck(Deck)
    ].

return_new_deck(Deck) ->
    fun() ->
        ?assertEqual(Deck, deck_util:new_deck())
    end.

return_full_deck_size(Deck) ->
    fun() ->
        ?assertEqual(52, deck_util:deck_size(Deck))
    end.

draw_first_card(Deck) ->
    fun() ->
        [_ | NewDeck] = Deck,
        ?assertMatch({{hearts,2},NewDeck}, deck_util:draw_top_card(Deck))
    end.

draw_N_cards(Deck) ->
    fun() ->
        {DrawnCards, RestDeck} = deck_util:draw_N_cards(5,Deck),
        ?assertEqual({5, 47},{length(DrawnCards),length(RestDeck)})
    end.

shuffle_deck(Deck) ->
    fun() ->
       ?assertNot(?DECK =:= deck_util:shuffle_deck(Deck))
    end.

sort_deck(Deck) ->
    fun() ->
       ShuffledDeck = deck_util:shuffle_deck(Deck),
       ?assertEqual(?DECK, deck_util:sort_deck(ShuffledDeck))
    end.
