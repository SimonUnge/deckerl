-module(dm_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("../include/deck.hrl").

-export([all/0, groups/0, init_per_group/2, end_per_group/2]).
-export([
         get_full_sorted_deck/1,
         get_shuffled_deck/1,
         get_deck_size/1,
         draw_one_card_have_51_in_deck/1,
         draw_10_cards_have_42_in_deck/1,
         sort_shuffled_deck/1
        ]).

all() ->
    [{group, dm_deck_utils}
    ].

groups() ->
    [
     {dm_deck_utils,
      [],
      [
       get_full_sorted_deck,
       get_shuffled_deck,
       get_deck_size,
       draw_one_card_have_51_in_deck,
       draw_10_cards_have_42_in_deck,
       sort_shuffled_deck,
      ]
     }
    ].

get_full_sorted_deck(_Config) ->
    Deck = ?DECK,
    Deck = deck_manager:show_deck().

get_shuffled_deck(_Config) ->
    Deck = ?DECK,
    deck_manager:shuffle_deck(),
    ShuffledDeck = deck_manager:show_deck(),
    ct:pal("Shuffled Deck ~p", [ShuffledDeck]),
    Deck =/= ShuffledDeck.

sort_shuffled_deck(_Config) ->
    Deck = ?DECK,
    deck_manager:shuffle_deck(),
    ShuffledDeck = deck_manager:show_deck(),
    Deck =/= ShuffledDeck,
    SortedDeck = deck_manager:sort_deck(),
    Deck =:= SortedDeck

get_deck_size(_Config) ->
    52 = deck_manager:deck_size().

draw_one_card_have_51_in_deck(_Config) ->
    deck_manager:shuffle_deck(),
    deck_manager:draw_top_card(),
    51 = deck_manager:deck_size().

draw_10_cards_have_42_in_deck(_Config) ->
    deck_manager:reset_deck(),
    deck_manager:draw_N_cards(10),
    42 = deck_manager:deck_size().

init_per_group(dm_deck_utils, Config) ->
    start_app_return_config(Config);
init_per_group(_, Config) ->
    Config.

end_per_group(dm_deck_utils, _Config) ->
    stop_app();
end_per_group(_, _Config) ->
    ok.

start_app_return_config(Config) ->
    ok = application:start(deckerl),
    Config.

stop_app() ->
    ok = application:stop(deckerl).
