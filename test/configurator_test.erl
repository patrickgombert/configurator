-module(configurator_test).
-include_lib("eunit/include/eunit.hrl").

set_up() ->
  application:start(configurator).

tear_down() ->
  application:stop(configurator).

it_can_add_a_path_test() ->
  set_up(),
  ?assertEqual([], configurator:paths()),
  configurator:add_path("/some/path"),
  ?assertEqual(["/some/path"], configurator:paths()),
  tear_down().

it_returns_not_found_for_an_unfindable_config_test() ->
  set_up(),
  ?assertEqual(not_found, configurator:config("my.config")),
  tear_down().

it_finds_a_file_if_it_exists_test() ->
  set_up(),
  configurator:add_path("../test/fixture1/"),
  ?assertEqual([{key, value}], configurator:config("first_test.config")),
  tear_down().

it_finds_the_file_in_the_first_matching_path_test() ->
  set_up(),
  configurator:add_path("../test/fixture1/"),
  configurator:add_path("../test/fixture2/"),
  ?assertEqual([{key1, value1}], configurator:config("test.config")),
  tear_down().

it_finds_configs_in_a_later_path_test() ->
  set_up(),
  configurator:add_path("../test/fixture1/"),
  configurator:add_path("../test/fixture2/"),
  ?assertEqual([{other_key, other_val}], configurator:config("other_test.config")),
  tear_down().

it_recovers_from_bad_configuration_files_test() ->
  set_up(),
  configurator:add_path("../test/fixture1/"),
  ?assertEqual(bad_config, configurator:config("bad.config")),
  tear_down().

