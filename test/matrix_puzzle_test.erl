-module(matrix_puzzle_test).

-include_lib("eunit/include/eunit.hrl").

%%%
%%% Tests for part 1
%%%

read_matrix_file_with_small_values_test() ->
    ?assertMatch({ok, _}, utils:read_matrix_file("values_min.txt")).

read_matrix_file_with_large_values_test() ->
    % Arrange
    ExpectedRowsLength = 16,
    % Act
    Rows = utils:read_matrix_file("values.txt"),
    % Assert
    ?assertMatch({ok, _}, Rows),
    ?assertEqual(ExpectedRowsLength, length(element(2, Rows))).

read_matrix_file_that_doesnt_exist_test() ->
    % Arrange
    % Act
    % Assert
    ?assertMatch({error, _}, utils:read_matrix_file("sample.txt")).

checksum_with_small_sample_test() ->
    % Arrange
    ExpectedSum = 20,
    % Act
    ActualResult = matrix_puzzle_app:checksum("values_min.txt"),
    % Assert
    ?assertMatch({ok, _}, ActualResult),
    ?assertEqual(ExpectedSum, element(2, ActualResult)).

checksum_with_large_sample_test() ->
    % Arrange
    ExpectedSum = 54426,
    % Act
    ActualResult = matrix_puzzle_app:checksum("values.txt"),
    % Assert
    ?assertMatch({ok, _}, ActualResult),
    ?assertEqual(ExpectedSum, element(2, ActualResult)).

checksum_with_large_sample_test_should_fail_test() ->
    % Arrange
    ExpectedSum = 999,
    % Act
    ActualResult = matrix_puzzle_app:checksum("values.txt"),
    % Assert
    ?assertMatch({ok, _}, ActualResult),
    ?assertNotEqual(ExpectedSum, element(2, ActualResult)).

%%%
%%% Tests for part 2
%%%

divisible_checksum_with_small_sample_test() ->
  % Arrange
  ExpectedSum = 9,
  % Act
  ActualResult = matrix_puzzle_app:divisibles_checksum("values2_min.txt"),
  % Assert
  ?assertMatch({ok, _}, ActualResult),
  ?assertEqual(ExpectedSum, element(2, ActualResult)).

divisible_checksum_with_large_sample_test() ->
  % Arrange
  ExpectedSum = 333,
  % Act
  ActualResult = matrix_puzzle_app:divisibles_checksum("values.txt"),
  % Assert
  ?assertMatch({ok, _}, ActualResult),
  ?assertEqual(ExpectedSum, element(2, ActualResult)).