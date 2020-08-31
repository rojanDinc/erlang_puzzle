%%%-------------------------------------------------------------------
%% @doc matrix_puzzle public API
%% @end
%%%-------------------------------------------------------------------

-module(matrix_puzzle_app).

-behaviour(application).

-export([checksum/1,
         divisibles_checksum/1,
         start/2,
         stop/1]).

-import(utils, [max_min_difference/1, read_matrix_file/1]).

start(_StartType, _StartArgs) ->
    main(),
    matrix_puzzle_sup:start_link().

stop(_State) -> ok.

%% internal functions

checksum(Filename) ->
  case read_matrix_file(Filename) of
    {ok, Values} ->
      Sum = lists:foldl(fun (CurrentValue, Accumulator) ->
                          Accumulator + max_min_difference(CurrentValue)
                  end,
                  0,
                  Values),
      {ok, Sum};
    {error, Reason} ->
      {error, Reason}
  end.

find_divisibles_in_row ( List ) -> 
  lists:foldl(fun (CurrentValue, Accumulator) -> 
    Divisibles = lists:filter(fun (Value) ->
        (Value /= CurrentValue) and (CurrentValue rem Value == 0)
      end, List),
    SumOfDivisibles = lists:sum(lists:map(fun (Value) -> CurrentValue div Value end, Divisibles)),
    Accumulator + SumOfDivisibles
  end, 0, List).

divisibles_checksum ( Filename ) -> 
  case read_matrix_file(Filename) of
    {ok, Values} ->
      Sum = lists:foldl(fun (Row, Accumulator) -> 
          DivisiblesSumInRow = find_divisibles_in_row(Row),
          Accumulator + DivisiblesSumInRow
        end, 0, Values),
      {ok, Sum};
    {error, Reason} ->
      {error, Reason}
  end.

main() ->
  % run part1 and print result
  case checksum("values.txt") of
    {ok, Part1Sum} ->
      io:fwrite("~s~B~n", ["Ran part one, sum = ", Part1Sum]);
    {error, Part1Reason} ->
      io:fwrite("~s~s~n", ["Failed to run part1, reason: ", Part1Reason])
  end,

  % run part2 and print result
  case divisibles_checksum("values.txt") of
    {ok, Part2Sum} ->
      io:fwrite("~s~B~n", ["Ran part two, sum = ", Part2Sum]);
    {error, Part2Reason} ->
      io:fwrite("~s~s~n", ["Failed to run part2, reason: ", Part2Reason])
  end.