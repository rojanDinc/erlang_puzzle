-module(utils).

-export([read_matrix_file/1, max_min_difference/1]).

handle_file_open_error_cases(Reason) ->
  case Reason of
    enoent ->
      "File doesn't exist.";
    eacces ->
      "Missing permission for reading the file or"
      "searching one of the parent directories.";
    eisdir ->
      "The named file is a directory.";
    enotdir ->
      "A component of the filename is not a directory,"
      "or the filename itself is not a directory"
      "if the directory mode was specified.";
    enospc ->
      "There is no space left on the device."
  end.

read_file_values(IoDevice, PrevLines) ->
    case file:read_line(IoDevice) of
        {ok, LineBinary} ->
            Line = lists:map(fun (Item) ->
                                     list_to_integer(string:trim(Item))
                             end,
                             string:tokens(LineBinary, "\t")),
            read_file_values(IoDevice, PrevLines ++ [Line]);
        eof -> PrevLines
    end.

read_matrix_file(Filename) ->
    case file:open(Filename, [read]) of
      {ok, IoDevice} ->
        Values = read_file_values(IoDevice, []),
        file:close(IoDevice),
        {ok, Values};
      {error, Reason} ->
        ReasonMessage = handle_file_open_error_cases(Reason),
        io:fwrite("Could not read file, reason: ~s~n", [ReasonMessage]),
        {error, Reason}
    end.

max_min_difference(List) when length(List) =:= 0 -> 0;
max_min_difference(List) when length(List) > 0 ->
    Max = lists:max(List),
    Min = lists:min(List),
    Max - Min.
