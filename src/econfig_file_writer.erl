-module(econfig_file_writer).

-export([save_to_file/2]).

%% @spec save_to_file(
%%           Config::{{Section::string(), Option::string()}, Value::string()},
%%           File::filename()) -> ok
%% @doc Saves a Section/Key/Value triple to the ini file File::filename()
save_to_file({{Section, Key}, Value}, File) ->
    {ok, OldFileContents} = file:read_file(File),
    Lines = re:split(OldFileContents, "\r\n|\n|\r|\032", [{return, list}]),

    SectionLine = "[" ++ Section ++ "]",
    {ok, Pattern} = re:compile(["^(", Key, "\\s*=)|\\[[a-zA-Z0-9\_-]*\\]"]),

    NewLines = process_file_lines(Lines, [], SectionLine, Pattern, Key, Value),
    NewFileContents = reverse_and_add_newline(strip_empty_lines(NewLines), []),
    case file:write_file(File, NewFileContents) of
    ok ->
        ok;
    {error, eacces} ->
        {file_permission_error, File};
    Error ->
        Error
    end.


process_file_lines([Section|Rest], SeenLines, Section, Pattern, Key, Value) ->
    process_section_lines(Rest, [Section|SeenLines], Pattern, Key, Value);

process_file_lines([Line|Rest], SeenLines, Section, Pattern, Key, Value) ->
    process_file_lines(Rest, [Line|SeenLines], Section, Pattern, Key, Value);

process_file_lines([], SeenLines, Section, _Pattern, Key, Value) ->
    % Section wasn't found.  Append it with the option here.
    [Key ++ " = " ++ Value, Section, "" | strip_empty_lines(SeenLines)].


process_section_lines([Line|Rest], SeenLines, Pattern, Key, Value) ->
    case re:run(Line, Pattern, [{capture, all_but_first}]) of
    nomatch -> % Found nothing interesting. Move on.
        process_section_lines(Rest, [Line|SeenLines], Pattern, Key, Value);
    {match, []} -> % Found another section. Append the option here.
        lists:reverse(Rest) ++
        [Line, "", Key ++ " = " ++ Value | strip_empty_lines(SeenLines)];
    {match, _} -> % Found the option itself. Replace it.
        lists:reverse(Rest) ++ [Key ++ " = " ++ Value | SeenLines]
    end;

process_section_lines([], SeenLines, _Pattern, Key, Value) ->
    % Found end of file within the section. Append the option here.
    [Key ++ " = " ++ Value | strip_empty_lines(SeenLines)].


reverse_and_add_newline([Line|Rest], Content) ->
    reverse_and_add_newline(Rest, [Line, "\n", Content]);

reverse_and_add_newline([], Content) ->
    Content.


strip_empty_lines(["" | Rest]) ->
    strip_empty_lines(Rest);

strip_empty_lines(All) ->
    All.
