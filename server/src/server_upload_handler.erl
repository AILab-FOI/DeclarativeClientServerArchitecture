-module(server_upload_handler).

-behaviour(cowboy_handler).

-export([init/2, allowed_methods/2, content_types_accepted/2, charsets_provided/2,
         is_authorized/2, from_multipart/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

is_authorized(Req, State) ->
    request:auth(Req, State).

content_types_accepted(Req, State) ->
    {[{{<<"multipart">>, <<"form-data">>, []}, from_multipart}], Req, State}.

charsets_provided(Req, State) ->
    {[<<"utf-8">>], Req, State}.

from_multipart(Req0, State) ->
    case cowboy_req:read_part(Req0) of
        {ok, Headers, Req1} ->
            case cow_multipart:form_data(Headers) of
                {data, _} ->
                    {ok, _, _} = cowboy_req:read_part_body(Req1);
                {file, FieldName, _, _} ->
                    {Req3, File} = stream_file(Req1, State, FieldName),
                    NewState = State ++ [File],
                    from_multipart(Req3, NewState)
            end;
        {done, Req} ->
            [{Extension, File}] = State,
            Res = handle(File, Extension),
            request:send_response(Req, Res, State)
    end.

handle(File, Extension) ->
    handle_data(Extension, File).

generate_paths(Extension) ->
    Id = integer_to_binary(erlang:unique_integer([positive])),
    Separator = <<".">>,
    Path = <<"priv/assets/">>,
    FileName = <<Id/binary, Separator/binary, Extension/binary>>,
    WritePath = <<Path/binary, FileName/binary>>,
    {WritePath, FileName}.

write_assets(WritePath, File) ->
    case file:write_file(WritePath, File) of
        ok ->
            ok;
        {error, _} ->
            error
    end.

handle_data(Extension, File) ->
    {WritePath, FileName} = generate_paths(Extension),
    write_assets(WritePath, File),
    FileName.

stream_file(Req0, State, FieldName) ->
    case cowboy_req:read_part_body(Req0) of
        {ok, _LastBodyChunk, Req} ->
            {Req, {FieldName, _LastBodyChunk}};
        {more, _BodyChunk, Req} ->
            stream_file(Req, State, FieldName)
    end.
