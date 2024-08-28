-module(server_upload_handler).

-behaviour(cowboy_handler).

-export([init/2, allowed_methods/2, content_types_accepted/2, charsets_provided/2,
         is_authorized/2, from_json/2, from_multipart/2]).

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
                {data, FieldName} ->
                    {ok, Body, Req2} = cowboy_req:read_part_body(Req1),
                    get(FieldName, Body, State, Req2);
                {file, FieldName, _, _} ->
                    {Req3, File} = stream_file(Req1, State, FieldName),
                    NewState = State ++ [File],
                    from_multipart(Req3, NewState)
            end;
        {done, Req} ->
            [{Type, Object}, {Extension, File}] = State,
            handle(Type, Object, File, Extension, Req, State),

            {stop, Req, State}
    end.

handle(<<"korisnik">>, #{slika := Slika}, File, Extension, Req, State) ->
    case Slika =:= <<"21104.png">> of
        % file:write_file(, File),
        true ->
            io:format("TRUE");
        false ->
            io:format("FALSE")
    end,
    [];
handle(<<"fakultet">>,
       #{id := IdFakultet,
         naziv := Naziv,
         adresa := Adresa,
         logo := Logo},
       File,
       Extension,
       Req,
       State) ->
    case Logo =:= <<"21104.png">> of
        true ->
            % file:write_file(, File),
            io:format("~p~n", [integer_to_binary(erlang:unique_integer([positive]))]);
        false ->
            {WritePath, DeletePath, FileName} = generate_paths(Extension, Logo),
            delete_assets(DeletePath),
            Res = file:write_file(binary_to_list(WritePath), File),
            io:format("~p~n", [Res]),
            fakultet:uredi(IdFakultet, Naziv, fakultet:map_to_record_adresa(Adresa), FileName)
    end,
    [].

generate_paths(Extension, Logo) ->
    Id = integer_to_binary(erlang:unique_integer([positive])),
    Separator = <<".">>,
    Path = <<"priv/assets/">>,
    FileName = <<Id/binary, Separator/binary, Extension/binary>>,
    WritePath = <<Path/binary, FileName/binary>>,
    DeletePath = <<Path/binary, Logo/binary>>,
    {WritePath, DeletePath, FileName}.

delete_assets(DeletePath) ->
    case file:delete(DeletePath) of
        ok ->
            ok;
        {error, _} ->
            error
    end.

write_assets(WritePath, File) ->
    case file:write_file(WritePath, File) of
        ok ->
            ok;
        {error, _} ->
            error
    end.

handle_data() ->
    [].

get(<<"korisnik">> = Field, Id, State, Req) ->
    get_data(fun() -> korisnik:dohvati_korisnika(core, binary_to_integer(Id)) end,
             State,
             Field,
             Req);
get(<<"fakultet">> = Field, Id, State, Req) ->
    get_data(fun() -> fakultet:dohvati(core, binary_to_integer(Id)) end, State, Field, Req);
get(<<"kolegij">> = Field, Id, State, Req) ->
    get_data(fun() -> kolegij:dohvati(core, binary_to_integer(Id)) end, State, Field, Req);
get(<<"sadrzaj">> = Field, Id, State, Req) ->
    get_data(fun() -> sadrzaj:dohvati(core, binary_to_integer(Id)) end, State, Field, Req).

get_data(F, State, Field, Req) ->
    case F() of
        {atomic, Result} ->
            case Result of
                {error, Reason} ->
                    request:err(403, Reason, Req, State);
                _ ->
                    NewState = State ++ [{Field, Result}],
                    from_multipart(Req, NewState)
            end;
        {aborted, Reason} ->
            request:err(403, Reason, Req, State)
    end.

stream_file(Req0, State, FieldName) ->
    case cowboy_req:read_part_body(Req0) of
        {ok, _LastBodyChunk, Req} ->
            {Req, {FieldName, _LastBodyChunk}};
        {more, _BodyChunk, Req} ->
            stream_file(Req, State, FieldName)
    end.

from_json(Req, State) ->
    case cowboy_req:multipart(Req, #{}) of
        {ok, Parts, Req1} ->
            {FormData, Files} = extract_data(Parts),
            save_files(Files),
            process_form_data(FormData),
            {ok, Resp} =
                cowboy_req:reply(200,
                                 #{<<"content-type">> => <<"text/plain">>},
                                 <<"Upload successful!">>,
                                 Req1),
            {ok, Resp, State};
        {error, Reason} ->
            {ok, Resp} =
                cowboy_req:reply(400,
                                 #{<<"content-type">> => <<"text/plain">>},
                                 <<"Failed to upload: ">> ++ Reason,
                                 Req),
            {ok, Resp, State}
    end.

extract_data(Parts) ->
    lists:foldl(fun(Part, {FormDataAcc, FilesAcc}) ->
                   case Part of
                       {<<"file">>, #{filename := Filename}, FileData} ->
                           {[{Filename, FileData} | FilesAcc], FormDataAcc};
                       {Name, _, Value} -> {FilesAcc, [{Name, Value} | FormDataAcc]}
                   end
                end,
                {[], []},
                Parts).

save_files([]) ->
    ok;
save_files([{Filename, FileData} | Rest]) ->
    FilePath = "/path/to/store/files/" ++ Filename,
    file:write_file(FilePath, FileData),
    save_files(Rest).

process_form_data([]) ->
    ok;
process_form_data([{Name, Value} | Rest]) ->
    io:format("Received form data: ~p = ~p~n", [Name, Value]),
    process_form_data(Rest).

json_request(Req, State) ->
    case utils:gather_json(Req) of
        {error, Reason, _} ->
            request:err(400, Reason, Req, State);
        {ok, Map, Req2} ->
            run_put_request(Map, Req2, State)
    end.

run_put_request(#{<<"ime">> := Ime,
                  <<"prezime">> := Prezime,
                  <<"oib">> := Oib,
                  <<"lozinka">> := Lozinka,
                  <<"email">> := Email,
                  <<"opis">> := Opis,
                  <<"nadimak">> := Nadimak},
                Req,
                State) ->
    request:response(Req,
                     State,
                     fun() ->
                        korisnik:dodaj_studenta(Ime, Prezime, Oib, Lozinka, Email, Opis, Nadimak)
                     end);
run_put_request(_, Req, State) ->
    request:err(400, <<"Wrong keys">>, Req, State).
