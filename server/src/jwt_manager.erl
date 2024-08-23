-module(jwt_manager).

-export([generate_tokens/1, verify_refresh_token/1, verify_access_token/1,
         refresh_tokens/1]).

-define(ACCESS_TOKEN_SECRET, <<"ABXw3mqeOO40n/SzaU31lD2ENJePJ1Mr9ZQE1ZBBQAU=">>).
-define(REFRESH_TOKEN_SECRET,
        <<"7d9a3e394a52b064b064dd76924604b82a6b0b231c4b958006520b64420cfbfd">>).
-define(ACCESS_TOKEN_LIFETIME, 60 * 24 * 60 * 60).
-define(REFRESH_TOKEN_LIFETIME, 30 * 24 * 60 * 60).

generate_tokens(Id) ->
    CurrentTime =
        calendar:datetime_to_gregorian_seconds(
            calendar:universal_time())
        - calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    AccessTokenExp = CurrentTime + ?ACCESS_TOKEN_LIFETIME,
    RefreshTokenExp = CurrentTime + ?REFRESH_TOKEN_LIFETIME,
    AccessClaims = #{<<"sub">> => Id, <<"exp">> => AccessTokenExp},
    AccessToken = jwerl:sign(AccessClaims, hs256, ?ACCESS_TOKEN_SECRET),
    RefreshClaims = #{<<"sub">> => Id, <<"exp">> => RefreshTokenExp},

    RefreshToken = jwerl:sign(RefreshClaims, hs256, ?REFRESH_TOKEN_SECRET),
    {AccessToken, RefreshToken}.

verify_access_token(Token) ->
    jwerl:verify(Token, hs256, ?ACCESS_TOKEN_SECRET).

verify_refresh_token(Token) ->
    jwerl:verify(Token, hs256, ?REFRESH_TOKEN_SECRET).

refresh_tokens(RefreshToken) ->
    case verify_refresh_token(RefreshToken) of
        {ok, #{sub := Sub, exp := _}} ->
            {AccessToken, NewRefreshToken} = generate_tokens(Sub),
            {ok, AccessToken, NewRefreshToken};
        {error, Reason} ->
            {error, Reason}
    end.
