% This file is part of erlm8 released under the MIT license.
% See the LICENSE file for more information.

-module(irc).

-export([parse/1, format/2]).

parse(Binary) ->
    case re:run(Binary, "^PING :(?<server>.*)", [{capture, [1], binary}]) of
        {match, [Serv]} ->
            {ping, Serv};
        _ ->
            case re:run(Binary, "^:(?<source>.*)!.*PRIVMSG (?<chan>.*) :(?<text>.*)", [{capture, [1, 2, 3], binary}]) of
                {match, [Source, Chan, Text]} ->
                    case re:run(Text, "^(?<target>.*): (?<text>.*)", [{capture, [1, 2], binary}]) of
                        {match, [Target, T]} ->
                            {privmsg, {Chan, {Source, Target, T}}};
                        _ ->
                            {privmsg, {Chan, {Source, Text}}}
                    end;
                _ ->
                    {raw, Binary}
            end
    end.

format(ident, {Nick, Realname}) ->
    <<"USER ", Nick/binary, " <hostname> <servername> :", Realname/binary>>;
format(nick, Nick) ->
    <<"NICK ", Nick/binary>>;
format(ping, Server) ->
    <<"PONG :", Server/binary>>;
format(join, Chan) ->
    <<"JOIN :", Chan/binary>>;
format(part, Chan) ->
    <<"PART :", Chan/binary>>;
format(privmsg, {Channel, {Target, Text}}) ->
    <<"PRIVMSG ", Channel/binary, " :", Target/binary, ": ", Text/binary>>;
format(privmsg, {Channel, Text}) ->
    <<"PRIVMSG ", Channel/binary, " :", Text/binary>>;
format(action, {Channel, Text}) ->
    <<"PRIVMSG ", Channel/binary, " :\x01ACTION ", Text/binary, "\x01">>.
