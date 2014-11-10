-module(irc).

-export([parse/1, format/2]).

parse(List) ->
    case re:run(List, "^PING :(?<server>.*)", [{capture, [1], list}]) of
        {match, [Serv]} ->
            {ping, Serv};
        _ ->
            case re:run(List, "^PRIVMSG :(?<chan>.*) :(?<text>.*)", [{capture, [1, 2], list}]) of
                {match, [Chan, Text]} ->
                    {privmsg, {Chan, Text}};
                _ ->
                    {raw, List}
            end
    end.

format(ident, {Nick, Realname}) ->
    "USER " ++ Nick ++ " <hostname> <servername> :" ++ Realname;
format(nick, Nick) ->
    "NICK " ++ Nick;
format(ping, Server) ->
    "PONG :" ++ Server;
format(join, Chan) ->
    "JOIN :" ++ Chan;
format(part, Chan) ->
    "PART :" ++ Chan;
format(privmsg, {Channel, Text}) ->
    "PRIVMSG :" ++ Channel ++ " :" ++ Text;
format(memsg, {Channel, Text}) ->
    "PRIVMSG :" ++ Channel ++ " :\x01ACTION " ++ Text ++ "\x01".
