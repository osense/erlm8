-module(irc).

-export([parse/1, format/2]).

parse(List) ->
    case re:run(List, "^PING :(?<server>.*)", [{capture, [1], list}]) of
        {match, [Serv]} ->
            {ping, Serv};
        _ ->
            case re:run(List, "^:(?<source>.*)!.*PRIVMSG (?<chan>.*) :(?<text>.*)", [{capture, [1, 2, 3], list}]) of
                {match, [Source, Chan, Text]} ->
                    case re:run(Text, "^(?<target>.*): (?<text>.*)", [{capture, [1, 2], list}]) of
                        {match, [Target, T]} ->
                            {privmsg, {Chan, {Source, Target, T}}};
                        _ ->
                            {privmsg, {Chan, {Source, Text}}}
                    end;
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
format(privmsg, {Channel, {Target, Text}}) ->
    "PRIVMSG " ++ Channel ++ " :" ++ Target ++ ": " ++ Text;
format(privmsg, {Channel, Text}) ->
    "PRIVMSG " ++ Channel ++ " :" ++ Text;
format(action, {Channel, Text}) ->
    "PRIVMSG " ++ Channel ++ " :\x01ACTION " ++ Text ++ "\x01".
