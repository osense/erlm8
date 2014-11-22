erlm8
=====

#####Why another IRC bot?
Well, apart from me wanting to learn Erlang, the idea is to have the bot running on my VPS 24/7,
connected to the Freenode IRC server(and possibly others later).
This would allow you to have a bot in your channel without having to set one up, just by typing
into your IRC client
```
/msg erlm8 join #mychan
```
As of now, the bot can't do all that much and even the internal API that plugins use
is not entirely stable.

#####Planned features
- magic 8 ball, fortunes, url title retrieval
- the concept of bot operators
- github repository monitoring
- translate messages
- leave messages for people for when they come online

Even this lists is largely a work in progress, feel free to suggest (or even implement!) more.

#####Running the bot yourself
Do
```
./rebar get-deps
./rebar compile
./start.sh
```

The API for you to use is in the erlm8 module.
```
erlm8:connect("chat.freenode.net")
```
