erlm8
=====

Right now, the bot can:
- join and leave channels
- monitor a github repository

To get it up and running (no makefile for now):
```
./rebar get-deps
./rebar compile
./start.sh
```

The api for you to use is in the erlm8 module.
```
erlm8:connect("chat.freenode.net"),
```
When connected to a server, you can invite it into your channel through a PM on IRC.
Ideally, I should keep the bot running on freenode all the time, so you can just type:
```
/msg erlm8 join #mychan
```
The one who invites the bot to a channel automatically becomes it's OP.
Only the OP can tell the bot to leave the channel
```
erlm8: go away
```
