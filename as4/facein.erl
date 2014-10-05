-module(facein).
-export([person/1,start/1,add_friend/2,friends/1,broadcast/3,received_messages/1,shutdown/1,name/1]).

% Write report
% Refactor with rpc 

start(Name) ->
  Pid = spawn(facein,person,[{Name,[],[]}]),
  {ok,Pid}.

person({Name,FriendList,Inbox}) ->
  receive
    {From,{name}} ->
      From ! {self(),{ok,Name}},
      person({Name,FriendList,Inbox});
    {From,{received_messages}} ->
      From ! {self(),{ok,Inbox}},
      person({Name,FriendList,Inbox});
    {_   ,{receive_message,OriginName,Msg,Radius}} ->
      io:format("radius: ~s!~n",[Name]),
      send_list(OriginName,FriendList,Msg,Radius-1),
      person({Name,FriendList,[{OriginName,Msg}|Inbox]});
    {From,{shutdown}} ->
      From ! {self(),{ok}};
    {From,{friends}} ->
      From ! {self(),{ok,FriendList}},
      person({Name,FriendList,Inbox});
    {From,{add_friend,Fid}} ->
      FriendName = name(Fid),
      From ! {self(),{ok}},
      person({Name,[{FriendName,Fid} | FriendList],Inbox});
    _ ->
      io:format("Error")
  end.

add_friend(Pid,Fid) ->
  Pid ! {self(),{add_friend,Fid}},
  receive
    {Pid, {Status}} -> Status
  end.

%send to list of friends
send_list(_,[],_,_) ->
  ok;
send_list(_,_,_,0) ->
  ok;
send_list(OriginName,[{_,Fid}|Rest],Msg,Radius) ->
  send(OriginName,Fid,Msg,Radius),
  send_list(OriginName,Rest,Msg,Radius).

% send from Pid to Fid
send(OriginName,Fid,Msg,Radius) ->
  Fid ! {self(),{receive_message,OriginName,Msg,Radius}}.

received_messages(Pid) ->
  Pid ! {self(),{received_messages}},
  receive
    {Pid, {_,Msg}} -> Msg
  end.

broadcast(Pid,Msg,Radius) ->
  FriendList = friends(Pid),
  Name = name(Pid),
  send_list(Name,FriendList,Msg,Radius).

name(Pid) ->
  Pid ! {self(), {name}},
  receive
    {Pid, {_,Msg}} -> Msg
  end.

friends(Pid) ->
  Pid ! {self(), {friends}},
  receive
    {Pid, {_,Msg}} -> Msg
  end.

shutdown(Pid) ->
  Pid ! {self(), {shutdown}},
  receive
    {Pid, {Status}} -> Status
  end.
