-module(facein).
-export([person/1,start/1,add_friend/2,friends/1,broadcast/3,received_messages/1,shutdown/1,name/1]).

% Write report
% Refactor with rpc 

start(Name) ->
  Pid = spawn(facein,person,[{Name,[],[],[]}]),
  {ok,Pid}.

person({Name,FriendList,Inbox,Sent}) ->
  receive
    {From,{name}} ->
      From ! {self(),{ok,Name}},
      person({Name,FriendList,Inbox,Sent});
    {From,{received_messages}} ->
      From ! {self(),{ok,Inbox}},
      person({Name,FriendList,Inbox,Sent});
    {_   ,{receive_message,Ref,OriginName,Msg,Radius}} ->
      self() ! {self(),{broadcast,Ref,OriginName,Msg,Radius-1}},
      person({Name,FriendList,[{OriginName,Msg}|Inbox],Sent});
    {_   ,{broadcast,Ref,OriginName,Msg,Radius}} ->
      send_list(Ref,OriginName,FriendList,Msg,Radius,lists:member({Ref,Radius},Sent)),
      person({Name,FriendList,Inbox,[{Ref,Radius}|Sent]});
    {From,{shutdown}} ->
      From ! {self(),{ok}};
    {From,{friends}} ->
      From ! {self(),{ok,FriendList}},
      person({Name,FriendList,Inbox,Sent});
    {From,{add_friend,Fid}} ->
      FriendName = name(Fid),
      From ! {self(),{ok}},
      person({Name,[{FriendName,Fid} | FriendList],Inbox,Sent});
    _ ->
      io:format("Error")
  end.

add_friend(Pid,Fid) ->
  Pid ! {self(),{add_friend,Fid}},
  receive
    {Pid, {Status}} -> Status
  end.

%send to list of friends
send_list(_,_,_,_,_,true) -> % Don't send the same message twice
  ok;
send_list(_,_,[],_,_,_) -> % If there are no more receivers, stop sending
  ok;
send_list(_,_,_,_,0,_) -> % If the radius is zero, don't send anything
  ok;
send_list(Ref,OriginName,[{_,Fid}|Rest],Msg,Radius,false) ->
  send(Ref,OriginName,Fid,Msg,Radius),
  send_list(Ref,OriginName,Rest,Msg,Radius,false).

% send from Pid to Fid
send(Ref,OriginName,Fid,Msg,Radius) ->
  Fid ! {self(),{receive_message,Ref,OriginName,Msg,Radius}}.

received_messages(Pid) ->
  Pid ! {self(),{received_messages}},
  receive
    {Pid, {_,Msg}} -> Msg
  end.

broadcast(Pid,Msg,Radius) ->
  OriginName = name(Pid),
  Ref = make_ref(),
  Pid ! {self(),{broadcast,Ref,OriginName,Msg,Radius}}.

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
