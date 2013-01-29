%% Author: 
%% Created: Aug 15, 2011
-module(station).
-compile(export_all).

-define (HELLO_INTERVAL, 1000).
-define (MASTER, 'sasha1@132.72.253.29').
-define (SLAVE1, 'sasha2@132.72.253.29').
-define (SLAVE2, 'adir1@132.72.248.206').
-define (SLAVE3, 'adir2@132.72.248.206').
%%-define (SLAVE2, 'kobi1@132.72.253.242').
%%-define (SLAVE3, 'kobi2@132.72.253.242').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  user function: sets the timer to HELLO_INTERVAL 					      %%
%%  and enters the loop	where the station node can receive/send messages, 		      %%
%%  update it's tree and neighbors.							      %%
%%  gets: -initial info: Length,Speed, Angle, X, Y                                            %%
%%        -Manager             						                      %%
%%	  -Neighbors_list						                      %%    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

user(Mater_node, Station_num, Paint_station, Length, Speed, Angle, X, Y, Manager, Neighbors_list) ->
    timer:send_interval(?HELLO_INTERVAL,{interval}), 
    loop(Mater_node, Station_num, Paint_station, Length, Speed, Angle, X, Y, Manager, Neighbors_list, [], [], [], 1, false, 0).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  loop function:									               %%
%%      This function is the main loop which run all stations.                                         %%
%%      This function is built in a way of receiving a message and responding.                         %%
%%      the message taht he may to recive is:                                                          %%
%%             	{neighbors, Neighbors_within_transmission_list}                        	               %%
%%             	{interval                        				                       %%
%%          	{hello, NeighborPid}              				                       %%
%%             	{update_tree, SourcePid, UpdatedPid, NewNumOfHops}      	       	               %%
%%             	{forward_update_tree, SourcePid, DestinationPid, UpdatedPid, NewNumOfHops, MessageSN}  %%
%%       	{disconnect,Pid}                                                                       %%
%%      	{stop}                       				                               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

loop(Mater_node, Station_num, Paint_station, Length, Speed, Angle, X, Y, Manager, Neighbors_list, Tree_list, Childrens, Archive , SN, PainTree, TGPid) ->
    receive
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%   		this message tell my who is my neighbores		                      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	{neighbors, Neighbors_within_transmission_list} -> 
	    lists:foreach(fun(Pid) -> Pid ! {hello, self()} end, Neighbors_within_transmission_list),
	    loop(Mater_node, Station_num, Paint_station, Length, Speed, Angle, X, Y, Manager, Neighbors_list, Tree_list, Childrens, Archive, SN, PainTree, TGPid);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%      	this message tell my that interval time passed		                      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
	{interval} ->
	    Temp_neighbors_list = lists:filter(fun({_Pid, X}) -> X =:= true end, Neighbors_list),		
	    Disconected_neighbors_list = lists:filter(fun({_Pid, X}) -> X =:= false end, Neighbors_list),
	    New_neighbors_list = [{Pid, false} || {Pid, Y} <- Temp_neighbors_list, Y =:= true],
	    if
		(Disconected_neighbors_list =/= []) ->
		    New_archive = update_archive(Disconected_neighbors_list, Archive, Archive),
		    {Tree_after_changes, New_path_pids} = rearrange_tree_list(Disconected_neighbors_list, Tree_list, 1, Disconected_neighbors_list),
		    if
			(Childrens =/= []) ->
			    Temp_children = send_disconnect_to_children(New_path_pids, Childrens, Childrens),
			    Updated_children_list = clear_disconnected_pids_from_children(New_path_pids, Temp_children, Temp_children);
			true ->
			    Updated_children_list = []
		    end;
		true ->
		    New_archive = Archive,
		    Updated_children_list = Childrens,
		    Tree_after_changes = Tree_list
	    end,
	    if
		((Angle > 0) and (Angle =< 90)) or((Angle > 270) and (Angle =< 360)) ->
		    X2 = X + abs(?HELLO_INTERVAL*Speed*1000/3600*(math:sin(Angle*(math:pi()/180)))/1000); 
		true ->
		    X2 = X - abs(?HELLO_INTERVAL*Speed*1000/3600*(math:sin(Angle*(math:pi()/180)))/1000) 
	    end,
	    if
		((Angle > 0) and (Angle =< 180)) ->
		    Y2 = Y - abs(?HELLO_INTERVAL*Speed*1000/3600*(math:cos(Angle*(math:pi()/180)))/1000);
		true ->
		    Y2 = Y + abs(?HELLO_INTERVAL*Speed*1000/3600*(math:cos(Angle*(math:pi()/180)))/1000)
	    end,
	    Connected_nodes = nodes(connected),
	    case node() of
		?MASTER-> 
		    Temp1 = lists:member(?SLAVE1 ,Connected_nodes),
		    if(Temp1)->
			    XLength = 2*Length;
		      true->
			    XLength = Length
		    end,
		    Temp2 = lists:member(?SLAVE2 ,Connected_nodes),
		    if(Temp2)->
			    YLength = 2*Length;
		      true->
			    YLength = Length
		    end;
		?SLAVE1->
		    XLength = 2*Length,
		    Temp2 = lists:member(?SLAVE2 ,Connected_nodes),
		    if(Temp2)->
			    YLength = 2*Length;
		      true->
			    YLength = Length
		    end;
		?SLAVE2->
		    XLength = 2*Length,
		    YLength = 2*Length;
		?SLAVE3->
		    XLength = 2*Length,
		    YLength = 2*Length
		end,
		    
	    {NewAngle, X3, Y3} = border_test(Station_num, XLength, YLength, Angle, X2, Y2, Manager),	    
	    if
		(Station_num == Paint_station) and (PainTree == true) ->
		    TGPid ! {pain_tree, self(), Tree_after_changes,Paint_station};
		true ->
		    ok
	    end,
	    loop(Mater_node, Station_num, Paint_station, Length, Speed, NewAngle, X3, Y3, Manager, New_neighbors_list, Tree_after_changes, Updated_children_list, New_archive, SN, PainTree, TGPid);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%      	this message tell my that this pid is close to my	                      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
	{hello, NeighborPid} -> 
	    case lists:keysearch(NeighborPid,1,Neighbors_list) of
		false ->  
		    NewNeighbors_list = [{NeighborPid, true}|Neighbors_list];
		{value, _Tuple2} ->
		    NewNeighbors_list = lists:keyreplace(NeighborPid, 1, Neighbors_list, {NeighborPid, true})
	    end,
	    case lists:keysearch(NeighborPid, 1, Tree_list) of
		false -> 
		    New_Tree_list = [{NeighborPid, 1, NeighborPid, {NeighborPid, SN}}|Tree_list],  
		    NeighborPid ! {father, self(), NeighborPid},
		    send_update_to_all_tree(New_Tree_list, NeighborPid, 1, SN),
		    loop(Mater_node, Station_num, Paint_station, Length, Speed, Angle, X, Y, Manager, NewNeighbors_list, New_Tree_list, Childrens, Archive, SN+1, PainTree,TGPid);
		{value, {NeighborPid, NumOfHops, NextHopPid, {Update_pid, _OldSN}}} ->
		    if 
			(NumOfHops > 1) ->
			    New_Tree_list = lists:keyreplace(NeighborPid, 1, Tree_list, {NeighborPid, 1, NeighborPid, {Update_pid, SN}}),
			    NextHopPid ! {cancel_father, self(), NeighborPid},
			    NeighborPid ! {father, self(), NeighborPid},
			    send_update_to_all_tree(New_Tree_list, NeighborPid, 1, SN),				
			    loop(Mater_node, Station_num, Paint_station, Length, Speed, Angle, X, Y, Manager, NewNeighbors_list, New_Tree_list, Childrens, Archive, SN+1, PainTree,TGPid);
			true ->
			    loop(Mater_node, Station_num, Paint_station, Length, Speed, Angle, X, Y, Manager, NewNeighbors_list, Tree_list, Childrens, Archive, SN, PainTree,TGPid)
		    end
	    end;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%      	This message is telling someone that he is my father	                      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
	{father, SourcePid, DestinationPid} ->
	    if 
		(DestinationPid == self) ->
		    ok;
	    true ->
		    case find_list(DestinationPid, Childrens) of
			{not_found} ->
			    NewChildrens = [[{DestinationPid},[SourcePid]]|Childrens],
			    loop(Mater_node, Station_num, Paint_station, Length, Speed, Angle, X, Y, Manager, Neighbors_list, Tree_list, NewChildrens, Archive , SN, PainTree,TGPid);
			{{Pid},Childrens_list} ->
			    NewChildrens = [[{Pid},[SourcePid|Childrens_list]] | Childrens--[[{Pid},Childrens_list]]],
			    loop(Mater_node, Station_num, Paint_station, Length, Speed, Angle, X, Y, Manager, Neighbors_list, Tree_list, NewChildrens, Archive , SN, PainTree,TGPid)
		    end
	    end;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%      	This message is telling someone that he is no longer my father   	      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
	{cancel_father, SourcePid, DestinationPid} ->
	    case find_list(DestinationPid, Childrens) of
		{not_found} ->
		    loop(Mater_node, Station_num, Paint_station, Length, Speed, Angle, X, Y, Manager, Neighbors_list, Tree_list, Childrens, Archive , SN, PainTree,TGPid);
		{{Pid},Childrens_list} ->
		    New_Childrens_list = Childrens_list--[SourcePid],
		    if
			(New_Childrens_list == []) ->
			    NewChildrens = Childrens--[[{Pid},Childrens_list]];
			true ->
			    NewChildrens = [[{Pid}, New_Childrens_list] | Childrens--[[{Pid},Childrens_list]]]
		    end,
		    loop(Mater_node, Station_num, Paint_station, Length, Speed, Angle, X, Y, Manager, Neighbors_list, Tree_list, NewChildrens, Archive , SN, PainTree,TGPid)
	    end;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%      	this message tell my to do update and chenge the path to this pid (UpdatePid) %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
	{update_tree, SourcePid, UpdatedPid, NewNumOfHops, NextHopPid, {Update_pid, MessageSN}} ->
	    if
		(UpdatedPid =/= self())->
		    case lists:keysearch(UpdatedPid, 1, Tree_list) of
			false -> 
			    if
				(SourcePid == Update_pid)->
				    NewArchive = update_tree_in_Archive(SourcePid, UpdatedPid, NewNumOfHops, NextHopPid, MessageSN, Archive),
				    New_Tree_list = [{UpdatedPid, NewNumOfHops, SourcePid, {Update_pid, MessageSN}}|Tree_list];
				true->
				    case lists:keysearch(SourcePid, 1, Tree_list) of
					false ->
					    NewArchive = Archive,
					    New_Tree_list = Tree_list;
					{value, {_S_NeighborPid, _S_NumOfHops, _S_NextHopPid, {_S_Update_pid, _S_SN}}} ->
					    NewArchive = update_tree_in_Archive(SourcePid, UpdatedPid, NewNumOfHops, NextHopPid, MessageSN, Archive),
					    New_Tree_list = [{UpdatedPid, NewNumOfHops, SourcePid, {Update_pid, MessageSN}}|Tree_list],
					    SourcePid ! {father, self(), UpdatedPid}
				    end	
			    end,
			    loop(Mater_node, Station_num, Paint_station, Length, Speed, Angle, X, Y, Manager, Neighbors_list, New_Tree_list, Childrens, NewArchive, SN+1, PainTree,TGPid);
			
			{value, {_S_NeighborPid, S_NumOfHops, S_NextHopPid, {S_Update_pid, S_SN}}} ->
			    if
				(S_Update_pid == Update_pid) and (S_SN > MessageSN) ->
				    NewArchive = Archive,
				    New_Tree_list = Tree_list;
				true->
				    case lists:keysearch(SourcePid, 1, Tree_list) of
					false ->
					    NewArchive = Archive,
					    New_Tree_list = Tree_list;
					{value, {_S1_NeighborPid, _S1_NumOfHops, _S1_NextHopPid, {_S1_Update_pid, _S1_SN}}} ->
					    if (S_NumOfHops > NewNumOfHops)->
						    NewArchive = update_tree_in_Archive(SourcePid, UpdatedPid, NewNumOfHops, NextHopPid, MessageSN, Archive),
						    New_Tree_list = lists:keyreplace(UpdatedPid, 1, Tree_list, {UpdatedPid, NewNumOfHops, SourcePid, {Update_pid, MessageSN}}),
						    S_NextHopPid ! {cancel_father, self(), UpdatedPid},
						    SourcePid ! {father, self(), UpdatedPid},
						    send_update_to_all_tree(New_Tree_list, UpdatedPid, NewNumOfHops, SN);
					       true ->
						    NewArchive = Archive,
						    New_Tree_list = Tree_list
					    end
				    end				   
			    end,	
			    loop(Mater_node, Station_num, Paint_station, Length, Speed, Angle, X, Y, Manager, Neighbors_list, New_Tree_list, Childrens, NewArchive, SN+1, PainTree,TGPid)  
		    end;
		true ->
		    loop(Mater_node, Station_num, Paint_station, Length, Speed, Angle, X, Y, Manager, Neighbors_list, Tree_list, Childrens, Archive, SN, PainTree,TGPid)  
	    end;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%      	this message tell my to forward the update on my tree 	                      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
	{forward_update_tree, _SourcePid, DestinationPid, UpdatedPid, NewNumOfHops, {Update_pid, MessageSN}} ->
	    case lists:keysearch(DestinationPid, 1, Tree_list) of
		false -> 
		    %%io:format("~nforward_update_tree: ~p~nDestinationPid: ~p~nTree_list: ~p~n~n",[self(),DestinationPid,Tree_list]),	
		    ok;
		{value, {NeighborPid, NumOfHops, NextHopPid, {_Update_pid, _SN}}} ->
		    if
			(NumOfHops == 1) ->
			    NeighborPid ! {update_tree, self(), UpdatedPid, NewNumOfHops+1, {Update_pid, MessageSN}};
			true ->	
                            NextHopPid ! {forward_update_tree, self(), DestinationPid, UpdatedPid, NewNumOfHops+1, {Update_pid, MessageSN}}    
		    end
	    end,
	    loop(Mater_node, Station_num, Paint_station, Length, Speed, Angle, X, Y, Manager, Neighbors_list, Tree_list, Childrens, Archive, SN, PainTree,TGPid);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%   is function gives instruction graphical interface to draw the tree                       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	{start_pain_your_tree, NewTGPid} ->
	    loop(Mater_node, Station_num, Paint_station, Length, Speed, Angle, X, Y, Manager, Neighbors_list, Tree_list, Childrens, Archive, SN, true, NewTGPid);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  This function closes the plug-in graphic that shows the tree.                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

{stop_pain_your_tree} ->
	    loop(Mater_node, Station_num, Paint_station, Length, Speed, Angle, X, Y, Manager, Neighbors_list, Tree_list, Childrens, Archive, SN, false,TGPid);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%      	this message tell my to kill the station.                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	
	{stop} -> exit()
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  send_update_to_all_tree function: each node sends updates to all the nodes in it's tree   %%
%%  gets: -a list of tupples [{NeighborPid, NumOfHops, NextHopPid}|T] to update		      %%
%%  	  -and the needed updates: UpdatedPid, NewNumOfHops.  				      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send_update_to_all_tree([], _UpdatedPid, _NewNumOfHops, _NewSN) ->
    ok;
send_update_to_all_tree([{Pid_in_tree, NumOfHops, NextHopPid, {_Update_pid, _SN}}|T], UpdatedPid, NewNumOfHops, NewSN) ->
    if 
	(NumOfHops == 1) ->
	    if
		(Pid_in_tree == UpdatedPid) ->
		    Pid_in_tree ! {update_tree, self(), UpdatedPid, NewNumOfHops, NextHopPid, {self(), NewSN}};
		true ->
		    Pid_in_tree ! {update_tree, self(), UpdatedPid, NewNumOfHops+1, self(), {self(), NewSN}}
	    end;
	true ->
		    NextHopPid ! {forward_update_tree, self(), Pid_in_tree, UpdatedPid, NewNumOfHops+1, {self(), NewSN}}
    end,
    send_update_to_all_tree(T, UpdatedPid, NewNumOfHops, NewSN).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%     find_list function: This function searches the list of lists.		              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%      
find_list(_WantedPid, [])->
    {not_found};
find_list(WantedPid, [[{Pid},Childrens_list]|T]) ->
    if 
	(WantedPid == Pid) ->
	    {{Pid},Childrens_list};
	true ->
	    find_list(WantedPid, T)
    end.    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% update_tree_in_Archive function:                                                           %%
%%                       this function responsible enter to archive all the offer             %%
%%                       that i have from my neighbors				              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_tree_in_Archive(SourcePid, UpdatedPid, NewNumOfHops, NextHopPid, NewSN, Archive) ->
    case find_tree_in_Archive(SourcePid, Archive) of
	{not_found} ->
	    NewArchive = [[{SourcePid},[{UpdatedPid, NewNumOfHops, NextHopPid, NewSN}]] | Archive],
	    NewArchive;
	{{Pid}, [Tree]} -> 
	    case lists:keysearch(UpdatedPid, 1, Tree) of
		false -> 
		    NewArchive = [[{Pid},[{UpdatedPid, NewNumOfHops, NextHopPid, NewSN}|Tree]] | Archive--[[{Pid},Tree]]],
		    NewArchive;
		{value, {S_Pid, _S_NumOfHops, _S_NextHopPid, S_SN}} ->
		    if 
			(NewSN > S_SN) ->
			    NewSourcePidTree = lists:keyreplace(UpdatedPid, 1, [Tree], {S_Pid, NewNumOfHops, NextHopPid, NewSN}),		    
			    NewArchive = [[{Pid}, [NewSourcePidTree]] | Archive--[{Pid}, Tree]],
			    NewArchive;
			true ->
			    Archive 
		    end    
	    end
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% find_tree_in_Archive function: this funcation responsible 				      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

find_tree_in_Archive(_SourcePid, []) ->
    {not_found};
find_tree_in_Archive(SourcePid, [[{Pid}, Tree]|T]) ->
    if 
	(SourcePid == Pid) ->
	    {{Pid}, [Tree]};
	true ->
	    find_tree_in_Archive(SourcePid, T)
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  border test function: we move the nodes according to Snell's law.                         %%
%%  gets: -Length, Angle, (X,Y)- from where the node starts to move			      %%
%%        -Manger that updates the node's information			                      %%
%%  this function checks if the node is a about to exit the specified area (Length*Length)    %% 
%%  and directs it according to Snell's law		                                      %%
%%                                                                        		      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

border_test(Station_num, XLength, YLength, Angle, X, Y, Manager)->
    if 
	(Y >= YLength) -> 
	    Y2 = YLength - (Y - YLength),
	    if 
		(Angle < 270) ->
		    NewAngle = 360 - Angle,
		    border_test(Station_num, XLength, YLength, NewAngle, X, Y2, Manager);
		(Angle >= 270) ->
		    NewAngle = 360 - Angle,
		    border_test(Station_num, XLength, YLength, NewAngle, X, Y2, Manager);
		true ->
		    border_test(Station_num, XLength, YLength, Angle, X, Y2, Manager)
	    end;
	
	true -> 
	    if 
		(X >= XLength) ->
		    X2 = XLength - (X - XLength),
		    if 
			(Angle =< 90) ->
			    NewAngle = 180 - Angle,
			    border_test(Station_num, XLength, YLength, NewAngle, X2, Y, Manager);
			(Angle >= 270) ->
			    NewAngle = 360 + 180 - Angle,
			    border_test(Station_num, XLength, YLength, NewAngle, X2, Y, Manager);
			true ->
			    border_test(Station_num, XLength, YLength, Angle, X2, Y, Manager)
		    end;
		true -> 
		    if 
			(Y < 0) ->
			    Y3 = (-Y),
			    if 
				(Angle >= 90) ->
				    NewAngle = 360 - Angle,
				    border_test(Station_num, XLength, YLength, NewAngle, X, Y3, Manager);
				(Angle < 90) ->
				    NewAngle = 360 - Angle,
				    border_test(Station_num, XLength, YLength, NewAngle, X, Y3, Manager);
				true ->
				    border_test(Station_num, XLength, YLength, Angle, X, Y3, Manager)
			    end;
			true -> 
			    if 
				(X < 0) -> 
				    X3 = (-X),
				    if 
					(Angle >= 180) ->
					    NewAngle = 360 + 180- Angle,
					    border_test(Station_num, XLength, YLength, NewAngle, X3, Y, Manager);
					(Angle < 180) ->
					    NewAngle = 180 - Angle,
					    border_test(Station_num, XLength, YLength, NewAngle, X3, Y, Manager);
					true ->
					    border_test(Station_num, XLength, YLength, Angle, X3, Y, Manager)
				    end;
				true -> 
				    %%io:format("station_border_test:before send to creator ~p connected: ~p manager ~p~n",[self(), nodes(connected), Manager]),
				    Manager ! {update, node(), self(), X, Y, Station_num},
				    Manager ! {get_neighbors, self(), X, Y, Station_num},
				    %%io:format("station_border_test: after send to creator ~p~n",[self()]),
				    {Angle, X, Y}
			    end
		    end
	    end
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% send_disconnect_to_children funcation:                                                     %%
%%                               when the link broken we send a message to all  children's    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send_disconnect_to_children(_X,[],[]) ->
    [];
send_disconnect_to_children([],_Some_children_list, New_childrens) ->
    New_childrens;
send_disconnect_to_children([{Disconnect_pid,_false}|New_path_t], [Ch_h|Ch_t], Childrens) ->
    [{To_pid}|[List]] = Ch_h,
    if 
	(To_pid =:= Disconnect_pid) ->
	    lists:foreach(fun(X)-> X!{disconnected, To_pid} end, List),
	    New_childrens = Childrens--[Ch_h],
	    send_disconnect_to_children(New_path_t, New_childrens, New_childrens);
	true ->
	    if 
		length(Ch_t > 0) ->
		    send_disconnect_to_children([{Disconnect_pid,_false}|New_path_t],Ch_t,Childrens);
		true ->
		    %%io:format("send_disconnect_to_children: ~p~n",[self()]),
		    send_disconnect_to_children([],[],Childrens)
	    end
    end.    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  rearrange_tree_list function:                                                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rearrange_tree_list([], Tree_list, Num, Find_path_list) ->
    if 
	(Num == 1) ->
	    rearrange_tree_list(Find_path_list, Tree_list, 3, Find_path_list);
	true ->
	    New_tree_list = remove_unknown_fathers(Tree_list, Tree_list),
	    {New_tree_list, Find_path_list}
    end;
rearrange_tree_list([{Disconnect_pid,_false}|Dis_nei_t], Tree_list, Num, Find_path_list) ->
    case lists:keysearch(Disconnect_pid, Num, Tree_list) of	
	{value, Tuple} -> 
	    if 
		(Num == 1) ->
		    New_tree_list = Tree_list--[Tuple],
		    rearrange_tree_list(Dis_nei_t, New_tree_list, Num, Find_path_list);
		true -> 
		    {Pid, _Hops, _Father, _Count}=Tuple,
		    New_find_path_list = [{Pid,false}|Find_path_list],
		    rearrange_tree_list(Dis_nei_t, Tree_list, Num, New_find_path_list)
	    end;
	false -> 
	    rearrange_tree_list(Dis_nei_t, Tree_list, Num, Find_path_list)
    end.

remove_unknown_fathers([], Tree_list) ->
    Tree_list;
remove_unknown_fathers([{DstinationPid, NumOfHops, FatherPid, SN}|T], Tree_list) ->
    case lists:keysearch(DstinationPid, 3, Tree_list) of
	{value, _Tuple}->
	    remove_unknown_fathers(T, Tree_list);
	false ->
	    New_tree_list = Tree_list--[{DstinationPid, NumOfHops, FatherPid, SN}],
	    remove_unknown_fathers(T, New_tree_list)
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  update_archive function:                                                                  %%
%%                        update the archive (archive - save the offer that all pid send me)  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_archive([], [], Archive) -> 
    Archive;
update_archive([], [_H|_T], Archive) ->
    Archive;
update_archive([{_Disconnect_pid,_false}|Disconected_neighbors_list_tail], [],Archive) ->
    update_archive(Disconected_neighbors_list_tail, Archive ,Archive);    
update_archive([{Disconnect_pid,_false}|Disconected_neighbors_list_tail], [H|T],Archive) ->
    [{Pid}|_L] = H,
    if 
	(Pid =:= Disconnect_pid) ->
	    New_Archive = Archive--[H],
	    update_archive(Disconected_neighbors_list_tail, New_Archive, New_Archive);
	true -> 
	    update_archive([{Disconnect_pid, _false}|Disconected_neighbors_list_tail], T, Archive)
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% clear_disconnected_pids_from_children function:                                            %%
%%                                     delete a pid that go far a way from the children list  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear_disconnected_pids_from_children([], [], Childrens) ->
    Childrens;
clear_disconnected_pids_from_children([], [_H|_T], Childrens) ->
    Childrens;
clear_disconnected_pids_from_children([{_Disconnect_pid,_false}|New_path_t], [], Childrens) ->
    clear_disconnected_pids_from_children(New_path_t, Childrens, Childrens);
clear_disconnected_pids_from_children([{Disconnect_pid,_false}|New_path_t], [H|T], Childrens) ->
    [{Pid}|[List]] = H,
    New_List = lists:delete(Disconnect_pid,List),
    if 
	(New_List == []) ->
	    NewChildrens = Childrens--[[{Pid},List]];
	true ->
	    NewChildrens = [[{Pid}, New_List] | Childrens--[[{Pid},List]]]
    end,
    clear_disconnected_pids_from_children([{Disconnect_pid,_false}|New_path_t], T, NewChildrens).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  exit function:the station is terminated by user 		                              %%                                
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

exit()->
    io:format("station ~p exit by user~n",[self()]).
