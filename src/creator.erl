%% Author: 
%% Created: Aug 15, 2011

-module(creator).
-compile(export_all).

-define (CREATION_DELAY, 100).
-define (MASTER, 'sasha1@132.72.253.29').
-define (SLAVE1, 'sasha2@132.72.253.29').
-define (SLAVE2, 'adir1@132.72.248.206').
-define (SLAVE3, 'adir2@132.72.248.206').
%%-define (SLAVE2, 'kobi1@132.72.253.242').
%%-define (SLAVE3, 'kobi2@132.72.253.242').
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Start function: creates a process manager in the future will resemble the channel         %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() -> 
    spawn(creator, manager_job,[[]]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  managar job function: Reads data from the file "settings"                                 %%
%%  creates a process responsible for running the graphics                                    %%
%%  Create all the stations                                                                   %%
%%  ready to receive two types of message stations):                                          %%
%%  Updated location,                                                                         %%
%%  request to know who the neighbors of each station.                                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
manager_job ([])->
    manager_job (self());

manager_job (Master_pid)->
    Mypid = self(),
    Settings = rw:read_file("settings.txt"), 
    Usr_num = rw:read_options(Settings, num_of_station),
    Radius = rw:read_options(Settings, reception_radius),
    Speed = rw:read_options(Settings, station_speed),
    Lenght = rw:read_options(Settings, length),
    Paint_station = rw:read_options(Settings, paint_station),
    
    net_kernel:start([?MASTER]),
    erlang:set_cookie(node(), aa),
    net_adm:ping(?SLAVE1),
    net_adm:ping(?SLAVE2),
    net_adm:ping(?SLAVE3),
    io:format("manager_job:~nnode: ~p~ncookie: ~p~nconnected: ~p~n",[node(), erlang:get_cookie(), nodes(connected)]),
    EtsID = ets:new(location_data, [bag, public, named_table, {write_concurrency, true}, {read_concurrency, true}]),
    create_stations(Usr_num, Paint_station, Radius, Speed, Lenght, Mypid, Master_pid, EtsID),
    Graphic_pid = spawn(graphic, start_frame,[Paint_station, Lenght, Mypid, Radius, EtsID, main]),   
    get_message(Graphic_pid, Radius, EtsID, []).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  create stations function:  Create all the stations                                        %%
%%  Creates all the stations and each station gives the following data:                       %%
%%  radius - distance power transmission,                                                     %%
%%  angle - since the beginning of the movement,                                              %%
%%  length - the maximum movement distance,                                                   %%
%%  Director - Pid of the Director,                                                           %%
%%  speed-Step to do the station at any time.                                                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_stations(0, _Paint_station, _Radius, _Speed, _Lenght, _Manager, _Master_pid, _EtsID) ->
    ok;
create_stations(Usr_num, Paint_station, Radius, Speed, Length, Manager,Master_pid,EtsID) ->
    timer:sleep(random:uniform(?CREATION_DELAY)), % create different start time between the station. 
    Angle = random:uniform(360),
    Connected_nodes = nodes(connected),
    Temp1 = lists:member(?SLAVE1 ,Connected_nodes),
    if(Temp1)->
	    X = random:uniform(Length*2);
      true ->
	    X = random:uniform(Length)
    end,
    Temp2 = lists:member(?SLAVE2 ,Connected_nodes),
    if(Temp2)->
	    Y = random:uniform(Length*2);
      true ->
	    Y = random:uniform(Length)
    end,
    if(X > Length)->
	    if(Y > Length)->
		    %%io:format("creat_station_kobi2: ~p~ndata_before: ~p~n~n",[Usr_num, ets:match(EtsID, {'_', {'$1', '$2', '$3', '$4'}})]), 
		    Pid = spawn(?SLAVE3, station, user, [?MASTER, Usr_num, Paint_station, Length, Speed, Angle, X, Y, Manager, []]),
		    ets:insert(EtsID,{?SLAVE3,{Pid,X,Y,Usr_num}});
	      true ->
		    %%io:format("creat_station_sasha2: ~p~ndata_before: ~p~n~n",[Usr_num, ets:match(EtsID, {'_', {'$1', '$2', '$3', '$4'}})]), 
		    Pid = spawn(?SLAVE1, station, user, [?MASTER, Usr_num, Paint_station, Length, Speed, Angle, X, Y, Manager, []]),
		    ets:insert(EtsID,{?SLAVE1,{Pid,X,Y,Usr_num}})
	    end;
      true ->
	    if(Y > Length)->
		    %%io:format("creat_station_kobi1: ~p~ndata_before: ~p~n~n",[Usr_num, ets:match(EtsID, {'_', {'$1', '$2', '$3', '$4'}})]), 
		    Pid = spawn(?SLAVE2, station, user, [?MASTER, Usr_num, Paint_station, Length, Speed, Angle, X, Y, Manager, []]),
		    ets:insert(EtsID,{?SLAVE2,{Pid,X,Y,Usr_num}});
	      true ->
		    %%io:format("creat_station_sasha1: ~p~ndata_before: ~p~n~n",[Usr_num, ets:match(EtsID, {'_', {'$1', '$2', '$3', '$4'}})]), 
		    Pid = spawn(?MASTER, station, user, [?MASTER, Usr_num, Paint_station, Length, Speed, Angle, X, Y, Manager, []]),
		    ets:insert(EtsID,{?MASTER,{Pid,X,Y,Usr_num}})
	    end
    end,	  
    create_stations(Usr_num - 1, Paint_station, Radius, Speed, Length, Manager, Master_pid, EtsID).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  get message function:  know to recive two types of messages:                              %%
%%  update loction, and get neighbors (-Request for a list of my neighbors).                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_message (Graphic_pid, Radius, EtsID, Tree_Graphic_pid) ->
    receive
	{update, Node_name, Pid, X, Y, Station_num} -> 
	    ets:match_delete(EtsID,{Node_name, {Pid,'_','_','_'}}),
	    ets:insert(EtsID,{Node_name,{Pid, X, Y, Station_num}}),
	    get_message (Graphic_pid, Radius, EtsID, Tree_Graphic_pid);
		
	{get_neighbors, Pid, X, Y, Station_num} -> 
	    Neighbors_list = get_neighbors (Pid, EtsID, Radius, X, Y, Station_num),
	    Pid ! {neighbors, Neighbors_list},
	    Pid ! {stop, self()},
	    get_message (Graphic_pid, Radius, EtsID, Tree_Graphic_pid);

	{pained_tree_station_num, Wanted_station_num, New_Tree_Graphic_pid} ->
	    [[Pid]] = ets:match(EtsID,{'_',{'$1', '_', '_', Wanted_station_num}}),
	    Pid ! {start_pain_your_tree, New_Tree_Graphic_pid},
	    get_message (Graphic_pid, Radius, EtsID, New_Tree_Graphic_pid);

	{stop_paint, Station_num} ->
	    [[Pid, _X, _Y]] = ets:match(EtsID,{'_',{'$1', '$2', '$3', Station_num}}),
	    Pid ! {stop_pain_your_tree},
	    get_message (Graphic_pid, Radius, EtsID, []);

	{stop}->
	    Stations_pids = ets:match(EtsID,{'_',{'$1','_','_','_'}}), 
	    lists:foreach(fun([Pid]) -> Pid ! {stop} end, Stations_pids),
	    Graphic_pid ! {stop},
            io:format("creator_stop:~n");
	_ -> error
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  get neighbors function:  returns the station's neighbors                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   
    

get_neighbors(Pid, EtsID, Radius, X, Y,Station_num) ->
    Data = ets:match(EtsID, {'_', {'$1', '$2', '$3', '$4'}}), 
    %%io:format("get_neighbors: Pid: ~p~nData: ~p~n",[Pid, Data]), 
    compare({Pid, X, Y, Station_num}, Data, Radius, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  compare function: Calculates which of the stations is a neighbor of this station          %% 
%%  and returns the station's neighbors (-New_Neighbors_list).                                %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compare({_Pid, _X, _Y,_Station_num}, [], _Radius, Neighbors_list) -> 
    Neighbors_list;
compare({Pid, X, Y, Station_num}, [H|T], Radius, Neighbors_list) ->
    [Pid2, X2, Y2, _S_Station_num] = H,
    Distance = math:sqrt(math:pow(X-X2, 2) + math:pow(Y-Y2, 2)),
    if 
	(Distance =< Radius) and (Pid =/= Pid2) -> 
	    New_Neighbors_list = [Pid2|Neighbors_list],
	    compare({Pid,X,Y,Station_num},T, Radius, New_Neighbors_list);
	true -> 
	    compare({Pid, X, Y,Station_num}, T, Radius, Neighbors_list)
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  stop program function: stop the program - send to all stations command to stop            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
