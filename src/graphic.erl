%% Author: 
%% Created: Aug 3, 2011

-module(graphic).
-compile(export_all).
-include_lib("wx/include/wx.hrl").

-define (REFRESH_INTERVAL, 100).
-define (CREATION_DELAY, 1000).
-define (STATION_RADIUS, 6).  %% define the radius of the station

-define (MASTER, 'sasha1@132.72.253.29').
-define (SLAVE1, 'sasha2@132.72.253.29').
-define (SLAVE2, 'adir1@132.72.248.206').
-define (SLAVE3, 'adir2@132.72.248.206').
%%-define (SLAVE2, 'kobi1@132.72.253.242').
%%-define (SLAVE3, 'kobi2@132.72.253.242').


start_frame(Paint_station, Length, ManagerPid, Radius, EtsID, FrameType) ->
    ConnectedFrames = nodes(connected),
    Key1 = lists:member(?SLAVE1, ConnectedFrames),
    if(Key1)->
	    FrameLength = 2*Length + 2*?STATION_RADIUS;
      true ->
	    FrameLength = Length + 2*?STATION_RADIUS
    end,
    Key2 = lists:member(?SLAVE2, ConnectedFrames),
    if(Key2)->
	    FrameWidth = 2*Length + 2*?STATION_RADIUS;
      true ->
	    FrameWidth = Length + 2*?STATION_RADIUS
    end,
    io:format("graphic:~nnode: ~p~ncookie: ~p~nconnected: ~p~nFrameWidth: ~p~nFrameLength: ~p~n",[node(), erlang:get_cookie(), nodes(connected),FrameWidth,FrameLength]),
    if
	(FrameType == main)->
	    {Frame, Panel} = make_main_frame(Length, FrameLength, FrameWidth),
	    {_Ok, TRef} = timer:send_interval(?REFRESH_INTERVAL,{refresh}),
	    main_frame_loop ({Frame, Panel, Length, FrameLength, FrameWidth, ManagerPid}, TRef, EtsID, Radius, Paint_station),
	    wx:destroy();
	true ->
	    {Frame, Panel} = make_tree_frame(Length, FrameLength, FrameWidth),
	    {_ok, TRef} = timer:send_interval(?REFRESH_INTERVAL,{refresh}), 
	    tree_frame_loop ({Frame, Panel, Length, FrameLength, FrameWidth, ManagerPid}, TRef, EtsID, Radius, Paint_station),
	    wx:destroy()
    end.


make_main_frame(UserLength, Length, Width) ->
    Server = wx:new(),
    Frame = wxFrame:new(Server, -1, "Ad-Hoc Zone", [{size,{Length, Width + 50}}]),
    Panel  = wxPanel:new(Frame),
    B102 = wxButton:new(Panel, ?wxID_EXIT, [{label, "E&xit"}]),
    B101 = wxButton:new(Panel, 101, [{label, "&Paint_tree"}]),

    %%You can create sizers before or after the widgets that will go into them, but
    %%the widgets have to exist before they are added to sizer.
    MainSizer  = wxBoxSizer:new(?wxVERTICAL),
    ButtonSizer = wxBoxSizer:new(?wxHORIZONTAL),
    OnPaint2 = fun(_Evt, _Obj) ->
		      Paint = wxPaintDC:new(Panel),
		      Pen = wxPen:new(),
		      
		      wxPen:setColour(Pen, ?wxBLUE),
		      wxDC:setPen(Paint, Pen),
		      wxDC:drawLine(Paint, {0, Width}, {Length, Width}),
		       if
			   (UserLength + 2*?STATION_RADIUS =/= Length) -> 
			       wxDC:drawLine(Paint, {round(Length/2), 0}, {round(Length/2), Width});
			   true ->
			       ok 
		       end,
		       if
			   (UserLength + 2*?STATION_RADIUS =/= Width) -> 
			       wxDC:drawLine(Paint, {0, round(Width/2)}, {Length, round(Width/2)});
			   true ->
			       ok 
		       end,
		      
		      wxPen:setColour(Pen, ?wxRED),
		      wxDC:setPen(Paint, Pen), 		      
		      wxPen:destroy(Pen),
		      wxPaintDC:destroy(Paint)
	      end,
    
    %% Note that the widget is added to the sizer using the VARIABLE, not the ID.
    %% The order that they are added to the sizers controls their appearance in the window.ky
    wxSizer:addSpacer(MainSizer, Width+5),  %spacer
    wxSizer:addSpacer(ButtonSizer, Length - 160),  
    wxSizer:add(ButtonSizer, B101,  []),  
    wxSizer:add(ButtonSizer, B102,  []),
    
    wxSizer:add(MainSizer, ButtonSizer, []),    
    %% Now 'set' MainSizer into the Panel
    wxPanel:setSizer(Panel, MainSizer),
    wxFrame:connect(Panel, paint, [{callback, OnPaint2}]),
    wxFrame:show(Frame),
    
    %% create two listeners
    wxFrame:connect( Frame, close_window),
    wxPanel:connect(Panel, command_button_clicked),
    {Frame, Panel}.

make_tree_frame(UserLength, Length, Width) ->
    Server = wx:new(),
    Frame = wxFrame:new(Server, -1, "Station Tree in Ad-Hoc Zone", [{size,{Length, Width + 50}}]),
    Panel  = wxPanel:new(Frame),
 
    B102 = wxButton:new(Panel, ?wxID_EXIT, [{label, "E&xit"}]),
    %%You can create sizers before or after the widgets that will go into them, but
    %%the widgets have to exist before they are added to sizer.
    MainSizer  = wxBoxSizer:new(?wxVERTICAL),
    ButtonSizer = wxBoxSizer:new(?wxHORIZONTAL),
    OnPaint1 = fun(_Evt, _Obj) ->
		       Paint = wxPaintDC:new(Panel),
		       Pen = wxPen:new(),      
		       wxPen:setColour(Pen, ?wxBLUE),
		       wxDC:setPen(Paint, Pen),
		       wxDC:drawLine(Paint, {0, Width}, {Length, Width}),
		       if
			   (UserLength + 2*?STATION_RADIUS =/= Length) -> 
			       wxDC:drawLine(Paint, {round(Length/2), 0}, {round(Length/2), Width});
			   true ->
			       ok 
		       end,
		       if
			   (UserLength + 2*?STATION_RADIUS =/= Width) -> 
			       wxDC:drawLine(Paint, {0, round(Width/2)}, {Length, round(Width/2)});
			   true ->
			       ok 
		       end,
		       wxPen:destroy(Pen),
		       wxPaintDC:destroy(Paint)
	       end,
    
    %% Note that the widget is added to the sizer using the VARIABLE, not the ID.
    %% The order that they are added to the sizers controls their appearance in the window.
    wxSizer:addSpacer(MainSizer, Width+5),  %spacer
    wxSizer:addSpacer(ButtonSizer, Length - 80),
    wxSizer:add(ButtonSizer, B102,  []),
    
    wxSizer:add(MainSizer, ButtonSizer, []),    
    %% Now 'set' MainSizer into the Panel
    wxPanel:setSizer(Panel, MainSizer),
    wxFrame:connect(Panel, paint, [{callback, OnPaint1}]),
    wxFrame:show(Frame),
    
    %% create two listeners
    wxFrame:connect( Frame, close_window),
    wxPanel:connect(Panel, command_button_clicked),
    {Frame, Panel}.

main_frame_loop({Frame, Panel, UserLength, Length, Width, ManagerPid}, TRef, EtsID, Radius, Paint_station)->
    receive
        % a connection get the close_window signal
        % and sends this message to the server
        #wx{event=#wxClose{}} ->
	    %%now we use the reference to Frame
	    wxWindow:destroy(Frame),  %closes the window
	    ManagerPid ! {stop},
	    timer:cancel(TRef),
	    ok;  % we exit the loop

        #wx{id = ?wxID_EXIT, event=#wxCommand{type = command_button_clicked} } ->
            %%this message is sent when the exit button (ID 102) is clicked
            %%the other fields in the tuple are not important to us.
	    timer:cancel(TRef),
	    wxWindow:destroy(Frame),
	    ManagerPid ! {stop},
	    ok;
	#wx{event=#wxCommand{type = command_button_clicked}} ->
	    Tree_Graphic_pid = spawn(graphic, start_frame,[Paint_station, UserLength, ManagerPid, Radius, EtsID, tree]),
	    ManagerPid ! {pained_tree_station_num, Paint_station, Tree_Graphic_pid},
	    main_frame_loop({Frame, Panel, UserLength, Length, Width, ManagerPid}, TRef, EtsID, Radius, Paint_station);	
	
	{refresh}->
	    OnPaint3 = fun(_Evt, _Obj) ->
			       Paint = wxPaintDC:new(Panel),
			       Pen = wxPen:new(),
			       
			       wxPen:setColour(Pen, ?wxBLUE),
			       wxDC:setPen(Paint, Pen), 
			       Data = ets:match(EtsID,{'_',{'_','$1','$2','_'}}),
			       pain_station_radius(Data, Paint, Radius),
			       
			       [[_Pid, X, Y]] = ets:match(EtsID,{'_',{'$1', '$2', '$3', Paint_station}}),
			       wxDC:drawCircle(Paint,{round(X+?STATION_RADIUS), round(Y+?STATION_RADIUS)}, ?STATION_RADIUS),
			       wxDC:drawLine(Paint, {0, Width}, {Length, Width}),
			       
			       if
				   (UserLength + 2*?STATION_RADIUS =/= Length) -> 
				       wxDC:drawLine(Paint, {round(Length/2), 0}, {round(Length/2), Width});
				   true ->
				       ok 
			       end,
			       if
				   (UserLength + 2*?STATION_RADIUS =/= Width) -> 
				       wxDC:drawLine(Paint, {0, round(Width/2)}, {Length, round(Width/2)});
				   true ->
				       ok 
			       end,
			       
			       %%same pen, change color    
			       wxPen:setColour(Pen, ?wxRED),
			       wxDC:setPen(Paint, Pen), 
			       pain_station(ets:match(EtsID, {'_', {'$1', '$2', '$3', '$4'}}), Paint, Paint_station),
				   
			       wxPen:destroy(Pen),
			       wxPaintDC:destroy(Paint)
		       end,
	    
	    wxFrame:connect(Panel, paint, [{callback, OnPaint3}]),
	    wxFrame:refresh(Frame),
	    main_frame_loop({Frame, Panel, UserLength, Length, Width, ManagerPid}, TRef, EtsID, Radius,Paint_station)	;									       
	{stop} ->
	    io:format("main_frame_getStopMessage: ~n")
    end. 

tree_frame_loop({Frame, Panel, UserLength, Length, Width, ManagerPid}, TRef, EtsID, Radius, Paint_station) ->
    receive
	%% a connection get the close_window signal
	%% and sends this message to the server
        #wx{event=#wxClose{}} ->
            io:format("~p Closing window ~n",[self()]), %optional, goes to shell
	    %%now we use the reference to Frame
	    wxWindow:destroy(Frame),  %closes the window
	    ManagerPid ! {stop_paint, Paint_station},
	    ok;  % we exit the loop
	
        #wx{id = ?wxID_EXIT, event=#wxCommand{type = command_button_clicked} } ->
            %%this message is sent when the exit button (ID 102) is clicked
            %%the other fields in the tuple are not important to us.
	    wxFrame:destroy(Frame),
	    ManagerPid ! {stop_paint, Paint_station},
	    ok; % we exit the loop


	{pain_tree, Root, Tree, Paint_station} ->
	    [[Root_x, Root_y]] = ets:match(EtsID,{'_',{Root, '$1', '$2','_'}}),
	    {List, Pid_list} = find_coordinate(EtsID, Root, Tree, [], []),
	    OnPaint4 = fun(_Evt, _Obj) ->
			       Paint = wxPaintDC:new(Panel),
			       Pen = wxPen:new(),
			       
			       wxPen:setColour(Pen, ?wxRED),
			       wxDC:setPen(Paint, Pen), 
			       pain_tree_ribs(Paint, List),
			       pain_tree_station(Paint, Pid_list),
			       wxPen:setColour(Pen, ?wxBLUE),
			       wxDC:setPen(Paint, Pen), 
			       wxDC:drawCircle(Paint,{round(Root_x + ?STATION_RADIUS), round(Root_y + ?STATION_RADIUS)}, ?STATION_RADIUS),
			       wxDC:drawLine(Paint, {0, Width}, {Length, Width}),
			       if
				   (UserLength + 2*?STATION_RADIUS =/= Length) -> 
				       wxDC:drawLine(Paint, {round(Length/2), 0}, {round(Length/2), Width});
				   true ->
				       ok 
			       end,
			       if
				   (UserLength + 2*?STATION_RADIUS =/= Width) -> 
				       wxDC:drawLine(Paint, {0, round(Width/2)}, {Length, round(Width/2)});
				   true ->
				       ok 
			       end,
			       
			       wxPen:destroy(Pen),
			       wxPaintDC:destroy(Paint)
		       end,
	    wxFrame:connect(Panel, paint, [{callback, OnPaint4}]),
	    wxFrame:refresh(Frame),
	    tree_frame_loop({Frame, Panel, UserLength, Length, Width, ManagerPid}, TRef, EtsID, Radius,Paint_station);						
	{stop} ->
	    ok
	end.

find_coordinate(_EtsID, _Root, [], List, Pid_list) ->
    {List,Pid_list};
find_coordinate(EtsID, Root, [{Neighbor_pid, _Num_of_hops, Father_pid, _SN}|Tree_tail], List, Pid_list) ->
    if
	(Neighbor_pid =/= Father_pid)->
	    [[X1, Y1]] = ets:match(EtsID,{'_',{Neighbor_pid, '$1', '$2','_'}}),
	    [[X2, Y2]] = ets:match(EtsID,{'_',{Father_pid, '$1', '$2','_'}}),
	    New_list = [{X1, Y1, X2, Y2}|List];
	true->
	    [[X1, Y1]] = ets:match(EtsID,{'_',{Neighbor_pid, '$1', '$2','_'}}),
	    [[X2, Y2]] = ets:match(EtsID,{'_',{Root, '$1', '$2','_'}}),
	    New_list = [{X1, Y1, X2, Y2}|List]
	end,
    find_coordinate(EtsID, Root, Tree_tail, New_list, [{Neighbor_pid, X1, Y1}|Pid_list]).
   

pain_tree_station(_Paint, [])->
    ok;
pain_tree_station(Paint, [{_Pid, X, Y}|List])->
    wxDC:drawCircle(Paint,{round(X+?STATION_RADIUS), round(Y+?STATION_RADIUS)}, ?STATION_RADIUS),
    pain_tree_station(Paint, List).
   
pain_tree_ribs(_Paint, [])->
    ok; 
pain_tree_ribs(Paint, [{X1, Y1, X2, Y2}|T]) ->
    wxDC:drawLine(Paint, {round(X1 + ?STATION_RADIUS), round(Y1 + ?STATION_RADIUS)}, {round(X2 + ?STATION_RADIUS), round(Y2 + ?STATION_RADIUS)}),
    pain_tree_ribs(Paint, T).   

pain_station([], _Paint, _Paint_station)->
    ok;
pain_station([[_Pid, X, Y, Station_num]|T], Paint, Paint_station)->
    if
	(Station_num =/= Paint_station)->
	    wxDC:drawCircle(Paint,{round(X+?STATION_RADIUS), round(Y+?STATION_RADIUS)}, ?STATION_RADIUS);
	true->
	    ok
    end,
    pain_station(T, Paint, Paint_station).

pain_station_radius([], _Paint, _Radius) ->
    ok;
pain_station_radius([[X, Y]|T], Paint, Radius) ->
    wxDC:drawCircle(Paint,{round(X+?STATION_RADIUS), round(Y+?STATION_RADIUS)}, Radius),
    pain_station_radius(T, Paint, Radius).
