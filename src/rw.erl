%% Author: 
%% Created: Jun 7, 2011

-module(rw).

-compile ( export_all ).

%% Open and read configuration data
%% input: 
%% File - List (FileName)

read_file ( File ) ->
    case file:consult(File) of
	{ok, Settings} -> 
	    Settings;
	
	{error, Why}  ->
	    io:format("Error reading settings file: ~p~n", [Why])
    end.

read_options( [], _What ) -> error;
read_options( [H_Tuple | T_Tuple], What) ->
    case H_Tuple of
	{What,Value} -> Value; 	
	_Else -> read_options(T_Tuple, What)
    end.

