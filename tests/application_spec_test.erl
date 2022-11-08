%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Created :
%%% Node end point  
%%% Creates and deletes Pods
%%% 
%%% API-kube: Interface 
%%% Pod consits beams from all services, app and app and sup erl.
%%% The setup of envs is
%%% -------------------------------------------------------------------
-module(application_spec_test).      
 
-export([start/0]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
    io:format("Start  ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    ok=setup(),
    
    ok=read_spec(),
    ok=all_names(),
    ok=info(),
    ok=item(),
    
           
    io:format("Test OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),
  %  init:stop(),
    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
read_spec()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
  
    {ok,Spec}=application_spec:read_spec(),
    [
     [{name,"controller_app"},{vsn,["0.1.0"]},{app,controller_app},{gitpath,"https://github.com/joq62/controller_app.git"}]
     |_]=lists:sort(Spec),
      
    io:format("Stop OK !!! ~p~n",[?FUNCTION_NAME]),
    ok.




%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
all_names()->
     io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    AllNames=application_spec:all_names(),
    ["controller_app"|_]=lists:sort(AllNames),
  
    io:format("Stop OK !!! ~p~n",[?FUNCTION_NAME]),    
    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
info()->
     io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    Info=application_spec:info("test_app_1"),
    [{name,"test_app_1"},
     {vsn,["0.1.0"]},
     {app,test_app_1},
     {gitpath,"https://github.com/joq62/test_app_1.git"}
    ]=Info, 
    
    io:format("Stop OK !!! ~p~n",[?FUNCTION_NAME]),    
    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
item()->

    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    
    ["0.1.0"]=config:application_vsn("test_app_1"),
    test_app_1=config:application_app("test_app_1"),
    "https://github.com/joq62/test_app_1.git"=config:application_gitpath("test_app_1"),

    {error,[application_name_eexists,"glurk"]}=config:application_gitpath("glurk"),
    
    io:format("Stop OK !!! ~p~n",[?FUNCTION_NAME]),    

    ok.


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
-define(SourceFile,"./tests/specs/spec.application").
-define(File,"spec.application").
	 	 

setup()->
    io:format("Start ~p~n",[?FUNCTION_NAME]),
    file:delete(?File),
    {ok,Bin}=file:read_file(?SourceFile),
    ok=file:write_file(?File,Bin),
       
    io:format("Stop OK !!! ~p~n",[?FUNCTION_NAME]),

    ok.
