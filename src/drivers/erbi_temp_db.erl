%%% -*- coding:utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
-module(erbi_temp_db).
-include("erbi.hrl").

-callback start_temp(PropList::[property()])->
    ok.


-callback stop_temp(PropList::[property()])->
    ok.
    
-callback get_temp_connect_data(PropList::[property()],
                                Args::[any()] | atom(),
                                Username::unicode:chardata(),
                                Password::unicode:chardata())->
    {[property()],
     [any()] | atom(),
     unicode:chardata(),
     unicode:chardata()}.
    
                                
