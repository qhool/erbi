%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%% @copyright 2013 Voalte Inc. <jburroughs@voalte.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% @doc
%% Main erbi module.
%% @end
-module(erbi).
-export([connect/4,connect/3,connect/2,connect/1,
         driver_call/3,
         normalize_data_source/1,
         normalize_data_source/2,
         normalize_properties/2,
         parse_data_source/1,
         get_driver_module/1,
         start/0,
         start/2,
         stop/1]).
-include("erbi.hrl").
-include("erbi_private.hrl").

-behavior(application).

%% --------------------------------------
%% @doc Allows erbi application to be started with -s option from command line.
%% @end
%% --------------------------------------
-spec start() -> 'ok' | {'error', _Reason}.
start() ->
    case application:start(erbi) of
        {error,{{already_started,_},_}} ->
            ok;
        R -> R
    end.

%% --------------------------------------
%% @doc Connect to a database.
%%
%% <ul><li>DataSource  - DB connect term or string "erbi:Driver:params"</li>
%% <li>Username</li>
%% <li>Password</li>
%% </ul>
%% @end
%% --------------------------------------
-spec connect( DataSource :: unicode:chardata() | erbi_data_source() | erbi_connect_tuple(),
               Username :: undefined | unicode:chardata() ,
               Password :: undefined | unicode:chardata() ) ->
                     { ok, erbi_connection() } | { error, any() }.
connect( DataSource, Username, Password ) ->
    connect( DataSource, Username, Password, default ).


-spec connect( DataSource :: unicode:chardata() | erbi_data_source() | erbi_connect_tuple(),
               Username :: undefined | unicode:chardata() ,
               Password :: undefined | unicode:chardata() ,
               Timeout :: pos_integer() | default ) ->
                     { ok, erbi_connection() } | { error, any() }.
connect( DataSource, Username, Password, Timeout ) when is_list(DataSource) ->
    connect( parse_data_source( DataSource ), Username, Password, Timeout );
connect( {erbi,Driver}, Username, Password, Timeout ) ->
    connect( #erbi{ driver = Driver }, Username, Password, Timeout );
connect( {erbi,Driver,Props}, Username, Password, Timeout ) ->
    connect( #erbi{ driver = Driver, properties = Props }, Username, Password, Timeout );
connect( #erbi{} = DataSource, Username, Password, Timeout ) ->
    connect( erbi_pool:scrape_pool_properties(DataSource), Username, Password, Timeout );
connect( {[], DataSource}, Username, Password, _Timeout ) -> % requesting non-pooled connection
    Module = get_driver_module(DataSource),
    case normalize_data_source(Module,DataSource) of
        {error,_}=E ->
            E;
        DataSource1 ->
            Info = Module:driver_info(),
            ConnectReq = {Module,Info,DataSource1,Username,Password},
            case gen_server:start(erbi_driver,ConnectReq,[]) of
                {ok,Pid} ->
                    true = link(Pid),
                    Connection = {erbi_connection,#conn{ pid = Pid }},
                    {ok, Connection};
                ignore -> {error,ignore};
                {error,Reason} -> {error,Reason}
            end
    end;
connect( {PoolProps, DataSource}, Username, Password, Timeout ) when is_list(PoolProps) -> % requesting pooled connection
    Module = get_driver_module(DataSource),
    case normalize_data_source(Module,DataSource) of
        {error,_}=E ->
            case proplists:get_value(name, PoolProps, undefined) of
                undefined -> E;
                % this to account for fetching connection by the code that assumes
                % pools are pre-created. So a call like this erbi:connect("erbi:epgsql:pool_name=test")
                % should not fail unless we have no pool.
                PoolName -> erbi_pool:checkout(PoolName,
                                               proplists:get_value(queue, PoolProps, false),
                                               Timeout
                                              )
            end;
        DataSource1 ->
            ConnectReq = {Module,Module:driver_info(),DataSource1,Username,Password},
            PoolName = proplists:get_value(name, PoolProps),
            PoolQueue = proplists:get_value(queue, PoolProps),
            {ok, _Pool} = erbi_pool:start_pool(PoolName, PoolProps, ConnectReq),
            erbi_pool:checkout(PoolName, PoolQueue, Timeout)
    end.


%% --------------------------------------
%% @doc Connect without supplying username/password
%%
%% Same as {@link connect/3} with undefined for the 2nd and 3rd args.
%% @end
%% --------------------------------------
-spec connect( DataSource :: unicode:chardata() | erbi_data_source() ) ->
                     { ok, erbi_connection() } | { error, any() }.
connect( DataSource ) ->
    connect( DataSource, undefined, undefined ).

-spec connect( DataSource :: unicode:chardata() | erbi_data_source(),
               Timeout :: pos_integer()) ->
                     { ok, erbi_connection() } | { error, any() }.
connect( DataSource, Timeout ) ->
    connect( DataSource, undefined, undefined, Timeout ).


%% --------------------------------------
%% @doc normalize data source.
%%
%% Takes a data source descriptor (as a string or tuple),
%% and produces a normalized version.  This function uses
%% driver-specific callbacks, so it will fail if the specified
%% driver cannot be found.
%% @end
%% --------------------------------------
-spec normalize_data_source( DataSource :: unicode:chardata() | erbi_data_source() ) ->
                                   erbi_data_source().

normalize_data_source( DataSource ) when is_list(DataSource) ->
    case parse_data_source(DataSource) of
        {error,_}=E -> E;
        DS -> normalize_data_source(DS)
    end;
normalize_data_source( DataSource ) ->
    Module = get_driver_module(DataSource),
    normalize_data_source( Module, DataSource ).

%% --------------------------------------
%% @doc Parse data source string.
%%
%% Takes a data source descriptor in the form "erbi:driver:arg=val;arg=val[...]"
%% and returns an erbi_data_source() value.
%% @end
%% --------------------------------------
-spec parse_data_source( DataSource :: unicode:chardata() ) ->
                               erbi_data_source() | { error, any() }.


parse_data_source( DataSource ) ->
    Charlist = unicode:characters_to_list(DataSource),
    Tokens=scan_ds(Charlist,[],[]),
    parse_ds( Tokens ).

%% --------------------------------------
%% @doc Call driver function
%%
%% Calls the specified function on the driver module
%% with the given args.  This is exactly the same as
%% {@link erlang:apply/3}, except that the actual module
%% of the driver is resolved for you -- driver can be specified
%% using any valid datasource term (string or record),
%% or an atom.  This function is mainly here for completeness
%% for something much more useful, see {@link erbi_connection:driver_call}
%% and {@link erbi_statement:driver_call}
%% @end
%% --------------------------------------
-spec driver_call( DriverSpec :: erbi_data_source() | unicode:chardata() | atom(),
                   Function :: atom(),
                   Args :: [any()] ) ->
                         any().
driver_call( DataSource, Func, Args ) when is_list(DataSource) ->
    DS = parse_data_source(DataSource),
    driver_call(DS,Func,Args);
driver_call( DSOrAtom,Func,Args ) ->
    Module = get_driver_module(DSOrAtom),
    apply(Module,Func,Args).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start(StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term()) ->
    {ok, pid()} |
    {ok, pid(), State :: term()} |
    {error, Reason :: term()}).
start(_StartType, _StartArgs) ->
    ErbiPools = case application:get_env(erbi, pools) of
                    undefined -> [];
                    {ok, Value} -> Value
                end,
    erbi_sup:start_link(ErbiPools).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop(State :: term()) -> term()).
stop(_State) ->
    ok.


%% @headerfile "erbi.hrl"

%%==== Internals ====%%

-spec get_driver_module( atom() | erbi_data_source() ) -> atom().

get_driver_module( #erbi{driver=DriverAtom} ) ->
    get_driver_module(DriverAtom);
get_driver_module( DriverAtom ) when is_atom(DriverAtom) ->
    get_driver_module(atom_to_list(DriverAtom));
get_driver_module( DriverList ) when is_list(DriverList) ->
    Module = list_to_atom("erbdrv_" ++ DriverList),
    {module,Module} = code:ensure_loaded(Module),
    Module.

%% @headerfile "erbi.hrl"

%%==== Internals ====%%
-spec normalize_data_source( Module :: atom(), erbi_data_source() ) ->
                                   erbi_data_source() | {error,any()}.
normalize_data_source(Module,#erbi{properties=Props,args=Args}=DataSource ) ->
    case normalize_properties( Module, Props ) of
        {error,_}=E -> E;
        Props1 ->
            case normalize_args( Module, Args ) of
                {error,_}=E -> E;
                Args1 ->
                    DataSource#erbi{properties = Props1, args=Args1}
            end
    end.

-spec normalize_args( Module :: atom(), Args :: undefined | [any()] ) ->
                            undefined | {error,any()} | term().
normalize_args( _, undefined ) ->
    undefined;
normalize_args( Module, Args ) ->
    case {Args,Module:parse_args(Args)} of
        {[],declined} ->
            undefined;
        {_,declined} ->
            {error,{invalid_datasource,args_not_supported}};
        {_,{error,Reason}} ->
            {error,{invalid_datasource,Reason}};
        {_,A} ->
            A
    end.

%normalize the driver properties;
%uses driver callbacks property_info; does similar
-spec normalize_properties( Module :: atom(), Props :: [property()] ) ->
                                  [property()] | {error,any()}.

normalize_properties(Module,Props) ->
    PropInfo = Module:property_info(),
    %decompose PropInfo:
    Aliases = proplists:get_value(aliases,PropInfo,[]),
    Negations = proplists:get_value(negations,PropInfo,[]),
    Expands = proplists:lookup_all(expand,PropInfo),
    Defaults = proplists:get_value(defaults,PropInfo,[]),
    Required = proplists:get_value(required,PropInfo,[]),
    MakeUnique = proplists:get_value(unique,PropInfo,false),
    %processing phases:
    Props1 = proplists:substitute_aliases(Aliases,Props),
    case validate_properties( Module, Props1 ) of
        {error,Reason} ->
            {error,{invalid_datasource,Reason}};
        {ok,Props2} ->
            Props3 = proplists:normalize(Props2,[{negations,Negations}|Expands]),
            % get the list of default values which are not present
            NeedDefaults =
                lists:filter(fun({DefK,_Val}) ->
                                     case proplists:lookup(DefK,Props3) of
                                         none -> true;
                                         _ -> false
                                     end
                              end, proplist_uncompact(Defaults)),
            case lists:filter(fun(Req) ->
                                      case proplists:lookup(Req,Props3) of
                                          none -> true;
                                          _ -> false
                                      end
                              end, Required) of
                [] -> %nothing missing
                    Props4 =
                        case MakeUnique of
                            true ->
                                Keys = proplists:get_keys(Props3),
                                lists:map( fun(K) ->
                                                   proplists:lookup(K,Props3)
                                           end, Keys );
                            false ->
                                Props3
                        end,
                    lists:sort( proplists:compact(Props4++NeedDefaults) );
                Missing ->
                    {error,{invalid_datasource,{missing_properties,Missing}}}
            end
    end.

-spec validate_properties( Module :: atom(), Props :: [property()] ) ->
                                 [property()] | {error,any()}.
validate_properties(Module,Props) ->
    lists:foldl( fun(_,{error,R}) ->
                         {error,R}; % short-circuit after error
                    ({P,V},{ok,Properties}) ->
                         case Module:validate_property(P,V) of
                             ok ->
                                 {ok,[{P,V}]++Properties};
                             {ok,VProps} when is_list(VProps) ->
                                 {ok,VProps++Properties};
                             {ok,VProp} ->
                                 {ok,[VProp|Properties]};
                             {error,Reason} -> {error,Reason}
                         end
                 end, {ok,[]}, proplist_uncompact(Props) ).

proplist_uncompact(Props) ->
    lists:map( fun(P) when is_atom(P) ->
                       {P,true};
                  ({P,V}) -> {P,V}
               end, Props ).


-spec parse_ds( Tokens :: list(string()) ) ->
                      erbi_data_source() | { error, any() }.

parse_ds( [ "erbi", ":", Driver, ":" ] ) ->
    mk_ds( Driver, [], [] );
parse_ds( [ "erbi", ":", Driver, ":" | Tokens ] ) ->
    case parse_ds_props(Tokens,[]) of
        {error,Reason} -> {error,Reason};
        {Props,Args} ->
            mk_ds(Driver,Props,Args)
    end;
parse_ds( [ "erbi", ":", Driver ] ) ->
    mk_ds( Driver, [], [] );
parse_ds( [ "erbi" | _ ] ) ->
    { error, { expected, driver_name } };
parse_ds( _Tokens ) ->
    { error, { expected, erbi } }.

mk_ds( "", _Props, _Args ) ->
    { error, { expected, driver_name } };
mk_ds( Driver, Props, Args ) ->
    DriverAtom = list_to_atom(Driver),
    #erbi{driver=DriverAtom,properties=Props,args=Args}.

%%---------------------------------------------------------------
%% Parser for name-value pairs in datasource
%% uses tokens as produced by scan_ds
%%---------------------------------------------------------------
parse_ds_props( [ PropName, "=", Value | Tokens ], Props ) ->
    parse_ds_prop_sep(Tokens,[{list_to_atom(PropName),Value}|Props]);
parse_ds_props( [ "" | Tokens ], Props ) ->
    parse_ds_prop_sep(Tokens,Props);
%% bare-atom property
parse_ds_props(  [ PropName | [P|_]=Tokens ], Props )
  when (P == ":") or (P == ";") ->
    parse_ds_prop_sep(Tokens,[list_to_atom(PropName)|Props]);
parse_ds_props( [ [_] | _ ] = Tokens, Props ) ->
    parse_ds_prop_sep( Tokens,Props );
parse_ds_props( [], _ ) ->
    {error,{unexpected,end_of_data}}.

%% after each name-value pair check whether to switch to positional args
parse_ds_prop_sep( [ ";" | Tokens ], Props ) ->
    parse_ds_props( Tokens, Props );
parse_ds_prop_sep( [ ":" | Tokens ], Props ) ->
    Args = parse_ds_args( Tokens,[],[] ),
    {lists:reverse(Props),Args};
parse_ds_prop_sep( [SomethingElse|_Tokens], _Props ) ->
    {error, {unexpected, SomethingElse}};
parse_ds_prop_sep( [], Props ) ->
    {lists:reverse(Props),[]}.

%%---------------------------------------------------------------
%% Parser for positional args.  Unlike name-value, empty items are allowed
%%---------------------------------------------------------------
parse_ds_args( [ ":" | Tokens ], Accum, Args ) ->
    Arg = lists:concat(lists:reverse(Accum)),
    parse_ds_args( Tokens, [], [Arg|Args] );
parse_ds_args( [Tok|Tokens] , Accum, Args ) ->
    parse_ds_args( Tokens, [Tok|Accum], Args );
parse_ds_args( [], [], Args ) ->
    lists:reverse(Args);
parse_ds_args( [], Accum, Args ) ->
    Arg = lists:concat(lists:reverse(Accum)),
    parse_ds_args( [], [], [Arg|Args] ).

%%---------------------------------------------------------------
%% simple tokenization on :;= delimiters;
%% handles backslash-escaping and quoted strings (single or double)
%%---------------------------------------------------------------
-define(isQuote(C), ((C == $\") or (C == $\')) ). %%'
-define(isEscape(C), (C == $\\) ).
-define(isPunctuation(C), ((C == $:) or (C == $;) or (C == $=)) ).
%%find the start of string
scan_ds( [Q|Chars], Accum, Tokens )
  when ?isQuote(Q) ->
    %%if quoted string is immediately after a delimiter, accum will be empty
    Tokens1 = case Accum of
                  [] -> Tokens;
                  _ -> [Accum|Tokens]
              end,
    scan_ds_quoted( Q, Chars, [], Tokens1 );
%%process escape char
scan_ds( [Esc , Char | Chars], Accum, Tokens )
%%escape char is not special unless it precedes an escapable char
  when ?isEscape(Esc) and (?isEscape(Char) or ?isQuote(Char) or ?isPunctuation(Char)) ->
    scan_ds( Chars, [Char|Accum], Tokens );
%%punctuation character closes the current token
scan_ds( [P|Chars], Accum, Tokens )
  when ?isPunctuation(P) ->
    Tokens1 = case Accum of
                  quote_end -> Tokens;
                  _ -> [Accum|Tokens]
              end,
    scan_ds( Chars, [], [[P]|Tokens1] );
%%anything besides punctuation after a close-quote is an error
scan_ds( [_|_], quote_end, _Tokens ) ->
    {error,{expected, [delimiter,end_of_string]}};
%%normal case -- add current character to token
scan_ds( [C|Chars], Accum, Tokens ) ->
    scan_ds( Chars, [C|Accum], Tokens );
%%processing complete; all tokens collected
scan_ds( [], [], Tokens) ->
    %%each token is reversed, not just the list of tokens
    lists:reverse( lists:map( fun lists:reverse/1, Tokens ) );
%%handle end-of-quotes at end-of-string
scan_ds( [], quote_end, Tokens ) ->
    scan_ds( [], [], Tokens );
%%end of string terminates current token
scan_ds( [], Accum, Tokens ) ->
    scan_ds( [], [], [Accum|Tokens] ).

%%quoted-string handling; Quote param ensures close matches open
%%handle escapes within quotes -- only quote chars are escaped
scan_ds_quoted( Quote, [Esc,Char|Chars], Accum, Tokens )
  when ?isEscape(Esc) and (Char == Quote or ?isEscape(Char) ) ->
    scan_ds_quoted( Quote, Chars, [Char|Accum], Tokens );
%%unescaped close-quote
scan_ds_quoted( Quote, [Q|Chars], Accum, Tokens )
  when Q == Quote ->
    %%let main scanner func know a quote has ended
    scan_ds( Chars, quote_end, [Accum|Tokens] );
%%normal case; add character to quoted token
scan_ds_quoted( Quote, [Char|Chars], Accum, Tokens ) ->
    scan_ds_quoted( Quote, Chars, [Char|Accum], Tokens ).
