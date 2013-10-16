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
-export([connect/3,connect/1,normalize_data_source/1,parse_data_source/1]).
-include("erbi.hrl").
-include("erbi_private.hrl").

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
connect( DataSource, UserName, Password ) ->                    
    { error, "not implemented" }.

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

%% normalize_data_source( DataSource ) when is_list(DataSource) ->
%%     case parse_data_source(DataSource) of
%%         {error,_}=E -> E;
%%         DS -> normalize_data_source(DS)
%%     end;
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

%% @headerfile "erbi.hrl"

%%==== Internals ====%%

-spec get_driver_module( erbi_data_source() ) -> atom().

get_driver_module( #erbi{driver=DriverAtom} ) ->
    Module = list_to_atom("erbdrv_" ++ atom_to_list(DriverAtom)),
    {module,Module} = code:ensure_loaded(Module),
    Module.

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
    
-spec normalize_args( Module :: atom(), Args :: [any()] ) ->
                            undefined | {error,any()} | term().
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
                    lists:sort( proplists:compact(Props3++NeedDefaults) );
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
                                 {ok,Properties++[{P,V}]};
                             {ok,VProps} ->
                                 {ok,Properties++VProps};
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

parse_ds( [ "erbi", ":", Driver, ":" | Tokens ] ) ->
    DriverAtom = list_to_atom(Driver),
    case parse_ds_props(Tokens,[]) of
        {error,Reason} -> {error,Reason};
        {Props,Args} ->
            #erbi{driver=DriverAtom,properties=Props,args=Args}
    end;
parse_ds( [ "erbi", ":" | _ ] ) ->
    { error, { expected, driver_name } };
parse_ds( _Tokens ) ->
    { error, { expected, erbi } }.


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


