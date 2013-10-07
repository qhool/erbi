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
-export([connect/3,parse_data_source/1]).
-include("erbi.hrl").

%% --------------------------------------
%% @doc Connect to a database.
%% 
%% <ul><li>DataSource  - DB connect term or string "erbi:Driver:params"</li>
%% <li>Username</li>
%% <li>Password</li>
%% </ul>
%% @end
%% --------------------------------------
-spec connect( DataSource :: unicode:chardata() | erbi_data_source(),
               Username :: unicode:chardata() ,
               Password :: unicode:chardata() ) -> 
                     { ok, erbi_connection() } | { error, any() }.
connect( DataSource, UserName, Password ) ->                    
    { error, "not implemented" }.

%% --------------------------------------
%% @doc Parse data source string.
%%
%% Takes a data source descriptor in the form "erbi:driver:arg=val;arg=val[...]"
%% and returns an erbi_data_source() value.
%% @end
%% --------------------------------------
-spec parse_data_source( DataSource :: unicode:chardata() ) ->
                               { ok, erbi_data_source() } | { error, any() }.


parse_data_source( DataSource ) ->
    Charlist = unicode:characters_to_list(DataSource),
    Tokens=scan_ds(Charlist,[],[]),
    parse_ds( Tokens ).

%% @headerfile "erbi.hrl"

%%==== Internals ====%%

-spec parse_ds( Tokens :: list(string()) ) ->
                      { ok, erbi_data_source() } | { error, any() }.

parse_ds( [ "erbi", ":", Driver, ":" | Tokens ] ) ->
    DriverAtom = list_to_atom(Driver),
    case parse_ds_props(Tokens,[]) of
        {error,Reason} -> {error,Reason};
        {Props,Args} ->
            #erbi{driver=DriverAtom,properties=Props,args=Args}
    end;
parse_ds( [ "erbi", ":" | _ ] ) ->
    { error, { expected, driver_name } };
parse_ds( Tokens ) ->
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
parse_ds_prop_sep( [SomethingElse|Tokens], Props ) ->
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
scan_ds( [_|_], quote_end, Tokens ) ->
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


