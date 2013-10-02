%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
-ifndef(ERBI_HRL).

-type erbi_connection() :: {erbi_connection, erbi_connection:conn_private()}.
% Opaque connection handle

-type erbi_statement() :: {erbi_statement, erbi_statement:stmt_private()}.
% Opaque statement handle

-type erbi_bind_values() :: [erbi_bind_value()] | [erbi_bind_value_named()].
-type erbi_bind_value() :: erbi_bind_value_typed() | erbi_bind_value_untyped().
-type erbi_bind_value_typed() :: {erbi_value_type(), any()}.
-type erbi_bind_value_untyped() :: any().
-type erbi_bind_value_named() :: erbi_bind_value_named_typed() | erbi_bind_value_named_untyped().
-type erbi_bind_value_named_typed() :: {erbi_identifier(), erbi_value_type(), any()}.
-type erbi_bind_value_named_untyped() :: {erbi_identifier(), any()}.
-type erbi_identifier() :: atom() | string() | binary().
-type erbi_value_type() :: atom().

-record(erbi,
        { driver :: atom(),
          properties = [] :: [proplist:property()],
          args = [] :: [any()]
        }).
-type erbi_data_source() :: #erbi{}.
-endif.
