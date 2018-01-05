%
% Copyright 2018 KasperskyLab
%
% Permission is hereby granted, free of charge, to any person obtaining
% a copy of this software and associated documentation files (the "Software"),
% to deal in the Software without restriction, including without limitation
% the rights to use, copy, modify, merge, publish, distribute, sublicense,
% and/or sell copies of the Software, and to permit persons to whom
% the Software is furnished to do so, subject to the following conditions:
%
% The above copyright notice and this permission notice
% shall be included in all copies or substantial portions of the Software.
%
% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
% IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
% DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
% TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
% WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%
-module(epolymorph_interface_pt).
-author('eldar.kononov@kaspersky.com').

-export([parse_transform/2]).

find_callbacks(Forms) ->
  [{Name,Arity} || {attribute,_,callback,{{Name,Arity},_}} <- Forms].

generate_vars(Arity, Line) ->
  [{var, Line, list_to_atom("Var_" ++ integer_to_list(I))} || I <- lists:seq(2, Arity)].

generate_function({Name, Arity}, Line, InterfaceMod) ->
  Vars = generate_vars(Arity, Line),

  F =
    {function,Line,Name,Arity,
      [{clause,Line,
        [{tuple,Line,[{atom,Line,InterfaceMod},{var,Line,'Module'},{var,Line,'Instance'}]}|Vars],
          [],
          [{call,Line,
            {remote,Line,
              {var,Line,'Module'},
              {atom,Line,Name}},
            [{var,Line,'Instance'}|Vars]}]}]},

  {F, Line + 1}.

create_function_clause(Line, InterfaceMod) ->
  {function,Line,create,2,
    [{clause,Line,
      [{var,Line,'Mod'},{var,Line,'Args'}],
        [],
        [{call,Line,
          {remote,Line,{atom,Line,epolymorph},{atom,Line,create}},
          [{atom,Line,InterfaceMod},{var,Line,'Mod'},{var,Line,'Args'}]}]}]}.

add_exports({Forms, LastLine}, Callbacks) ->
  {Forms ++ [{attribute, LastLine, export, [{create,2}|Callbacks]}], LastLine + 1}.

add_functions({Forms, LastLine}, Callbacks, InterfaceMod) ->
  lists:foldl(
    fun(Callback, {Forms1, LastLine1}) ->
      {Function, LastLine2} = generate_function(Callback, LastLine1, InterfaceMod),
      {Forms1 ++ [Function], LastLine2}
    end,
    {Forms, LastLine},
    Callbacks).

add_callbacks(Forms, Callbacks, InterfaceMod) ->
  {Forms1, [{eof,LastLine1}]} = lists:split(length(Forms) - 1, Forms),
  {Forms2, LastLine2} = add_exports({Forms1,LastLine1}, Callbacks),
  {Forms3, LastLine3} = add_functions({Forms2, LastLine2}, Callbacks, InterfaceMod),

  Forms3 ++ [create_function_clause(LastLine3, InterfaceMod), {eof, LastLine3 + 1}].

find_interface_name(Forms) ->
  hd([A || {attribute,_,module,A} <- Forms]).

parse_transform(Forms, _Opts) ->
  add_callbacks(Forms, find_callbacks(Forms), find_interface_name(Forms)).
