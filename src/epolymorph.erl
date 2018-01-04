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
-module(epolymorph).
-author('eldar.kononov@kaspersky.com').

-export([create/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

create(Mod, Args) ->
    case catch Mod:epolymorph_create(Args) of
        {ok, Instance} ->
            {ok, {Mod, Instance}};

        {'EXIT', {undef, [{Mod,epolymorph_create,[_],[]}|_]}} ->
            {error, {Mod, must_implement, epolymorph_instance_spec}};

        R ->
            R
    end.

%============
% EUnit tests
%============
-ifdef(TEST).

spec_not_implemented_test() ->
    ?assertMatch({error, {gen_server,must_implement,epolymorph_instance_spec}}, ?MODULE:create(gen_server, [])).

-endif.

% vim:ts=4:expandtab:sw=4
