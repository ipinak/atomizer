%% Copyright (c) 2007, Kevin A. Smith<kevin@hypotheticalabs.com>
%% 
%% All rights reserved.
%% 
%% Redistribution and use in source and binary forms, with or without 
%% modification, are permitted provided that the following 
%% conditions are met:
%% 
%% * Redistributions of source code must retain the above copyright notice, 
%% this list of conditions and the following disclaimer.
%% 
%% * Redistributions in binary form must reproduce the above copyright 
%% notice, this list of conditions and the following disclaimer in the 
%% documentation and/or other materials provided with the distribution.
%% 
%% * Neither the name of hypotheticalabs.com nor the names of its 
%% contributors may be used to endorse or promote products derived from 
%% this software without specific prior written permission.
%% 
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
%% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT 
%% OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, 
%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT 
%% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, 
%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY 
%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR 
%% TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF 
%% THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
%% DAMAGE.

-module(fetcher).

-export([fetch/1, fetch/2]).

-define(content_type, "content-type").

init() ->
    inets:start().

fetch([_|_]=Url) ->
    init(),
	fetch(Url, []).

fetch(Url, Headers) when is_list(Url) andalso is_list(Headers) ->
    init(),
	case httpc:request(get, {Url, Headers}, [], []) of
		{ok, {Status, RespHeaders, Body}} -> 
			{ok, extract_content_type(RespHeaders), 
             Status, RespHeaders, Body};
		{error, Reason} -> 
			{error, Reason}
	end.

extract_content_type(Headers) ->
	case extract_content_type(Headers, []) of
		[H|_] -> H;
		true  -> "none"
	end.

extract_content_type([{?content_type, Value} | T], Accum) ->
    %% We found some element that has content-type, so it's
    %% parsing timeee...
    case string:rstr(Value, ";") - 1 of 
        BreakPos when BreakPos > 0 ->
            extract_content_type(T, [string:sub_string(
                                       Value, 1, BreakPos) | Accum]);
        _ ->
            extract_content_type(T, [Value|Accum])
    end;
extract_content_type([_ | T], Accum) ->
    %% Moving to the next element
    extract_content_type(T, Accum);
extract_content_type([], Accum) ->
    Accum.

