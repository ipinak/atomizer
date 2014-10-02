# General

This is forked version of atomizer library with some additional
improvements. The original can be found at:
https://code.google.com/p/atomizer/

# Description

atomizer fetches and parses RSS feeds.

# Compile

    rebar get-deps
    rebar compile

# Test

## Automated testing

    $ rebar eunit

## Manual testing

You can manually test it by following the steps:

    $ rebar shell
    ==> atomizer (shell)
    Erlang/OTP 17 [erts-6.1] [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]
    1> nl(atomizer).
       abcast  
    2> atomizer:parse_url("http://droid-toolbox.com/feed.atom").

# License

Copyright (c) 2007, Kevin A. Smith<kevin@hypotheticalabs.com>

All rights reserved.

Redistribution and use in source and binary forms, with or without 
modification, are permitted provided that the following 
conditions are met:

\* Redistributions of source code must retain the above copyright notice, 
this list of conditions and the following disclaimer.

\* Redistributions in binary form must reproduce the above copyright 
notice, this list of conditions and the following disclaimer in the 
documentation and/or other materials provided with the distribution.

\* Neither the name of the hypotheticalabs.com nor the names of its 
contributors may be used to endorse or promote products derived from 
this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT 
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, 
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT 
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, 
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY 
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR 
TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF 
THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
DAMAGE.
