#!/usr/bin/env pl
:- set_prolog_flag(verbose, silent).
:- user:asserta(file_search_path(library,prolog)).

:- use_module(library(clk), [main/1]).
