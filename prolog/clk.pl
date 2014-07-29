:- module(clk, [main/1]).
:- set_prolog_flag(verbose, silent).
%:- set_prolog_flag(verbose_load, normal).
%:- set_prolog_flag(verbose_file_search, true).
:- use_module(library(sweet)).

:- use julian.
:- use func.
:- use my(clk/struct).
:- use my(clk/timeline).

% edit most recent timeline file
main([edit|_]) :-
    now_mark(Mark),
    mark_file(Mark, File),
    getenv('EDITOR', Editor),
    Command =.. [Editor, '+99999', File],
    exec(Command).

% record a new mark
main([in|Words]) :-
    !,
    now_mark(Mark),
    words(Mark, Words),
    timeline_append(Mark).

% list marks
main([ls|_]) :-
    !,
    now_mark(Mark),
    mark_file(Mark, File),
    exec(tail(File)).

% clock out
main([out|_]) :-
    main([in, out]).

% handle unknown commands
main([Command|_]) :-
    !,
    format(user_error, "Unknown command: ~s~n", [Command]),
    help.
main(_) :-
    help.


%% now_mark(-Mark)
%
%  Construct a mark for the current time. Fields other than the time are
%  left empty.
now_mark(Mark) :-
    new_struct(mark, Mark),
    datetime(Mark, form_time $ now).


help :-
    writeln("Usage: clk COMMAND [ARGUMENTS]"),
    writeln("COMMAND is one of:"),
    nl,
    writeln("    in  - start working on a new topic"),
    writeln("          (arguments describe the topic)"),
    writeln("    out - stop working on the current topic"),
    nl.
