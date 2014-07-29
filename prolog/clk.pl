:- module(clk, [main/1]).
:- set_prolog_flag(verbose, silent).
%:- set_prolog_flag(verbose_load, normal).
%:- set_prolog_flag(verbose_file_search, true).
:- use_module(library(sweet)).

:- use julian.
:- use func.
:- use my(clk/struct).
:- use my(clk/timeline).

main([in|Words]) :-
    !,
    % construct a new mark
    new_struct(mark, Mark),
    datetime(Mark, form_time $ now),
    words(Mark, Words),

    % append it to the timeline
    timeline_append(Mark).
main([out|_]) :-
    main([in, out]).
main([Command|_]) :-
    !,
    format(user_error, "Unknown command: ~s~n", [Command]),
    help.
main(_) :-
    help.


help :-
    writeln("Usage: clk COMMAND [ARGUMENTS]"),
    writeln("COMMAND is one of:"),
    nl,
    writeln("    in  - start working on a new topic"),
    writeln("          (arguments describe the topic)"),
    writeln("    out - stop working on the current topic"),
    nl.
