:- module(clk_timeline, [ timeline_append/1
                        , mark_file/2
                        , most_recent_file/1
                        ]).
:- use_module(library(sweet)).

:- use func.
:- use julian.
:- use my(dcg).
:- use my(struct).


%% timeline_append(+Mark) is det.
%
%  Append a record for this mark to the appropriate timeline file.
timeline_append(Mark) :-
    mark_file(Mark, TimelineFile),
    open(TimelineFile, append, Stream),
    cleanup(close(Stream)),

    phrase(mark(Mark), Line),
    format(Stream, "~s", [Line]).


%% most_recent_file(-File) is det.
%
%  True if File is the most recent timeline file.
most_recent_file(File) :-
    now_mark(Mark),
    mark_file(Mark, File).


%% mark_file(+Mark, -File:atom) is det.
%
%  True if Mark should be stored in File.
mark_file(Mark, File) :-
    form_time(unix(Epoch), datetime $ Mark),
    format_time(atom(Relative), "%Y-%m.txt", Epoch),
    timeline_directory(Dir),
    format(atom(File), "~s/~s", [Dir,Relative]).


%% timeline_directory(Directory)
%
%  True if Directory is where timeline data is stored.
:- dynamic timeline_dir/1.
timeline_directory(Dir) :-
    timeline_dir(Dir),
    !.
timeline_directory(Dir) :-
    % create the directory, the first time, if necessary
    format(atom(Dir), "~s/.clkq/timeline", [getenv $ 'HOME']),
    make_directory_path(Dir),
    assert(timeline_dir(Dir)).
