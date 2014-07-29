:- module(clk_struct, [ new_struct/2
                      , datetime/2
                      , words/2
                      ]).

:- use_module(library(sweet)).

:- use julian.

%% new_struct(+Type:atom, -Mark) is det.
%
%  Constructs a new Mark value.
new_struct(mark, mark(_Datetime,_Words)).


%% datetime(?Mark, ?Datetime)
%
%  True if Mark happened at Datetime.
datetime(mark(Datetime,_),Datetime).

%% words(?Mark, ?Words:list(atom)).
%
%  True if Mark has a note composed of Words.
words(mark(_,Words), Words).

