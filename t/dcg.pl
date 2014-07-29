:- use_module(library(clk/dcg)).
:- use_module(library(clk/struct)).

% define helper predicates here

:- use_module(library(tap)).

'parsing antiquated lines' :-
    phrase(mark(M),`foo@ex.us\t2014-07-29T14:47:59.36560896Z\t\thello world\n`,R),
    M == mark(datetime(56867, 53279365608960), [hello, world]),
    R == [].


'parsing modern lines' :-
    phrase(mark(M),`2014-07-29T14:47:59.36560896Z\thello world\n`,R),
    M == mark(datetime(56867, 53279365608960), [hello, world]),
    R == [].


'generating modern lines' :-
    new_struct(mark, M),
    datetime(M,datetime(56867, 53868415152128)),
    words(M,[hello,world]),
    phrase(mark(M), Codes),
    Codes == `2014-07-29T14:57:48.415152128Z\thello world\n`.
