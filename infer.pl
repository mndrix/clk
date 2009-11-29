#!/usr/bin/env swipl -t halt -g main -q -s

infer( [tech,team],        'tech team',    [gsg, meeting] ).
infer( [time,tracker],     'time tracker', [gsg         ] ).
infer( [gitc,support],     'user support', [gitc,gsg    ] ).
infer( [svkc,support],     'user support', [gsg,svkc    ] ).
infer( [review,Changeset], 'code review',  Tags ) :-
    changeset( Changeset, Project, Name ),
    sort([gsg,Project,Name], Tags).
infer( Words, Subject, Tags ) :-
    infer_(Words, [], [], Subject, Tags).

% accumulating helper for infer/3
infer_( [H|Tail], S0, T0, Subject, Tags ) :-
    expand( H, S, T ),
    !,
    append( S0, S, S1 ),
    append( T0, T, T1 ),
    infer_( Tail, S1, T1, Subject, Tags ).
infer_( [], S0, T0, Subject, Tags ) :-
    concat_atom( S0, ' ', Subject ),
    sort( T0, Tags ).

% expand single words into subject words and tags
expand( jjg, [], [jjgames] ).
expand( Changeset, [], [gsg,Project,Name] ) :-
    changeset( Changeset, Project, Name ).
expand( Client, [], [Client] ) :-
    member( Client, [ jjgames, gsg, scs, conserve ] ).
expand( Project, [], [Client,Project] ) :-
    project( Client, Project ).
expand( Bug, [], [Bug] ) :-
    concat_atom( [b, X], Bug ),
    catch( atom_number( X, N ), _, fail ),
    N > 0.
expand( W, [W], [] ).

% each client's projects
project( gsg, P ) :-
    member( P, [ arc,tla,cd,pma,zions,'tax-lien','tax-cbs','tva-note','fmac-ebb',eventum,'yield-auction',foreclosures,'fmac-debt','gsg-catalyst','gsg-shared','gsg-db-schema','mellon-gsl','time-tracker','gsg-homepage',gitc,'zions-sso','zions-new-account','gsg-catalyst-base' ] ).
project( scs, P ) :-
    member( P, [ wcit, qa ] ).

% GSG's changeset format (e.g. tax-cbs#e12345)
changeset( Changeset, Project, Name ) :-
    concat_atom( [ Project, Name ], '#', Changeset ).

% find structure for the command line arguments
main( Words ) :-
    infer( Words, Subject, Tags ),
    format('-m "~w" -t ~w~n', [Subject, Tags] ).

% boilerplate for extracting command-line arguments
main :-
    current_prolog_flag( argv, Argv ),
    append( _, [--|Args], Argv ),
    !,
    main( Args ).

% tests
ok(Goal) :- Goal, format('ok - ~w~n', [Goal]), !.
ok(Goal) :- format('not ok - ~w~n', [Goal]), !.
test :-
    format('1..~w~n', [7]),
    ok( infer([tech,team],             'tech team',     [gsg,meeting    ])),
    ok( infer([gsg,email],             'email',         [gsg            ])),
    ok( infer(['zions#e1234'],         '',              [e1234,gsg,zions])),
    ok( infer([review, 'zions#e1234'], 'code review',   [e1234,gsg,zions])),
    ok( infer([scs,wcit,fixing,errors],'fixing errors', [scs,wcit       ])),
    ok( infer([jjg,b690],              '',              [b690,jjgames   ])),
    % atom_number might throw an exception, be sure it's caught
    ok( infer([boo],                   'boo',           [               ])),
    true.
