#!/usr/bin/env swipl -t halt -g main -q -s
infer( [tech,team],    entry( 'tech team',    [gsg, meeting] ) ).
infer( [time,tracker], entry( 'time tracker', [gsg         ] ) ).
infer( [gitc,support], entry( 'user support', [gsg, gitc   ] ) ).
infer( [Changeset], entry( '', [gsg,Project,Name] ) ) :-
    changeset( Changeset, Project, Name ).
infer( [review,Changeset], entry( 'code review', [gsg,Project,Name] ) ) :-
    changeset( Changeset, Project, Name ).
infer( Words, entry( Subject, Tags ) ) :-
    infer_tags( Words, Tags ),
    subtract( Words, Tags, SubjectWords ),
    concat_atom( SubjectWords, ' ', Subject ).

infer_tags( Words, Tags ) :-
    findall( Ts, ( member( W, Words ), tags(W, Ts) ), NestedTags ),
    flatten( NestedTags, TagList ),
    list_to_set( TagList, Tags ).
    
tags( jjg, jjgames ).
tags( W, [W]) :- member( W, [ jjgames, gsg, scs, conserve ] ).
tags( Project, [ Client, Project ] ) :- project( Client, Project ).
tags( Changeset, [ gsg, Project, Name ] ) :-
    changeset( Changeset, Project, Name ).
tags( Bug, [Bug] ) :- bugzilla(Bug).

% projects for each client
project( gsg, P ) :- member( P, [ 'gsg-arc', zions, gitc ] ).
project( scs, P ) :- member( P, [ wcit, qa ] ).

% GSG's changeset designation (like project#e1234b)
changeset( C, Project, Name ) :- concat_atom( [ Project, Name ], '#', C ).

% Bugzilla bug number
bugzilla(Bug) :-
    concat_atom( [b, X], Bug ),
    atom_number( X, N ),
    N > 0.

% find structure for the command line arguments
main( Words ) :-
    infer( Words, entry( Subject, Tags ) ),
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
    format('1..~w~n', [6]),
    ok( infer([tech,team],entry('tech team', [gsg,meeting]))),
    ok( infer([gsg,email],entry('email', [gsg]))),
    ok( infer(['zions#e1234'],entry('', [gsg,zions,e1234]))),
    ok( infer([review, 'zions#e1234'],entry('code review', [gsg,zions,e1234]))),
    ok( infer([scs,wcit,fixing,errors],entry('fixing errors', [scs,wcit]))),
    ok( infer([jjg,b690],entry('', [jjgames,b690]))),
    true.
