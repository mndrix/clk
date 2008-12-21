/*
 * this script takes a list of words on the command-line and infers structure
 * based on various rules.  After being compiled, it's designed to be used
 * as a 'pre-in' hook.
 */

/******************* business rules ****************************/

% how do we determine the client?
client(jjgames) :- word(jjg); word(jjgames).
client(scs)     :- word(scs);   scs_project.
client(gsg)     :- word(gsg);   gsg_project.
client(ndrix)   :- word(ndrix); ndrix_project.

% how do we determine the project?
project(wcit)       :- word(esr); word(mis).
project(research)   :- word(blogs).
project(P) :- word(P), member( P, [
    accounting,
    systems,
    svkc,
    qa
]).
project(P) :- word(Changeset), changeset( P, _Id, Changeset ).

% a list of client-specific projects
ndrix_project :- project(P), member( P, [ systems, research, accounting ]).
scs_project   :- project(P), member( P, [ wcit, qa ]).
gsg_project   :- project(P), member( P, [
    'gsg-arc',
    'tax-cbs',
    'svkc'
]).

% how do we determine the ticket number?
ticket(Ticket) :- word(Changeset), changeset( _Project, Ticket, Changeset ).
ticket(Bug)    :-  % b1234, for example
    word(BNumbers),
    concat_atom( [ b, Bug ], BNumbers ),
    numeric_atom(Bug).

% how do we determine the message?
message('conference call') :- word(call), client(C), C \= ndrix.
message('reading blogs')   :- word(blogs).
message('user support')    :- project(svkc), word(support).
message(Message) :-
    b_getval( unused_words, UnusedWords ),
    length( UnusedWords, Length ),
    Length > 0,
    concat_atom( UnusedWords, ' ', Message ).

% define how a changeset looks
changeset( Project, Id, Changeset ) :-
    concat_atom( [ Project, Id ], '#', Changeset ).


/******************* helper rules ****************************/

% convert command line arguments into word assertions
assert_words([]).
assert_words([ H | T ]) :- assert(w(H)), assert_words(T).

% does an atom consist entirely of numbers?
numeric_atom(A) :- atom_chars(A, L), numeric_list(L).
numeric_list([]).
numeric_list([H|T]) :-
    atom_chars( '0123456789', Numbers ),
    member( H, Numbers ),
    numeric_list(T).

% if the word exists and we end up using it, remove it from unused_words list
word(Word) :-
    w(Word),
    b_getval( unused_words, UnusedWords0 ),
    delete( UnusedWords0, Word, UnusedWords ),
    b_setval( unused_words, UnusedWords ).

% outputs a single "key: value" line
print_kv(Key) :-
    call( Key, X ),
    format('~w: ~w~n', [ Key, X ] ).
print_kv(_).

% the main program
main :-
    current_prolog_flag( argv, [ _ | Argv ] ),
    nb_setval( unused_words, Argv ),
    assert_words(Argv),
    print_kv( client ),
    print_kv( project ),
    print_kv( ticket ),
    print_kv( message ).
