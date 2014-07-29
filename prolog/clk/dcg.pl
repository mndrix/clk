:- module(clk_dcg, [ mark//1
		   ]).
:- use_module(library(dcg/basics)).
:- use_module(library(sweet)).

:- use delay.
:- use julian.
:- use my(struct).

%% mark(?Mark)//
%
%  True if Mark is described by a tab separated line.
mark(Mark) -->
    mark_(Mark),
    !.

mark_(Mark) -->
    % modern format
    { new_struct(mark, Mark) },
    { datetime(Mark, Datetime) },
    { words(Mark, Words) },
    rfc3339(Datetime),
    separator,
    words(Words),
    newline.
mark_(Mark) -->
    % antiquated format
    { new_struct(mark, Mark) },
    { datetime(Mark, Datetime) },
    { words(Mark, Words) },
    username(_),
    separator,
    rfc3339(Datetime),
    separator,
    separator,
    words(Words),
    newline.


%% separator(?Char).
%
%  Char is the code that separates fields.
separator(0'\t).

%% newline(?Char).
%
%  Char is the newline character.
newline(0'\n).


%% separator//
%
%  True if the next character is a field separator.
separator -->
    [Char],
    { separator(Char) }.


%% newline//
%
%  True if the next character is a newline.
newline -->
    [Char],
    { newline(Char) }.


%% field(?Field:codes)
%
%  Field is a string containing neither tab nor newline characters.
field([]) -->
    peek(Char),
    { when(ground(Char), once(separator(Char);newline(Char))) }.
field([Char|Rest]) -->
    [Char],
    field(Rest).


%% peek(?Code)//
%
%  True if code is the next character code, without consuming it.
peek(Char, Codes, Codes) :-
    Codes = [Char|_].


%% username(?User:codes)
%
%  Antiquated field, contents ignored.
username(_) -->
    field(_).


%% rfc3339(?Datetime)
%
%  True for an RFC 3339 value representing Datetime.
rfc3339(Datetime) -->
    ( { var(Datetime) } ->
        field(Codes),
        { form_time(rfc3339(Codes), Datetime) }
    ; { otherwise } ->
        { form_time(rfc3339(Codes), Datetime) },
        { format(codes(WithZ), "~sZ", [Codes]) },
        field(WithZ)
    ).


%% words(?Words:list(atom))
%
%  Space-separated list of words. Accepts leading and trailing spaces as
%  well as repeated, internal spaces. Generates single internal spaces
%  without leading or trailing whitespace.
words(Words) -->
    whites, % describe leading spaces
    words_(Words),
    whites. % describe trailing spaces

words_([]) -->
    [].
words_([Word|Words]) -->
    { delay(atom_codes(Word,Codes)) },
    nonblanks(Codes),
    ( {var(Words)} ->
        words(Words)
    ; {Words=[]} ->
        []
    ; {otherwise} ->
        ws,
        words_(Words)
    ).


%% ws//
%
%  Consumes one or more spaces, generates a single space.
ws([0'\s|Rest], Rest0) :-
    ( var(Rest) ->
        Rest = Rest0
    ; otherwise ->
        once( ws(Rest,Rest0); Rest=Rest0 )
    ).
