main :-
    open('input/day2.in', read, F),
    read_line(F, Result),
    close(F),
    write(Result), nl.

read_line(F, Total) :- 
   read_line_to_codes(F, Line),
   read_line2(F, Line, Total).

read_line2(_F, end_of_file, 0) :-
    true.
read_line2(F, [OCode, 32, MCode], Total) :-
    char_to_lowercase_atom(OCode, O),
    char_to_lowercase_atom(MCode, M),
    round(O, M, Score),
    read_line(F, Next),
    Total is Next + Score.

char_to_lowercase_atom(Code, Atom) :-
    LCaseCode is Code + 0'a - 0'A,
    atom_chars(Atom, [LCaseCode]).

wins(a, b).
wins(b, c).
wins(c, a).

shape_score(a, 1).
shape_score(b, 2).
shape_score(c, 3).

part1(_O, x, a).
part1(_O, y, b).
part1(_O, z, c).

part2(O, x, Translated) :- wins(Translated, O).
part2(O, y, O).
part2(O, z, Translated) :- wins(O, Translated).

round(O, M, Score) :-
    part2(O, M, MTranslated),
    round_translated(O, MTranslated, Score).

round_translated(O, M, Score) :-
    wins(O, M),
    shape_score(M, ShapeScore),
    Score is 6 + ShapeScore.
round_translated(M, M, Score) :-
    shape_score(M, ShapeScore),
    Score is 3 + ShapeScore.
round_translated(_O, M, Score) :-
    shape_score(M, Score).
