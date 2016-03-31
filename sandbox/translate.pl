translate(_, [], []).

% D = [(mouse, maus), (horse, pferd), (dog, hund)]
%   translate(D, [dog, horse], T).
% computes:
%   T = [hund, pferd].
translate(D, [ SourceWord | Rest ], Translated) :-
    member( (SourceWord, TranslatedWord), D),
    translate(D, Rest, TranslatedRest),
    append( [TranslatedWord], TranslatedRest, Translated).
