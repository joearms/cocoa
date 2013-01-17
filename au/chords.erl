-module(chords).

-compile(export_all).

chords() ->
    [major, minor, maj7, min7, 7, 6, min6, sus4, 9, min9, dim, cdim7, c7b5, 'c7#5', aug,
     m7f5].

notes(major)  -> [1,5,8]; 
%% C Major C,Cmaj,CM
%% 1,3,5

notes(minor)  -> [1,4,8]; 
%% 1,3b,5
%% Cm, Cminor,Cmin

notes(maj7) -> [1,5,8,12];
%% Cmaj7, CM7
%% C,E,G,B

notes(min7) -> [1,4,8,11];
%% Cm7 Cminor7, Cmin7
%% 1,3b,5,7b
%% C,Eb,G,Bb

notes(7)    -> [1,5,8,11]; 
%% 1,3,5,7b
%% C7, C dominant 7, Cdom7
%% C,E,G,Bb

notes(6)    -> [1,5,8,10];
notes(min6) -> [1,4,8,10];
notes(sus4) -> [1,6,8];
notes(9)    -> [1,5,8,15];
notes(min9) -> [1,4,8,15];

notes(dim)  -> [1,4,7];
%% Cdim C,Eb,Fb  1,3b,5b
%% Diminished

notes(cdim7) -> [1,4,7,10];
%% C diminished 7 1,3b,5b,6
%% C,D#,F#,A

notes(c7b5) -> [1,5,7,11];
%% C dominant 7, flat fifth
%% 1,3,5b,7b

notes('c7#5') -> [1,5,9,11];
%% C dominant 7, sharp fifth, C7+5
%% C,E,G# A#
%% 1,3,5#,7

notes(aug)  -> [1,5,9];
notes(m7f5) -> [1,4,7,11]. %%"min7 flat 5"

%% http://www.onlinepianist.com/chords/

    


