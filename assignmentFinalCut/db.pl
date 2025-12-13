start :-
    read_input(N),
    read_file(File, Chars),
    chars_to_words(Chars, Words),
	write('\t=== Highlighting Words ===\n\t'),
    check_conditions(Words, N), nl.


%take_input_and_process_it
read_input(N) :-
	repeat,
	write('Enter the value of N: '),
	read(N), nl,
	(   integer(N), N > 0
    ->  !
    ;   write('Invalid input, try again.'), nl,
		fail
    ).

%_Reading_file_and_returning_a_list_of_its_chars_%
%Creating_file_if_not_found
read_file(File, Chars) :- 
	write('Enter the file name: '), nl,
	read(File),
	( catch(open(File, read, Stream), _, fail)
		->	close(Stream),
			read_chars(File,Chars)
		
		;	write(' File Doesn\'t exist!'), nl,
			format('=== Creating a file with the name ~w ===', [File]), nl, 
			write_file(File),
			read_chars(File, Chars)
	).
%DRY_preparing_for_reading_file_chars
read_chars(File, Chars):-
	open(File, read, Stream),
	get_char(Stream, Char),
	process_stream(Char, Stream, Chars),
	close(Stream).


%_Cut_reading_chars_from_file_when_none_are_left_
process_stream(end_of_file, _, []) :- !.

%_Reading_file_chars
process_stream(Char, Stream, [Char|R]) :-
	get_char(Stream, NextChar),
	process_stream(NextChar, Stream, R).

%TODO
write_file(File) :-
	write('Text file content: '), nl,
	read(Text),
	open(File, write, Stream),
	write(Stream, Text), nl,
	close(Stream).


%_Concat_chars_from_the_file_into_full_words_
chars_to_words([], []).

chars_to_words(Chars, [Word|Words]) :-
    drop_separators(Chars, Rest),
    Rest \= [],
    take_word(Rest, Word, Remaining),
    chars_to_words(Remaining, Words).


separator(' ').

drop_separators([], []).
drop_separators([C|T], Rest) :-
    separator(C), !,
    drop_separators(T, Rest).
drop_separators(Rest, Rest).

take_word([], [], []).
take_word([C|T], [], [C|T]) :-
    separator(C), !.
take_word([C|T], [C|W], Rest) :-
    \+ separator(C), !,
    take_word(T, W, Rest).

%KISS_First_condition_checking_if_the_word_starts_with_a_vowel
vowel('i').	
vowel('o').	
vowel('u').	
vowel('e').	
vowel('v').
vowel('I').	
vowel('O').	
vowel('U').	
vowel('E').	
vowel('V').

starts_with_vowel([First|_]) :-
    vowel(First).
	
check_conditions([Word], N).

check_conditions([Word|Rest], N) :-
    length(Word, L),
	
    (	L >= N,
		starts_with_vowel(Word)
		
    ->  write('{'),
        write_word(Word),
		write('}'),
		write(' ')
		
    ;   write_word(Word),
		write(' ')
    ),
	check_conditions(Rest, N).

write_word([]).
write_word([H|T]) :-
    write(H),
    write_word(T).