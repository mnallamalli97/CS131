%# Given by TA Slides
all_unique([]).
all_unique([H|T]) :- member(H, T), !, fail.
all_unique([H|T]) :- all_unique(T).

%# Given by TA Slides
elements_between(List, Min, Max) :-
	maplist(between(Min,Max), List).

%# Given by TA Slides
unique_list(N, ROW):-
	length(ROW, N),
	elements_between(ROW, 1, N),
	all_unique(ROW).

%# Given by TA Slides
unique_list2(_, []).
unique_list2(N, [Row | Remaining_Rows]) :-
	length(Row,N),
	fd_domain(Row, 1, N),
	fd_all_different(Row),
	unique_list2(N, Remaining_Rows).

%# Given by TA Slides
plain_limitations(T, N) :-
	maplist(unique_list(N), T).

% need to count a single Row
check_single_row([Row | Remaining_Rows], Col) :-
	check_single_row(Remaining_Rows, Row, 1, Col).

%# Given by SWI Prolog Implementation 
transpose([[]|_], []).
transpose(Matrix, [Row|Rows]) :- transpose_1st_col(Matrix, Row, RestMatrix),
                                 transpose(RestMatrix, Rows).
transpose_1st_col([], [], []).
transpose_1st_col([[H|T]|Rows], [H|Hs], [T|Ts]) :- transpose_1st_col(Rows, Hs, Ts).


check_length([], _).
check_length([Row | Col], N) :-
	length(Row, N).
	check_length(Col, N).

tower(N, T, counts(TOP, BOTTOM, LEFT, RIGHT)) :-
	length(T, N),
	check_length(T, N),

	unique_list2(N, T),
	transpose(T, Cols),
	unique_list2(N, Cols),

	length(TOP, N), length(BOTTOM, N), length(LEFT, N), length(RIGHT, N),

	check_counts(T, Cols, TOP, BOTTOM, LEFT, RIGHT).


%# 	check_counts(T, Cols).
check_counts(T, Cols, TOP, BOTTOM, LEFT, RIGHT) :-
	check_row_col(T, LEFT),
	reverse_row_col(T, T_reverse),
	check_row_col(T_reverse, RIGHT),
	check_row_col(Cols, TOP),
	reverse_row_col(Cols, Cols_reverse),
	check_row_col(Cols_reverse, BOTTOM).


check_row_col([], []).
check_row_col([[Row | Remaining_Rows] | T], [Col | Remaining_Cols]) :-
	check_single_row([Row | Remaining_Rows], Col),
	check_row_col(T, Remaining_Cols).



% base case, this will return the answer for the count of the Row
check_single_row([], Row, C, C).


%originally, wanted to have this functionality in the same function using the if conditional ;
%% I was not able to get it to work, so had to rework the implementation to take in The curr col.
check_single_row([Row | Remaining_Rows], Peak, C, Col) :-
	/*
	Row #< Peak,
	check_single_row(Remaining_Rows, C, Peak);

	Row #> Peak,
	NewC #= C + 1,
	check_single_row(Remaining_Rows, NewC, Row).
	*/

	Row #> Peak,
	NewCount is C+1,
	check_single_row(Remaining_Rows, Row, NewCount, Col).


check_single_row([Row | Remaining_Rows], Peak, C, Col) :-
	Row #< Peak,
	check_single_row(Remaining_Rows, Peak, C, Col).



reverse_row_col([],[]).
reverse_row_col([Row | Remaining_Rows], [RowRev | Remaining_RowRev]) :-
	reverse(Row, RowRev),
	reverse_row_col(Remaining_Rows, Remaining_RowRev).

	

plain_tower(N, T, counts(TOP, BOTTOM, LEFT, RIGHT)) :-
	length(T, N),

	length(TOP, N),
	length(BOTTOM, N),
	length(LEFT, N),
	length(RIGHT, N),

	plain_limitations(T, N),
	transpose(T, Cols),
	plain_limitations(Cols, N),

	check_counts(T, Cols, TOP, BOTTOM, LEFT, RIGHT).


%# stats
speedup(X) :-
	get_stats(Normal, tower(4, T1, C)),
	get_stats(Plain, plain_tower(4, T2, C)),
	X is Normal/Plain.

get_stats(X, F) :-
	statistics(cpu_time, [_|X]), F, !.

ambiguous(N, C, T1, T2) :-
	tower(N, T1, C),
	tower(N, T2, C),

	T1 \= T2.

