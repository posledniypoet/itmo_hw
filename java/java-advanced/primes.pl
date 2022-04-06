is_prime(N, D) :- D * D > N, !.
is_prime(N, D) :-
 N mod D > 0,
 D1 is D + 6,
 is_prime(N, D1).

composite(P) :- \+ prime(P).

prime(2) :- !.
prime(3) :- !.
prime(5) :- !.
prime(7) :- !.
prime(P) :- prime_table(P), !.
prime(P) :-
 P > 10,
 P mod 2 > 0,
 P mod 3 > 0,
 is_prime(P, 5),
 is_prime(P, 7),
 assert(prime_table(P)).

prime_divisors(1, []) :- !.

prime_divisors(N, L) :-
    number(N),
    do_list(N, L, 2), !.

prime_divisors(N, L) :-
    count(L, N, 1).


count([], 1, Prev) :- !.
count([D | T], Res, Prev) :-
 D >= Prev,
 count(T, Res1, D),
 Res is Res1 * D.

do_list(1, [], _) :- !.
do_list(N, [D | T], D) :-
 0 =:= N mod D,
 R is div(N, D), !,
  do_list(R, T, D).

do_list(N, L, D) :-
  next(N, D, D1),
  do_list(N, L, D1).

next(_, 2, 3) :- !.
next(_, 3, 5) :- !.
next(N, D, N) :- D * D >= N, !.
next(N, D, D1) :-
 D mod 6 =:= 5,
 D1 is D + 2, !.
next(N, D, D1) :-
 D mod 6 =:= 1,
 D1 is D + 4.

count([], 1) :- !.
count([D | T], Res) :-
 count(T, Res1, D),
 Res is Res1 * D.

nth_prime(N, Ans) :-
 nth_ans(N, 2, Ans).

nth_ans(N, Cur, Ans) :-
    N =:= 0,
    Ans is Cur - 1, !.
nth_ans(N, Cur, Ans) :-
    prime(Cur),
    N1 is N - 1,
    Cur1 is Cur + 1,
    nth_ans(N1, Cur1, Ans), !.
nth_ans(N, Cur, Ans) :-
    Cur1 is Cur + 1,
    nth_ans(N, Cur1, Ans).