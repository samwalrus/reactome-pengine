#!/usr/bin/env swipl

:- use_module(library(pengines)).
:- initialization main.

server(S):-S="https://apps.nms.kcl.ac.uk/reactome-pengine".

main :-
        catch(readloop, E, (print_message(error, E), fail)),
        halt.
main :-
        halt(1).


readloop:-
        read_line_to_string(user_input,String),
        string_test(String).

string_test(String):-
        dif(String,end_of_file),
        atom_string(Atom,String),
        ridProtein_probelist(Atom,Animal),
        writeln(Animal),
        readloop.

string_test(Term):-
        Term = end_of_file,
        fail.


ridProtein_probelist(R,P):-
    server(S),
    pengine_rpc(S,ridProtein_probelist(R,P),[]).