:-use_module(library(pengines)).
reactome_server("https://apps.nms.kcl.ac.uk/reactome-pengine").
path_program(Program):-
 Program=[
 (:- meta_predicate path(2,?,?,?)),
 (:- meta_predicate path(2,?,?,?,+)),
 (graph_path_from_to(P_2,Path,From,To):-
    path(P_2,Path,From,To)),
 (path(R_2, [X0|Ys], X0,X):-
    path(R_2, Ys, X0,X, [X0])),
 (path(_R_2, [], X,X, _)),
 (path(R_2, [X1|Ys], X0,X, Xs) :- 
    call(R_2, X0,X1),
    non_member(X1, Xs),
    path(R_2, Ys, X1,X, [X1|Xs])),
 (non_member(_E, [])),
 (non_member(E, [X|Xs]) :-
  dif(E,X),non_member(E, Xs)),
  ( e(R1,R2):-
  ridReaction_ridLink_type_ridReaction(R1,_,_,R2)
  ) 
 ].
path_from_to(Path,From,To):-
 reactome_server(Server),
 path_program(Program),
 pengine_rpc(Server,
 graph_path_from_to(e,Path,From,To),
 [src_list(Program)]).