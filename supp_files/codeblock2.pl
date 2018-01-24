:-use_module(library(pengines)).
reactome_server('https://apps.nms.kcl.ac.uk/reactome-pengine').

my_program(P):-
    P=[
      (
      pathwayName_subpathway(PName,SubName):-
          rid_name(RidPathway,PName),
          ridPathway_component(RidPathway,RidComponent),
          rid_type_iri(RidComponent,'Pathway',_),
          rid_name(RidComponent,SubName)
      )
          
      ].
      
pathwayName_subpathway(PName,SubName):-
    reactome_server(S),
    my_program(P),
    pengine_rpc(S,pathwayName_subpathway(PName,SubName),[src_list(P)]).