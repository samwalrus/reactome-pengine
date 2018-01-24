:-use_module(library(pengines)).
reactome_server('https://apps.nms.kcl.ac.uk/reactome-pengine').

program(P):-
    P=[
      (
       pathway_acrossmembrane(Pathwayname):-
          Location = "plasma membrane",
          rid_type_iri(RidPathway,'Pathway',_Iri),
          rid_name(RidPathway,Pathwayname),
          ridPathway_component(RidPathway,RidReaction),
          ridReaction_input(RidReaction,RidEntity),
          rid_location(RidEntity,Location)
      )
      ].

pathway_acrossmembrane(PathwayName):-
    reactome_server(S),
    program(P),
    pengine_rpc(S,pathway_acrossmembrane(PathwayName),[src_list(P)]).