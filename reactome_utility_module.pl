/** <module> Reactome Utility Predicates
 *
 * This module has some useful predicates for interacting with the
 * owl:rdf ontology file for Reactome
 * @author Sam Neaves
 * With help from the SWI-Prolog Mailing list and the stackoverflow
 * communities.
 *
 */

:- module(reactome_utility,[
	      rid_type_iri/3,
	      rid_name/2,
              rid_stableid/2,
	      ridProtein_uniprotId/2,
	      ridProteinSet/1,
	      ridSimpleProtein/1,
	      allReactions/1,
	      ridReaction_input/2,
	      ridReaction_inputs/2,
	      ridReaction_output/2,
	      ridReaction_outputs/2,
	      ridProteinSet_component/2,
	      ridProteinSet_components/2,
	      ridComponent_child/2,
	      ridComponent_descendant/2,
              ridComponent_descendants/2,
	      ridComponent_descendantSetOrComplex/2,
	      ridComponent_childSimpleProtein/2,
              ridComplex_component/2,
              ridComplex_components/2,
	      rid_location/2,
	      ridReaction_controller_type/3,
	      ridReaction_ridLink_type_ridReaction/4,
	      ridPathway_component/2,
	      %ridPathway_descendant/2,
	      ridPathwayAtomic/1,
	      ridPathway_reactions/2,
	      ridPathway_link/2,
	      ridPathway_links/2,
              ridReaction_inputProbelist/2,
              ridProtein_probelist/2

	  ]).


:- use_module(pre_links).
:- use_module(calc_mods/probes).
:- use_module(pathway_link).
:- use_module(library(semweb/rdf11)).

:- rdf_register_prefix(bp, 'http://www.biopax.org/release/biopax-level3.owl#').
:- rdf_register_prefix(reactome, 'http://www.reactome.org/biopax/47/48887#').


/**
 * rid_type_iri(?Rid:atom, ?ClassName:atom, ?Iri:string) is nondet
 *
 * Generate Reactome identifiers alongside the class and International
 * Resource Identifiers (IRIs). An example:
 *
 * ?-rid_type_iri(Rid,Type,Iri).
 *
 * Rid = 'ModificationFeature296',
 * Type = 'ModificationFeature',Iri = 'http://www.reactome.org/biopax/47/48887#ModificationFeature296'
 * ;
 * Rid = 'PublicationXref8680', Type = 'PublicationXref', Iri ='http://www.reactome.org/biopax/47/48887#PublicationXref8680'
 *
 * etc
 */
rid_type_iri(Rid, ClassName, Iri) :-
  ground(ClassName), !,
  rdf_global_id(bp:ClassName, Class),
  rdf(Iri, rdf:type, Class),
  rdf_iri(Iri),
  rdf_global_id(reactome:Rid, Iri).
rid_type_iri(Rid, ClassName, Iri) :-
  ground(Rid), !,
  rdf_global_id(reactome:Rid, Iri),
  rdf(Iri, rdf:type, Class),
  rdf_global_id(bp:ClassName, Class).
rid_type_iri(Rid, ClassName, Iri) :-
  rdf_iri(Iri),
  rdf(Iri, rdf:type, Class),
  rdf_global_id(bp:ClassName, Class),
  rdf_global_id(reactome:Rid, Iri).

/**
 * rid_name(?Rid:atom, ?Name:string) is nondet
 *
 * Relates Reactome identifiers to their name. It can
 * be used in both directions or to generate. For example:
 *
 * ?-rid_name(R,Name).
 *
 * R = 'BiochemicalReaction31',
 * Name = "Dimerisation of CREB"
 *
 */
rid_name(Rid,Name):-
	rdf(Iri,bp:displayName,^^(Name,_S)),
	rdf_global_id(reactome:Rid,Iri).

/**
 * rid_stableid(?Rid:atom, ?StableId:atom) is nondet
 *
 * Convert between the identifiers used in this version of Reactome and
 * the stable identifiers. For example:
 *
 * ?-rid_stableid(R,S).
 *
 * R = 'Complex404',
 * S = 'REACT_14473.1'
 *
 */
rid_stableid(Rid,StableId):-
  rid_type_iri(Rid,_,Iri),
  rdf(Iri,bp:xref,IriCrossRef),
  rdf(IriCrossRef,bp:comment,Z^^_Type),
  string_concat("Reactome stable identifier. Use this URL to connect to the web page of this instance in Reactome: http://www.reactome.org/cgi-bin/eventbrowser_st_id?ST_ID=",StableIdString,Z),
  string_to_atom(StableIdString,StableId).


/** ridProtein_uniprotId(?RidProtein:atom, ?UniprotId:atom) is nondet
 *
 * Convert between Reactome protein identifiers and Uniprot identifiers.
 * For example:
 *
 * ?- ridProtein_uniprotId(RP,U).
 *
 * RP = 'Protein119',
 * U = 'Q96JA1'
 *
 */
ridProtein_uniprotId(P, UAtom) :-
  ground(UAtom), !,
  rid_type_iri(P, 'Protein', Iri),
  rdf(Iri, bp:entityReference, ERef),
  rdf(ERef, bp:xref, Xref),
  rdf(Xref,  bp:db, 'UniProt'^^_),
  rdf(Xref,  bp:id, UString^^_),
  atom_string(UAtom, UString).
ridProtein_uniprotId(P, UAtom) :-
  rid_type_iri(P, 'Protein', Iri),
  rdf(Iri, bp:entityReference, ERef),
  rdf(ERef, bp:xref, Xref),
  rdf(Xref,  bp:db, 'UniProt'^^_),
  rdf(Xref,  bp:id, UString^^_),
  atom_string(UAtom, UString).

/** ridProteinSet(?Rid:atom) is nondet
 *
 *  Generate a Reactome identifier that is a protien set, or check that
 *  an identifier refers to a protein set. For example:
 *
 * ?- ridProteinSet(R).
 *
 * R = 'Protein215' .
 *
 *
 */
ridProteinSet(Rid) :-
  rid_type_iri(Rid, 'Protein', Iri),
  rdf(Iri, bp:comment, "Converted from EntitySet in Reactome"^^_).

/** ridSimpleProtein(?Rid:atom) is nondet
 *
 *  Generate a Reactome identifier that refers to a protein that is
 *  not a protein set. Or test an identifier to see if it is a
 *  protein set. For example:
 *
 * ?- ridSimpleProtein(R).
 *
 * R = 'Protein119'
 *
 */
ridSimpleProtein(Rid):-
   rid_type_iri(Rid,'Protein',_),
   \+ridProteinSet(Rid).

%%%%%Membership related predicates%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/** allReactions(?Rids:list) is det
 *
 * Generate a list of of Reactions identifiers in Reactome (Or check
 * that a list corresponds to a list of all identifiers).
 * For example:
 *
 *
 * ?- allReactions(Rids).
 *
 * Rids = ['BiochemicalReaction57', 'BiochemicalReaction60',
 'BiochemicalReaction62', 'BiochemicalReaction63',
 'BiochemicalReaction65', 'BiochemicalReaction66',
 'BiochemicalReaction67', 'BiochemicalReaction68',
 'BiochemicalReaction69'|...]. *
 */
allReactions(Rids):-
	 findall(Rid,rid_type_iri(Rid,'BiochemicalReaction',_),Rids).

/** ridReaction_input(?Rid:atom, ?Rid:atom) is nondet
 *
 *  This finds top level inputs entities to a reaction that are
 *  not components of complexes or members of
 *  protein sets. For example:
 *
 * ?- ridReaction_input(Rid,I).
 *
 * Rid = 'BiochemicalReaction57',
 * I = 'Protein183'
 *
 */
ridReaction_input(Rid,I):-
         ground(I),!,
	 rid_type_iri(I,_,Input_iri),
         rdf(Iri,bp:left,Input_iri),
         rid_type_iri(Rid,'BiochemicalReaction',Iri).
ridReaction_input(Rid,I):-
         rid_type_iri(Rid,'BiochemicalReaction',Iri),
	 rdf(Iri,bp:left,Input_iri),
	 rid_type_iri(I,_,Input_iri).



/** ridReaction_inputs(?Rid:atom, ?Inputs:list) is nondet
 *
 * Relates a Reactome identifier for a reaction to a
 * list of top level inputs. (i.e. does not include
 * subcomponents of complexes and sets).
 * For example:
 *
 * ?- ridReaction_inputs(Rid, Inputs).
 *
 * Rid = 'BiochemicalReaction1',
 * Inputs = ['Protein1', 'Complex1']
 *
 *
 */
ridReaction_inputs(Rid, Inputs):-
	bagof(Input,ridReaction_input(Rid, Input), Inputs).

/** ridReaction_output(?Rid:atom, ?Rid:atom) is nondet
 *
 *  This finds top level outputs to a reaction but
 *  not sub components of complexes or members of
 *  protein sets.
 *  For example:
 *
 * ?- ridReaction_output(Rid,O).
 *
 * Rid = 'BiochemicalReaction57',
 * O = 'Complex116'
 *
 *
 */
ridReaction_output(Rid,O):-
	rid_type_iri(Rid,'BiochemicalReaction',Iri),
	rdf(Iri,bp:right,Out_iri),
	rid_type_iri(O,_,Out_iri).

/** ridReaction_outputs(?Rid:atom, ?Outputs:list) is nondet
 *
 * Relates a reaction to a list of top level outputs (i.e
 * does not include subcomponents of complexes and sets).
 *
 * ?- ridReaction_outputs(Rid,Outputs).
 *
 * Rid = 'BiochemicalReaction1',
 * Outputs = ['Complex2']
 *
 */
ridReaction_outputs(Rid,Outputs):-
	 bagof(Output,ridReaction_output(Rid,Output),Outputs).

/** ridProteinSet_component(?ProteinSet:atom, ?Rid:atom) is nondet
 *
 * Relates a Reactome identifier of a protein set to a
 * component of that Protein set.
 * For example:
 *
 * ?- ridProteinSet_component(ProteinSet, Component).
 *
 * ProteinSet = 'Protein215',
 * Component = 'Protein210'
 *
 */
ridProteinSet_component(ProteinSet, Component) :-
  ground(Component), !,
  rid_type_iri(Component, _, SetMemberIri),
  rdf(ProteinSetIri, bp:memberPhysicalEntity, SetMemberIri),
  rid_type_iri(ProteinSet,_,ProteinSetIri),
  ridProteinSet(ProteinSet).
ridProteinSet_component(ProteinSet, Component) :-
  ridProteinSet(ProteinSet),
  rid_type_iri(ProteinSet, _, ProteinSetIri),
  rdf(ProteinSetIri, bp:memberPhysicalEntity, SetMemberIri),
  rid_type_iri(Component, _, SetMemberIri).

/** ridProteinSet_components(?RidProteinSet:atom, ?Components:list) is nondet
 *
 *  Relates a Reactome identifier of a protein set to a list of
 *  components.
 *  For example:
 *
 *  ?- ridProteinSet_components(ProteinSet, Components).
 *
 * ProteinSet = 'Protein10002',
 * Components = ['Protein10003', 'Protein10004', 'Protein10005',
 'Protein10006', 'Protein10007', 'Protein10008', 'Protein10009',
 'Protein10010', 'Protein10011'|...] *
 *
 */
ridProteinSet_components(ProteinSet, Components) :-
  bagof(Component, ridProteinSet_component(ProteinSet, Component), Components).

/** ridComplex_component(?RidComplex:atom, ?Rid:atom) is nondet
 *
 * Relates a Reactome identifier of a protein complex to a
 * component.
 * For example:
 *
 * ?- ridComplex_component(ComplexId,ComponentId).
 *
 * ComplexId = 'Complex101',
 * ComponentId = 'Protein179'
 *
 */
ridComplex_component(ComplexId,ComponentId):-
	rid_type_iri(ComplexId,'Complex',Complex_iri),
	rdf(Complex_iri,bp:component,Component_iri),
	rid_type_iri(ComponentId,_,Component_iri).

/** ridComplex_components(?RidComplexId:atom, ?Components:list) is nondet
 *
 *  Relates a Reactome identifier of a protein complex to a list of
 *  components.
 *  For example:
 *
 * ?-ridComplex_components(ComplexId,ComponentId).
 *
 * ComplexId = 'Complex1',
 * ComponentId = ['Protein3', 'Protein2', 'Protein6', 'Protein9']
 *
 */
ridComplex_components(ComplexId,Components):-
	bagof(ComponentId,ridComplex_component(ComplexId,ComponentId),Components).

/** ridComponent_child(?RidComponent:atom, ?Child:atom) is nondet
 *
 * Releates either Reactome protein set identifiers or
 * Reactome complex identifiers to a subcomponent
 * that is one level down.
 * For example:
 *
 * ?- ridComponent_child(Rid,Child).
 *
 * Rid = 'Complex101',
 * Child = 'Protein179'
 *
 *
 */
ridComponent_child(Rid,Child):-
	ridComplex_component(Rid,Child).
ridComponent_child(Rid,Child):-
	ridProteinSet_component(Rid,Child).

/** ridComponent_descendant(?RidComponent:atom, ?Descendant:atom) is nondet
 *
 * Releates either Reactome protein set identifiers or
 * reactome complex identifiers to a subcomponent
 * any level down.
 * For example:
 *
 *
 * ?- ridComponent_descendant(Rid,Des).
 *
 * Rid = 'Complex101',
 * Des = 'Protein179'
 *
 *
 */
ridComponent_descendant(Rid,Des):-
	ridComponent_child(Rid,Des).
ridComponent_descendant(Rid,Des):-
	ridComponent_child(Rid,Inter),
	ridComponent_descendant(Inter,Des).


/** ridComponent_descendants(?Rid:atom, ?Dess:List) is nondet
 *
 * Relates a protein set or complex identifier to
 * all subcomponents recursivily.
 * For example:
 */
ridComponent_descendants(Rid,Dess):-
  setof(Des,ridComponent_descendant(Rid,Des),Dess).

/** ridComponent_descendantSetOrComplex(?RidComponent:atom, ?Des:atom) is nondet
 *
 * Relates a protein set or complex identifier to sub set or complex.
 * For example:
 *
 * ?- ridComponent_descendantSetOrComplex(Rid,Des).
 *
 * Rid = 'Complex101',
 * Des = 'Complex98'
 *
 */
ridComponent_descendantSetOrComplex(Rid,Des):-
	ridComponent_descendant(Rid,Des),
	rid_type_iri(Des,'Complex',_).
ridComponent_descendantSetOrComplex(Rid,Des):-
	ridComponent_descendant(Rid,Des),
	ridProteinSet(Des).

/** ridComponent_childSimpleProtein(?Rid:atom,?RidProtein:atom) is nondet
 *
 * Relates a protein set or complex idenifier to a subcomponent
 * that is a simple protein. i.e is not a set or complex.
 * For example:
 *
 * ?- ridComponent_childSimpleProtein(Rid,Protein).
 *
 * Rid = 'Complex116',
 * Protein = 'Protein183'
 *
 *
 */
ridComponent_childSimpleProtein(Rid,Protein):-
	ridComponent_child(Rid,Protein),
	ridSimpleProtein(Protein).

/** rid_location(?Rid:atom,?Location:string) is nondet
 *
 * Relates a Reactome identifier to a physical location in the cell.
 * For example:
 *
 * ?- rid_location(Rid,Location).
 *
 * Rid = 'Complex404',
 * Location = "plasma membrane"
 *
 */
rid_location(Rid,Location):-
	rid_type_iri(Rid,_T,I),
	rdf(I,bp:cellularLocation,LocVocabIri),
	rdf(LocVocabIri,bp:term,Location^^_).

%%% Pathway Extraction %%%%
%

/** ridReaction_controller_type(?Rid:atom, ?RidController: atom, ?Type:string) is nondet
 *
 * Releates a Reactome reaction identifier to its controller and the
 * type of that controller.
 * For example:
 *
 * ?- ridReaction_controller_type(Rid,RidController,Type).
 *
 * Rid = 'BiochemicalReaction63',
 * RidController = 'Complex120',
 * Type = "ACTIVATION"
 *
 *
 */
ridReaction_controller_type(Rid,RidController,Type):-
        ground(RidController),!,
	rid_type_iri(RidController,_,Controller_Iri),
        rdf(ControlIri,bp:controlled,RidIri),
        rid_type_iri(Rid,'BiochemicalReaction',RidIri),
	rdf(ControlIri, bp:controlType, Type^^_),
	rdf(ControlIri, bp:controller,Controller_Iri).
ridReaction_controller_type(Rid,RidController,Type):-
        rid_type_iri(Rid,'BiochemicalReaction',RidIri),
	rdf(ControlIri,bp:controlled,RidIri),
	rdf(ControlIri, bp:controlType, Type^^_),
	rdf(ControlIri, bp:controller,Controller_Iri),
	rid_type_iri(RidController,_,Controller_Iri).



/** ridReaction_ridLink_type_ridReaction_(?Rid1:atom, ?RidLinkEntity:atom, ?Type:string, ?Rid2:atom) is nondet
 *
 *  This is the calculated version. It is best to use
 *  ridReaction_ridLink_type_ridReaction/4.
 *  This is a link in the reaction graph. How are two reactions
 *  linked together and what is the link type?
 *  For example:
 */
ridReaction_ridLink_type_ridReaction_(Rid1,RidLinkEntity,Type,Rid2):-
        Type ="precedes",
	ridReaction_output(Rid1,RidLinkEntity),
	ridReaction_input(Rid2,RidLinkEntity).
ridReaction_ridLink_type_ridReaction_(Rid1,RidLinkEntity,Type,Rid2):-
	ridReaction_controller_type(Rid2,RidLinkEntity,Type),
	ridReaction_output(Rid1,RidLinkEntity).

/**
 * allEdges(?Edges:list, ?Size:int) is det
 *
 * Generate a list of all edges in the reaction graph.
 */
allEdges(Edges,Size):-
  findall(R1-R2,ridReaction_ridLink_type_ridReaction(R1,_L,_T,R2),Edges),length(Edges,Size).

/** ridPathway_component(?RidPathway:atom, ?RidComponent:atom) is nondet
 *
 *  Releates a Reactome pathway identifier to a component in the pathway.
 *  This does not search recursivily for components of subpathway. Use:
 *  ridPathway_descendantRidReaction/2 for that.
 *  Example:
 *
 * ?- ridPathway_component(RidPathway,RidComponent).
 *
 * RidPathway = 'Pathway21',
 * RidComponent = 'BiochemicalReaction62'
 *
 */
ridPathway_component(RidPathway,RidComponent):-
	rid_type_iri(RidPathway,'Pathway',PathwayIri),
	rdf(PathwayIri,bp:pathwayComponent,ComponentIri),
	rid_type_iri(RidComponent,_,ComponentIri).

/** ridPathway_descendantRidReaction(?RidPathway:atom, ?RidReaction:atom) is nondet
 *
 *  Relates a Reactome pathway identifier to a subcomponent in the
 *  pathway including recursion on subpathways.
 *  Example:
 *
 *  ?- ridPathway_descendantRidReaction(RidPathway,RidReaction).
 *
 * RidPathway = 'Pathway21',
 * RidReaction = 'BiochemicalReaction62'
 *
 *
 */
ridPathway_descendantRidReaction(RidPathway,RidReaction):-
	ridPathway_component(RidPathway,RidReaction),
	rid_type_iri(RidReaction,'BiochemicalReaction',_Type).
ridPathway_descendantRidReaction(RidPathway,RidReaction):-
	ridPathway_component(RidPathway,RidCompPathway),
	rid_type_iri(RidCompPathway,'Pathway',_),
	ridPathway_descendantRidReaction(RidCompPathway,RidReaction).

/** ridPathwayAtomic(RidPathway) is nondet
 *
 * Generate or test a pathway that is atomic.
 * i.e. does not have subpathways.
 * For example:
 *
 * ?- ridPathwayAtomic(RidPathway).
 *
 * RidPathway = 'Pathway26'
 *
 */
ridPathwayAtomic(RidPathway):-
	rid_type_iri(RidPathway,'Pathway',_),
	findall(Component,ridPathway_component(RidPathway,Component),Components),
	maplist(rid_type_iri,Components,Types,_Iris),
	maplist(dif('Pathway'),Types).

/** ridPathway_reactions(?RidPathway:atom, ?Reactions:list) is nondet
 *
 * Relates a Reactome pathway identifier to all component reactions.
 * Including reactions of any subpathways(Recursive).
 * For example:
 *
 * ?- ridPathway_reactions(RidPathway,Reactions).
 *
 * RidPathway = 'Pathway21',
 * Reactions = ['BiochemicalReaction43', 'BiochemicalReaction44',
 'BiochemicalReaction45', 'BiochemicalReaction46',
 'BiochemicalReaction47', 'BiochemicalReaction48',
 'BiochemicalReaction49', 'BiochemicalReaction50',
 'BiochemicalReaction51'|...] * *
 */
ridPathway_reactions(RidPathway,Reactions):-
	rid_type_iri(RidPathway,'Pathway',_),
	setof(R,ridPathway_descendantRidReaction(RidPathway,R),Reactions).


/** ridPathway_link_(?RidPathway:atom , ?Link:ridReaction_ridLink_type_ridReaction(Rid1,RidLink,Type,Rid2)) is nondet
 *
 * Find links in a pathway. Link is a term with R1, Link entity, type oflink and the R2
 */
ridPathway_link_(RidPathway,ridReaction_ridLink_type_ridReaction(Rid1,RidLink,Type,Rid2)):-
	ridPathway_reactions(RidPathway,Reactions),
         %dif(Rid1,Rid2),
         %convert Reactions to an assoclist
         maplist(key_keypair,Reactions,ReactionPairs),
         list_to_assoc(ReactionPairs,Assoc),
         ridReaction_ridLink_type_ridReaction(Rid1,RidLink,Type,Rid2),
         get_assoc(Rid1,Assoc,_),
         get_assoc(Rid2,Assoc,_).

/** key_keypair(?Key:atom,?Pair:X-value) is det
 *
 */
key_keypair(X,X-value).

/** ridPathway_links(?RidPathway:atom, ?Links:list) is nondet
 *
 * Relates a Reactome pathway identifier to the links (edges) in that
 * subgraph.
 * Each item
 * in the
 * list is a
 * term like
 * ridReaction_ridLink_type_ridReaction('BiochemicalReaction6',
 * 'Complex18', precedes, 'BiochemicalReaction7' )
 * Which means that BiochemicalReaction6 is linked by
 * Complex 18 via a precedes link to BiochemicalReaction7
 *
 * An example:
 *
 * ?- ridPathway_links(RidPathway,Links).
 *
 * RidPathway = 'Pathway1',
 * Links = [ridReaction_ridLink_type_ridReaction('BiochemicalReaction6',
'Complex18', precedes, 'BiochemicalReaction7'),
ridReaction_ridLink_type_ridReaction('BiochemicalReaction5',
'Complex17', 'ACTIVATION', 'BiochemicalReaction6'),
ridReaction_ridLink_type_ridReaction('BiochemicalReaction5',
'Complex17', precedes, 'BiochemicalReaction6'),
ridReaction_ridLink_type_ridReaction('BiochemicalReaction7',
'Complex14', precedes, 'BiochemicalReaction5'),
ridReaction_ridLink_type_ridReaction('BiochemicalReaction7',
'Protein53', precedes, 'BiochemicalReaction6')]
 *
 *
 */
ridPathway_links(RidPathway,Links):-
  bagof(Link,ridPathway_link(RidPathway,Link),Links).

/** setorcomplex(Rid) is nondet
 *
 */
setorcomplex(Des):-
  rid_type_iri(Des,'Complex',_).
setorcomplex(Rid):-
 ridProteinSet(Rid).

/** ridReaction_inputProbelist(?RidReaction:atom, ?InputProbes:list) is det
 *
 * Relates a Reactome reaction identiifer to the set of affymetrix
 * probes that are used for the proteins involved in the inputs to
 * that reaction.
 * For example:
 *
 * ?- ridReaction_inputProbelist(Rid,ProbesFlat).
 *
 * Rid = 'BiochemicalReaction1',
 * ProbesFlat = ['204270_at', '213755_s_at', '229265_at', '206675_s_at',
 '215889_at', '217591_at', '225227_at', '232379_at', '1565702_at'|...]
 *
 *
 */
ridReaction_inputProbelist(Rid,ProbesFlat):-
  ridReaction_inputs(Rid,Inputs),
  findall(I,(member(I,Inputs),setorcomplex(I)),ComplexInputs),
  %fails as not all inputs are complex
  maplist(ridComponent_descendants,ComplexInputs,Deses),
  flatten([Inputs|Deses],AllComps),
  findall(Probes,(member(P,AllComps),ridSimpleProtein(P),ridProtein_probelist(P,Probes)),Probes),
  flatten(Probes,ProbesFlat).





