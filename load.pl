:- use_module(library(doc_http), []).
:- use_module(library(settings)).
:- use_module(library(http/http_host)).
:- use_module(library(http/http_path)).

:- set_setting_default(http:public_host,   'apps.nms.kcl.ac.uk').
:- set_setting_default(http:public_port,   443).
:- set_setting_default(http:public_scheme, https).

:- multifile http:location/3.
:- dynamic   http:location/3.

http:location(root, '/reactome-pengine', []).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- http_handler(root(.), say_hi, []).


%:- doc_server(4001).    % Start PlDoc at port 4000
%:- portray_text(true).  % Enable portray of strings
%Pengines imports
:- use_module(library(pengines)).

:- use_module(pengine_sandbox:reactome_utility_module).
%:- use_module(pengine_sandbox:library(semweb/rdf_db)).
:- use_module(pengine_sandbox:library(semweb/rdf11)).

%:- use_module(pengine_sandbox:library(semweb/rdf_db)).
%:- use_module(pengine_sandbox:calc_mods/link_types).
%:- use_module(pengine_sandbox:mydebug).
:- use_module(library(sandbox)).
%:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_sandbox)).


:- rdf_load('Homo_sapiens.owl',[register_namespaces(true)]).


:- multifile sandbox:safe_primitive/1.
sandbox:safe_primitive(rdf11:rdf(_,_,_)).
%sandbox:safe_primitive(rdf_prefixes:rdf_prefix(_,_)). %New


sandbox:safe_primitive(rdf11:rdf_iri(_)).
%sandbox:safe_primitive(rdf_db:rdf_current_prefix(_,_)).

%sandbox:safe_primitive(rdf_db:rdf_global_id(_,_)).
%sandbox:safe_primitive(rdf11:rdf_prefix(_,_)).


%sandbox:safe_primitive(reactome_utility_module:rid_type_iri(_,_,_)).

:- use_module(library(http/http_unix_daemon)).



:- initialization http_daemon.

%:-set_prolog_stack(global, limit(15*10**9)).
%:-load_rdf('Homo_sapiens.owl',List_of_RDF_statements),checklist(assert,List_of_RDF_statements).
%:-use_module('utility_module.pl').
:-use_module('reactome_utility_module.pl').
%:-use_module('calc_mods/types_corrected.pl').
%:-use_module('calc_mods/complexes_corrected.pl').
%:-use_module('calc_mods/protein_sets_corrected.pl').
%:-use_module('calc_mods/link_types.pl').
%:-use_module('calc_mods/linking_entities_whole_network.pl').
%:-use_module('calc_mods/reaction_outputs_corrected.pl').
%:-use_module('calc_mods/reaction_inputs_corrected.pl').
%:-use_module('mydebug.pl').

%sandbox:safe_primitive(mydebug,my_make).

:- use_module('logging.pl').

%:- doc_server(4000).

%sandbox:safe_primitive(reactome_utility_module:add_fact(_)).


http:location(pldoc, root(documentation), [priority(100)]).

say_hi(_Request) :-
    reply_html_page(
	   [title('Reactome Pengine')],
	   [h1('Reactome Pengine'),
	        p('This is the home page of Reactome Pengine developed by Sam Neaves. A paper describing the tool is in press. To see a notebook interacting with the service see here: '),
                a([href='https://swish.swi-prolog.org/?code=https://raw.githubusercontent.com/samwalrus/reactome_notebook/master/reactome_pengine.swinb'],'SWISH Notebook'),
                p('To read about the built in predicates see: '),
                a([href='https://apps.nms.kcl.ac.uk/reactome-pengine/documentation'],'Online Predicate Documentation')]).
