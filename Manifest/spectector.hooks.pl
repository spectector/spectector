:- module(_, [], [ciaobld(bundlehooks)]).

:- doc(title,  "Bundle Hooks for spectector").

:- use_module(engine(internals), [ciao_root/1]).
:- use_module(library(streams)).
:- use_module(library(bundle/bundle_paths)).
:- use_module(library(pathnames)).
:- use_module(library(sh_process), [sh_process_call/3]).

% (hook)
'$builder_hook'(custom_run(bindist, [])) :- !,
    % Create a binary distribution
    Script = ~bundle_path(spectector, 'Manifest/make_distro.sh'),
    ThirdParty = ~path_concat(~ciao_root, 'third-party'),
    Env = ['THIRDPARTY' = ThirdParty],
    sh_process_call(Script, [], [env(Env)]).

        
