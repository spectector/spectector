:- bundle(spectector).
version('1.0').
depends([
    core-[version>='1.18'],
    'github.com/ciao-lang/concolic',
    muasm_translator
]).
alias_paths([
    spectector = 'src'
]).
lib('src').
cmd('src/spectector').
