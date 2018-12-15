:- bundle(spectector).
version('1.0').
depends([
    core-[version>='1.18'],
    concolic,
    muasm_translator
]).
alias_paths([
    spectector = 'src'
]).
lib('src').
cmd('src/spectector').
