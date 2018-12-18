# Compiling Spectector from sources

For developing, it is recommended to define your own
_workspace directory_ and clone this repository into it.

One could proceed as follow:
1. Install the Ciao system from `https://github.com/ciao-lang/ciao`.
2.  Create a folder `~/ciao` using `mkdir ~/ciao`.
3.  Mark the folder `~/ciao` as a CIAO workspace by executing `export CIAOPATH=~/ciao` and update your `PATH` with `eval "$(ciao-env)"`. 
4.  Move to the workspace by executing `cd ~/ciao`.
5.  Clone the main Spectector's repository into the workspace by executing `git clone https://github.org/spectector/spectector.git`.
6.  Clone Spectector's dependencies into the workspace by executing `git clone https://github.org/spectector/concolic.git` and `git clone https://github.org/spectector/spectector.git`.
7. Spectector can be built by running `ciao build  -r spectector`  in the workspace `~\ciao`. This also downloads the Z3 solver.

Use the following commands to check your installation (it should show `ok` in a few seconds):
```
cd ~/ciao/spectector/tests/
./runtests.sh
```