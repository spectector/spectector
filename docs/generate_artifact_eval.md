# Generate Spectector's artifact evaluation version

One could proceed as follow:
1. Install the Ciao system from `https://github.com/ciao-lang/ciao`.
2.  Create a folder `~/ciao` using `mkdir ~/ciao`.
3.  Mark the folder `~/ciao` as a CIAO workspace by executing `export
    CIAOPATH=~/ciao` and update your `PATH` with `eval "$(ciao-env)"`. 
4.  Move to the workspace by executing `cd ~/ciao`.
5.  Clone the main Spectector's repository into the workspace by executing `git
    clone https://github.com/spectector/spectector.git`.
6.  Clone Spectector's dependencies into the workspace by executing `git clone
    https://github.com/spectector/concolic.git` and `git clone
    https://github.com/spectector/muasm_translator.git`.
7. Execute the following command: `ciao custom_run . bindist`. This calls the
   `make_distro.sh` script with some environment variables needed to locate the
   right binaries. The binary distribution is stored at the `dist/` folder under the
   `spectector/` directory. The command also generates a `.tar.gz` file with the distribution.