#!/bin/bash

# check that we get the expected output

function alltests() {
    # Program 2.5, equivalent to paper examples (but running from pc=0)
    $spectector 'p_2_5.muasm' $SOLVER -n -a reach1 -c 'c([0=0,1=1,2=2,10=0], [pc=0,y=0])' # m[1]=1
    $spectector 'p_2_5.muasm' $SOLVER -n -a reach1 -c 'c([0=0,1=10,2=2,10=0], [pc=0,y=0])' # m[1]=10
    $spectector 'p_2_5.muasm' $SOLVER -w 4 -a reach1 -c 'c([0=0,1=1,2=2,10=0], [pc=0,y=0])' # m[1]=1
    $spectector 'p_2_5.muasm' $SOLVER -w 4 -a reach1 -c 'c([0=0,1=10,2=2,10=0], [pc=0,y=0])' # m[1]=10
    # Program 2.5, run fully symbolic
    $spectector 'p_2_5.muasm' $SOLVER -n -a reach1
    # Program 2.5b, internal test for concolic, not in the paper
    #   If M[2]=2 then M[0]=(-1) makes sym(_=0) true later.
    #   If M[2]=0 then we must inspect sym trace to see why it happens
    $spectector 'p_2_5b.muasm' $SOLVER -n -a reach1
    $spectector 'p_2_5b.muasm' $SOLVER -n -a reach1 -c 'c([2=0], [pc=0])'
    $spectector 'p_2_5b.muasm' $SOLVER -n -a reach1 -c 'c([2=2], [pc=0])'
    $spectector 'p_2_5b.muasm' $SOLVER -n -a reach1 -c 'c([0=(-1),2=2], [pc=0])'
    # Program Fig10, requires window size = 4
    $spectector 'p_10.muasm' $SOLVER -n -a reach1 -c 'c([], [pc=0,in=0,bound=1])' # in<bound
    $spectector 'p_10.muasm' $SOLVER -n -a reach1 -c 'c([], [pc=0,in=2,bound=1])' # in>=bound
    $spectector 'p_10.muasm' $SOLVER -w 4 -a reach1 -c 'c([], [pc=0,in=0,bound=1])' # in<bound
    $spectector 'p_10.muasm' $SOLVER -w 4 -a reach1 -c 'c([], [pc=0,in=2,bound=1])' # in>=bound
    # Program Fig10, run fully symbollic, requires window size = 4
    $spectector 'p_10.muasm' $SOLVER -n -a reach1
    $spectector 'p_10.muasm' $SOLVER -w 4 -a reach1
    # Concolic testing Program 2.5 (nonspec, reach paths)
    $spectector 'p_2_5.muasm' $SOLVER -n -a reach
    # Concolic testing Program Fig10 (nonspec, reach paths)
    $spectector 'p_10.muasm' $SOLVER -n -a reach
    # Concolic testing p1010 (Fig10 twice)
    $spectector 'p_1010.muasm' $SOLVER -n -a reach
    # Concolic testing program with cmov (nonspec, reach paths)
    $spectector 'p_cmov.muasm' $SOLVER -n -a reach
    # Speculative non-interference checking Program Fig10
    $spectector 'p_10.muasm' $SOLVER -w 4 -a noninter
    #
    # Equivalent to paper examples (but running from pc=0)
    # [0=0,1=1,2=2,4=2,13=4,10=0], [pc=0,y=10,v=4] # Spectre without speculation (in=4)
    $spectector 'spectre_lfence.muasm' $SOLVER -n -a reach1 -c 'c([0=7,1=1,2=2,4=2,13=4,10=0], [pc=0,y=10,v=13])' # Spectre without speculation (in=13)
    $spectector 'spectre_lfence.muasm' $SOLVER -w 4 -a reach1 -c 'c([0=7,1=1,2=2,4=2,13=4,10=0], [pc=0,y=10,v=4])'
    $spectector 'spectre_lfence.muasm' $SOLVER -w 4 -a reach1 -c 'c([0=7,1=1,2=2,4=2,13=4,10=0], [pc=0,y=10,v=13])'
    $spectector 'spectre_slh.muasm' $SOLVER -n -a reach1 -c 'c([0=7,1=1,2=2,4=2,13=4,10=0], [pc=0,y=10,v=13])' # Spectre without speculation (in=13)
    $spectector 'spectre_slh.muasm' $SOLVER -w 4 -a reach1 -c 'c([0=7,1=1,2=2,4=2,13=4,10=0], [pc=0,y=10,v=4])'
    $spectector 'spectre_slh.muasm' $SOLVER -w 4 -a reach1 -c 'c([0=7,1=1,2=2,4=2,13=4,10=0], [pc=0,y=10,v=13])'
    $spectector 'spectre.muasm' $SOLVER -n -a reach1 -c 'c([0=7,1=1,2=2,4=2,13=4,10=0], [pc=0,y=10,v=13])' # Spectre without speculation (in=13)
    $spectector 'spectre.muasm' $SOLVER -w 4 -a reach1 -c 'c([0=7,1=1,2=2,4=2,13=4,10=0], [pc=0,y=10,v=4])'
    $spectector 'spectre.muasm' $SOLVER -w 4 -a reach1 -c 'c([0=7,1=1,2=2,4=2,13=4,10=0], [pc=0,y=10,v=13])'
    # Concolic
    $spectector 'spectre_lfence.muasm' $SOLVER -n -a reach
    $spectector 'spectre_slh.muasm' $SOLVER -n -a reach
    $spectector 'spectre.muasm' $SOLVER -n -a reach
    $spectector 'pload.muasm' $SOLVER -n -a reach
    case $SOLVER in
	*z3) $spectector 'loadsym.muasm' $SOLVER -n -a reach
    esac
}

spectector=spectector # TODO: locate

# SOLVER="--solver naive"
# alltests > test_results.txt || exit 1
# SOLVER="--solver z3"
# SOLVER="--nextpath-timeout 100 --noninter-timeout 100 --timeout 5000"
alltests > test_results.txt || exit 1
diff test_results.txt test_results.txt-ok && echo ok

