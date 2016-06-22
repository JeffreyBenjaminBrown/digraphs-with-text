Tables can only express certain kinds of data. Trees, only certain kinds of data. Lists, sets, maps, tuples; only certain kinds of data.

The expressivity limits of data structures are a problem, because information that cannot be represented cannot be processed automatically. Medicine provides the most stark example of the problem, but it is ubiquitous.

The Reflective Set of Labeled Tuples (RSLT) is a data structure, only slightly more complex than a graph, that can represent any kind of data.

This [short paper](/introduction/the_rslt,_why_and_how/it.pdf) describes what the RSLT is and why it is helpful. It requires no familiarity with computer programming.

There is an [interactive demonstration](/introduction/demo.hs) of how the RSLT represents data. The [howto](/howto) folder shows how to do other things, such as parse a Freeplane .mm file or search along branches.

The [simplified model of the type system](/introduction/Minimal_Types.hs) is the best place to start exploring the code.