# What DWT is

DWT is knowledge graph software aimed at non-programmers.

The Reflective Set of Labeled Tuples ("RSLT") is a generalization of the graph that is simultaneously easier to use and more expressive. Relationships in a RSLT can have any number of members, and can themselves be members of other relationships. Statements in a RSLT look like ordinary speech: for instance, "(I #like dentists) #because (dentists #give gerbils #to me)".

[Hash](https://github.com/JeffreyBenjaminBrown/digraphs-with-text/blob/master/Hash/the-hash-language.md) is a simple pattern-matching language for adding to and querying a RSLT. It is less expressive than, say, Gremlin. (I would like to implement Hash using Gremlin, and make Gremlin calls available to users who want it). However, Hash requires no programming experience, and Hash queries resemble ordinary speech. For instance, to ask for the reasons Germany lost WWII, one could write "(Germany #lost WWII) #because /it".


# How to try DWT

The demo requires no programming experience. This [short (730 words) guide](/introduction/try-the-ui.md) describes how to start it, how to use it, and how to import and export data.


# How DWT works

The codebase is small -- 1300 lines, if you exclude tests, imports, exports and blank lines. 

This [short paper](/introduction/the_rslt,_why_and_how/it.pdf) describes how the RSLT is implemented and why it is helpful. It requires no background in mathematics or programming.

The [simplified model of the type system](/introduction/Minimal_Types.hs) might be the best place to start exploring the code.

Or just dive into the src/ folder. (When you do that, [module-hierarchy.md](/introduction/module-hierarchy.md) explains how the folders and filenames under src/ indicate the dependency structure of the code.)


# The future of DWT

Some plans for the software's future can be found in the [issue tracker](https://github.com/JeffreyBenjaminBrown/digraphs-with-text/issues). Other ideas are welcome.
