# In a nutshell

DWT is knowledge graph software, designed with a focus on ease and expressivity.

DWT implements the RSLT, an unusually expressive kind of knowledge graph. In a traditional "flat" graph, all relationships are binary, and relationships cannot involve other relationships. The RSLT permits relationships of arbitrary arity (number of members), arbitrarily nested (e.g. edges connecting edges) relationships.

The Hash language allows a user to add statements to a knowledge graph without having to learn to program, and without needing to know about graph schemas like RDF.

Querying functions let the user traverse the graph, asking questions like, "What are all the things I have to do today?", "What do I care about that Professor X could talk about?", or "What tasks does Sharon have to do that depend on tasks I have to do?"


# To try it

[Try the UI!](/introduction/try-the-ui.md)


# How it works

## For users and coders

This [short paper](/introduction/the_rslt,_why_and_how/it.pdf) describes how the RSLT is implemented and why it is helpful. It requires no background in math or programming.

An even shorter paper describes [the Hash language](/Hash/the-hash-language.md), which allows a user to add data to the graph.


## For coders

The [simplified model of the type system](/introduction/Minimal_Types.hs) might be the best place to start exploring the code. Or maybe you should just dive into the src/ folder. ([module-hierarchy.md](/introduction/module-hierarchy.md) explains how the folders and filenames under src/ indicate the dependency structure of the code.)


# Related projects

[Semantic Synchrony](https://github.com/synchrony/smsn/wiki) changed my life. It offers scale, federation, Git integration, and a sweet Emacs GUI.
