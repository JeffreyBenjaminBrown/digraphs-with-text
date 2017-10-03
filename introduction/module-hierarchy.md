## Module hierarchy terminology: Initial, Second, and Terminal

I borrowed some language from category theory so that the names of modules could clarify the dependency hierarchy:

* The modules Dwt.Initial.* import nothing except in some cases each other.

* The modules Dwt.Second.* import nothing except modules from Dwt.Initial and Dwt.Second.

* Dwt.Query.Initial imports nothing else from Dwt.Query.

* Dwt.UI.Terminal imports every other module in Dwt.UI.

