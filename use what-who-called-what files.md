# Name persistence, version control, and the internet

It's a problem that names for files change. Tim Berners Lee wrote about this in [Design Issues with Linked Data](https://www.w3.org/DesignIssues/LinkedData.html).

A simple solution would be to keep `what-who-called-what` files. Keeping them for URLs and URIs would solve a lot of "404 errors".

They would also be helpful for ordinary natural language. Someone reading an economics student's notes, for instance, might be better able to decide which sections to read if they knew that "econometrics" means "statistics" to economists.

If your data is in VCS software, you're halfway there.

If you use Git with [Semantic Synchrony](https://github.com/synchrony/smsn/wiki), you can view an expression's git history as if it was part of the graph.
