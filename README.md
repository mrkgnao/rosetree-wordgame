# rosetree-wordgame
Suppose I give you a list of words. Call two words *neighbours* if you can get from one to the other by adding a letter, removing one, or replacing a letter by another.

What is the longest chain of words you can form by jumping from neighbour to neighbour?

# Algorithm
The problem is a fairly straightforward longest-path problem. Since I haven't really worked with graphs in years, no particular algorithm stuck out to me. (I could write BFSes pretty quickly once upon a time.)

I came up with one that was essentially generating a rose-tree of descendants for each vertex, looking at the maximum depth of leaves in each tree, and taking the max over all of these. (This is sort-of complicated by a. the presence of multiple longest paths in certain graphs, and a lot more by b. having to print out the paths in their entirety, instead of just the length of the longest path.)

# Other things
I won't even bother with licenses or anything. Sorry for putting all the code in one file. (Gists, I know, I know.)
