# Playing a wordgame using rose trees
Suppose I give you a list of words. Call two words *neighbours* if you can get from one to the other by adding a letter, removing one, or replacing a letter by another.

What is the longest chain of words you can form by jumping from neighbour to neighbour?

[Here](http://codepad.org/nBcCgA2Q) is a Codepad demo of this program, for those of you who don't have GHC installed.

# Algorithm
The problem is a fairly straightforward longest-path problem. Since I haven't really worked with graphs in years, no particular algorithm stuck out to me. (I could write BFSes pretty quickly once upon a time.)

I came up with one that was essentially generating a rose-tree (incidentally my favourite data-structure because of how pretty

    data RoseTree a = RoseTree a [RoseTree a]

looks in Haskell!) of descendants for each vertex, looking at the maximum depth of leaves in each tree, and taking the max over all of these. (This is sort-of complicated by a. the presence of multiple longest paths in certain graphs, and a lot more by b. having to print out the paths in their entirety, instead of just the length of the longest path.)

I'm sure this is terribly inefficient and everything (probably `O(V^2)`?)

# A few example runs
(These are from [the source of the problem](http://www.iarcs.org.in/inoi/2004/zio2004/zio2004-qpaper.pdf)).

    λ> main
    Enter words, separated by spaces:
    below blow bow bowl brow crow rot row
    The longest path length is 7
    Paths:
    below -> blow -> bow -> brow -> crow -> row -> rot
    bowl -> bow -> blow -> brow -> crow -> row -> rot
    ---
    λ> main
    Enter words, separated by spaces:
    be bet bet bud but dig do dog dug get go god got
    The longest path length is 13
    Paths:
    bud -> but -> bet -> be -> bet -> get -> got -> god -> go -> do -> dog -> dig -> dug
    dig -> dug -> dog -> do -> go -> god -> got -> get -> bet -> be -> bet -> but -> bud
    dug -> dig -> dog -> do -> go -> god -> got -> get -> bet -> be -> bet -> but -> bud
    ---
    λ> main
    Enter words, separated by spaces:
    fat fate fight fit fright gait gate light mite quit quite right sat sigh sight sit site suite writ
    The longest path length is 9
    Paths:
    gate -> fate -> fat -> sat -> sit -> site -> suite -> quite -> quit

# Other things
I won't even bother with licenses or anything. Sorry for putting all the code in one file. (Gists, I know, I know.)
