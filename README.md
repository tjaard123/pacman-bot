# Megatron - An F# Pac-Man Bot

This was my entry to the 2014 Entelect 100K challenge.  It was also my first venture into functional, obviously missing most of the point of functional but fun nonetheless.

---

building... normal msbuild build, nothing fancy

megatron's main strategy is building a transposition table (map) containing nodes like this:

(1,1) - [list of possible routes with their values]

megatron can then query this table and ask all sorts of questions:

what is the shortest route to this node?
what is the most valuable route to this node?
etc.

megatron also determines a target, by looking at the values in quadrants, taking his opponent into consideration
megatron rushes to stars as his opening, trying to hinder the opponent as much as possible

all F# functional goodness!