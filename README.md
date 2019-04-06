<img src="/ingraph-logo.png" align="right" width="240" style="float:right;">

# Ingraph

Ingraph is a tool for determining the best way to link portals together in the game Ingress. Briefly, portals exist at
various coordinates on a map, and these portals can be linked together. Linking three portals together to form a triangle
creates a field.

Links and fields are worth a certain number of points (AP) to create, and a different number of points to destroy. One way
to optimize links is to maximize points due to creation while minimizing points due to destruction.

I thought, it should be possible to write a little program that does this optimization and tells you what portals to link
together. One could even do the optimization in a fun physics-y way. So that's what you find here.

Using the metric described above, specifically the ratio of creation AP to destruction AP, this tool decides which portal
links will give you the best result. It also takes into account how badly you'd like to make fields, but for those details
see the [algorithm details](http://atistar.net/~stepp/ingraph/#algo). Since that initial idea, I've added several other optimization functions, which you can choose between.

I should also note that in terms of actual game-play, this optimization probably has no useful effect, or an effect that's
too small to notice. But it was a fun exercise!

Much more information and screenshots can found at [the Ingraph website](http://atistar.net/~stepp/ingraph/).
