from mdl.dsl import *

module("pipe_spec")


@record
class Pipe:
    length: Rat
    radius: Rat


pipe = entity(Pipe)


@rule(O)
def pipe_length_positive():
    return always(pipe.length > 0)


fact(pipe == Pipe(length=10, radius=2))
