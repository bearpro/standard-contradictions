# Standard contradiction andlysis

Set of tool to check for contradictions and conflicts inside and between normative documents 

# Repository overview

- `/ddl-ltlf` - DSL that can represent normative statements as set of predicates and [deontic logic](https://en.wikipedia.org/wiki/Deontic_logic) formulas.  
  This folder contains both grammar, parser, semantic analyther, runtime (solver) and shell tool for this language.
- `/ddl-ltlf-from-text` - Tool for generation `.mdl` (ddl-ltlf files) based on natural text, by using LLMs.
- `/semantic-segmenter` - Tool for splitting large text into semanticly complete chunks.
- `/embedding-clusters` - my early attempt to find contradictions, based on the idea of somewhat simmilarity between contradictive statements.
