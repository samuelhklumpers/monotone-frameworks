# mf

Authors: Yorick Sijsling, Jarno le Conte, Nico Naus, Jurriaan Hage, Ivo Gabe de Wolff

Students: Samuel Klumpers (6057314), Philipp Zander (7983034)

This projects implements three code analyses for the While language:
- Constant Propagation
- Branch Aware Constant Propagation
- Strongly Live Variable Analysis

These are constructed as monotone frameworks using attribute grammars,
which can then be promoted to embellished frameworks,
and be solved by the data flow equation solver described in NNH05.

Implemented extensions:
- Embellished instance for the strongly live variables analysis
- Add the branch aware constant propagation analysis of *our own design*
- Make some operations create constants even if one operand is not constant (e.g. `&& false`)
- Implement `print` statement (and alter lexer and parser)
- Implement `break` and `continue` language constructs
- Hack lexer and parser to handle boolean assignments.

Documentation:
- can be built using `stack haddock`, or viewed at https://rednaz.12hp.de/monotone_frameworks
- in your_tex_output_here.html
