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
- handle `print` statements
- handle `break` and `continue`
- handle boolean assignment `b=`
- introduce constants for operations that are sometimes constant in one parameter (e.g. `x && false == false`)
