## 0.5.0

- Parallel evaluation: with introduction of `ascent_par!` and `ascent_run_par!` macros, Ascent programs can utilize multiple CPU cores to speed up evaluation.

- Made improvements to handling of rules that start with `if`, `let`, or `for` (they can be simple joins).

## 0.4.0
- Reduced the size of generated code for a rule with N dynamic relations from O(N * 2^N) to O(N^2); 
  dramatically improving compile times for some programs.

## 0.3.0
- Introduced macro-in-macros!

## 0.2.1
- Bug fix

## 0.2.0
- Added run_timeout
- Disallowed shadowed variables