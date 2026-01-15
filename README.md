# Advent of FPGA submission implementing (at this time) day 1

The basic structure of the project is as follows:
- `D1_logic` implements the logic for the task proper. This file implements both parts, which adhere to the same interface so that the other parts of the project can be parameterised by the task part.
- `D1_byte` implements ASCII input on top of whichever part is given as a functor argument. **Note**: additionally, the ascii decoder is responsible for reducing the numbers mod 100. Therefore, `D1_logic` on its own will fail if you give it three-digit input numbers. One reason for this is that reducing mod 100 is a lot easier in decimal thna in binary. Another is that I am lazy.
- `Util_comb` implements widening and multiplication since I could not find those in the hardcaml library.
- `Generate` is the front-end from the template modified to generate Verilog code for any combination of {logic, byte} and {prat1, part2}.
- the "test" directory contains simple tests for the sample cases, together with (in the `_byte` version) my actual input, which is not included in the actual repository per AoC policy.

The implementation of part 1 is fairly straightforward, using the Always DSL. `cur_temp` and `cur_temp_post_mod` contain (current value + shift) and (that) % 100 respectively.

The implementation of part 2 is a bit messier. It vaguely corresponds to the following ~~Python~~ pseudocode logic:

```py
def accepting_inputs(direction, amount):
  if direction == left:
    current -= amount
    if current < 0: count += 1; current += 100
    if current == 0: count += 1
  else:
    current += amount
    if current >= 100: count += 1; current -= 100
  if old value of current == 0: don't do the count += 1
```

The implementation of `D1_byte` is again mostly straightforward. The digits in the input are stored in an ad hoc shift register thing, so that we can treat the hundreds specially later. When it receives a newline, it sends the received command to an embedded `D1_logic` component.

The implementation of the tests is uninteresting.

## How to run

`dune test` and observe the tests work, or `dune exec bin/generate.exe [...]` to generate verilog.
