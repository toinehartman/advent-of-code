module Util

import ParseTree;

loc getInput(int day, bool example = false) =
    |project://advent-of-code/input/<"<day>">/<example ? "example" : "puzzle">.txt|;
