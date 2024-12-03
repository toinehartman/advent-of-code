module days::Day2

import util::Math;
import util::Util;

import IO;
import List;
import Map;
import String;

alias Report = list[int];
alias Input = list[Report];

Input parseInput(str input) =
    [[toInt(l) | l <- split(" ", report)] | report <- split("\r\n", input)];

list[tuple[int, int]] asPairs(Report r) = zip2(prefix(r), tail(r));

bool isSafe(Report rep: [int a, int b, *_], false) {
    bool inc = b > a;
    for (<l, r> <- asPairs(rep)) {
        if (l == r) return false;
        if (r < l && inc) return false;
        if (l < r && !inc) return false;
        if (abs(r - l) > 3) return false;
    }
    return true;
}

bool isSafe(Report rep, true) = isSafe(rep, false) || any([*pre, _, *post] := rep, isSafe([*pre, *post], false));

int countSafe(Input input, bool dampen) = (0 | it + (isSafe(r, dampen) ? 1 : 0) | Report r <- input);

int a(Input input) = countSafe(input, false);

int b(Input input) = countSafe(input, true);

void main(bool example = false) {
    Input input = parseInput(readFile(getInput(2, example = example)));
    println("Part one: <a(input)>");
    println("Part two: <b(input)>");
}
