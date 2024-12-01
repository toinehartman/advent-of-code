module days::Day1

import util::Util;

import IO;
import List;
import String;
import util::Math;

alias Input = tuple[list[int] left, list[int] right];

Input parseInput(str input) =
    unzip2([<toInt(l), toInt(r)>
        | line <- split("\r\n", input)
        , [l, r] := split("   ", line)
    ]);

int pairWiseDistance(list[int] left, list[int] right) =
    (0 | it + abs(l - r) | <l, r> <- zip2(left, right));

int similarity(list[int] left, list[int] right) {
    rCount = distribution(right);
    return (0 | it + l * (rCount[l] ? 0) | l <- left);
}

int a(Input input) =
    pairWiseDistance(sort(input.left), sort(input.right));

int b(Input input) =
    similarity(input.left, input.right);

void main(bool example = false) {
    Input input = parseInput(readFile(getInput(1, example = example)));
    println("Part one: <a(input)>");
    println("Part two: <b(input)>");
}
