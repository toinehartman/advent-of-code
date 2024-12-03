module days::Day3

import util::Util;

import IO;
import String;

data Instruction = mul(int l, int r);

alias Input = list[Instruction];

Input parseInput(str input) = [mul(toInt(l), toInt(r)) | /mul\(<l:\d+>,<r:\d+>\)/ := input];

int a(Input input) = (0 | it + i.l * i.r | Instruction i <- input);

int b(Input input) = -1;

void main(bool example = false) {
    Input input = parseInput(readFile(getInput(3, example = example)));
    println("Part one: <a(input)>");
    println("Part two: <b(input)>");
}
