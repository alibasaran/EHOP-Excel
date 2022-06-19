from pprint import pprint
import re
from math import log2, exp

##
## Readability metric measurement
## Make sure that all the operators and operands are separated by spaces.
## Following "a simpler metric code for readability"
##

# filepath = "./koka_app_readability.kk"
filepath_h = "./HaskellAppReadability.hs"
# How comments are done in the code
comments_with_h = "--"
# Processes lines after start_offset, used to skip imports
start_offset_h = 29

filepath_kk = "./koka_app_readability.kk"
comments_with_kk = "//"
start_offset_kk = 10


def run_readability_analysis(filepath, comments_with, start_offset=0):
    with open(filepath, "r") as f:
        # Filter out empty lines and lines that only have comments
        # First 30 lines of ExcelBuild.hs is imports etc
        f_string = f.read()
        lines = [re.sub('\s+',' ', line).strip() for line in f_string.split("\n")][start_offset:]
        f.close()

    lines_without_comments = list(filter(lambda line: not line.startswith(comments_with), lines))
    physical_lines = list(filter(lambda line: line != "", lines_without_comments))

    counts = {}
    for line in physical_lines:
        # Remove inline comments
        line = line if comments_with not in line else line[:line.index(comments_with)]
        # Find all strings
        for matches in re.findall("(\"(.*?)\")|(\'(.*?)\')", line):
            for match in matches:
                if "\"" in match or "'" in match:
                    counts[match] = 1 if match not in counts else counts[match] + 1
                    line = line.replace(match, "")
        for op in line.split(" "):
            counts[op] = 1 if op not in counts else counts[op] + 1
    counts.pop("")

    num_lines = len(lines)
    print("===============")
    print(f"For {filepath}")
    print(f"Total num of lines: {num_lines}")
    print(f"Lines without comments: {len(lines_without_comments)}")
    print(f"Physical Lines (No comment or empty lines): {len(physical_lines)}")

    p_vocab = len(counts.keys())
    p_length = sum(counts.values())
    print(f"Program Vocabulary = {p_vocab}")
    print(f"Program Length = {p_length}")
    volume = p_length * log2(p_vocab)
    print(f"Halstead's Volume = {volume}")

    entropy = 0
    for _, count in counts.items():
        p = count / p_length
        entropy += p * log2(p)
    entropy *= -1

    print(f"Entropy = {entropy}")
    z = 8.87 - 0.033 * volume + 0.4 * num_lines - 1.5 * entropy
    # FROM: A simpler model of software readability by Posnett, Daryl, Hindle, Abram, Devanbu, Premkumar
    smcr = 1 / (1 + exp(-z))
    print(f"A simpler metric code for readability: {smcr}")

run_readability_analysis(filepath_kk, comments_with_kk, start_offset_kk)
run_readability_analysis(filepath_h, comments_with_h, start_offset_h)
