#!/usr/bin/env python
# coding: utf-8

import sys
import os
import argparse

from bs4 import BeautifulSoup
import frontmatter # pip install python-frontmatter

import re

def process_note(soup: BeautifulSoup, contents: frontmatter.Post) -> str:
    """Process a note"""

    return convert_to_str(soup, contents, True)

def process_subnote(soup: BeautifulSoup, contents: frontmatter.Post) -> str:
    """Process a subnote"""

    process_environments(soup)

    return convert_to_str(soup, contents, True)

def process_lecture(soup: BeautifulSoup, contents: frontmatter.Post) -> str:
    """Process a lecture"""

    process_lecture_sections(soup)
    process_environments(soup)
    remove_ignore(soup, "h[0-9]", "ignore_heading")

    return convert_to_str(soup, contents, True)

def process_problem(soup: BeautifulSoup, contents: frontmatter.Post) -> str:
    """Process a problem"""

    return convert_to_str(soup, contents, True)

def process_challenge(soup: BeautifulSoup, contents: frontmatter.Post) -> str:
    """Process a challenge"""

    return convert_to_str(soup, contents, True)

source_types = {"n" : process_note, "s" : process_subnote, "l" : process_lecture, "p" : process_problem, "c" : process_challenge}

def process_environments(soup: BeautifulSoup) -> BeautifulSoup:
    # Convert theorem like environments
    environment_specs = [{"type" : "thm", "title": "Theorem"},
                         {"type" : "lem", "title": "Lemma"},
                         {"type" : "defn", "title": "Definition"},
                         {"type" : "eg", "title": "Example"},
                         {"type" : "pf", "title": "Proof"}]

    for environment_spec in environment_specs:
        environments = soup.find_all("div", class_=environment_spec["type"])
        environment_count = 0
        for environment in environments:
            environment_wrapper = soup.new_tag("div")
            environment_wrapper["class"] = f"{environment_spec['type']} card"
            environment_wrapper["id"] = f"{environment_spec['type']}-{environment_count}"

            environment.wrap(environment_wrapper)
            environment["class"] = "card-body"
            del environment["id"]

            card_header = soup.new_tag("div")
            card_header["class"] = "card-header"

            header = soup.new_tag("h5")
            header["class"] = "card-title"
            card_header.insert(0, header)

            environment_wrapper.insert(0, card_header)

            environment_count = environment_count + 1

    return soup

def process_lecture_sections(soup: BeautifulSoup) -> BeautifulSoup:
    section_classes = ["outline-2", "outline-3"]

    for section_class in section_classes:
        sections = soup.find_all("div", class_=section_class)
        for section in sections:
            section.name = "section"

    return soup

def remove_ignore(soup: BeautifulSoup, match: str, ignore_tag: str) -> BeautifulSoup:
    elements = soup.find_all(re.compile(match), class_=ignore_tag)
    for element in elements:
        element.extract()

def convert_to_str(soup: BeautifulSoup, contents: frontmatter.Post, with_yaml: bool) -> str:
    """Convert a soup to a string with or without YAML"""

    soup.body.hidden = True
    contents.content = soup.body.prettify()

    if with_yaml:
        output = frontmatter.dumps(contents)
    else:
        output = str(contents)

    return output

def process(source: str, destination: str, source_type: str):
    """Process the input"""

    with open(source, 'r') as f:
        contents = frontmatter.load(f)

    soup = BeautifulSoup(contents.content, "lxml")

    output = source_types[source_type](soup, contents)

    with open(args.destination, "w") as f:
        f.write(output)

def get_args() -> argparse.ArgumentParser:
    """Get input arguments"""

    parser = argparse.ArgumentParser(description="Post process org-exports")

    parser.add_argument("-t", "--type", help="Type of org-export to convert", type=str, choices=source_types.keys(), required=True)
    parser.add_argument("source", help="Source html file from custom org-export", type=str)
    parser.add_argument("destination", help="Output html after processing", type=str)

    return parser.parse_args()

if __name__ == "__main__":
    os.chdir(os.path.join(sys.path[0], ".."))

    args = get_args()
    process(args.source, args.destination, args.type)
