"""Fetch and extract article text from a URL."""

import trafilatura

name = "fetch_url"
description = "Fetch a web page and extract its main text content. Use after web_search to read full articles."
input_schema = {
    "type": "object",
    "properties": {
        "url": {
            "type": "string",
            "description": "The URL to fetch",
        },
    },
    "required": ["url"],
}


def run(input):
    url = input["url"]
    downloaded = trafilatura.fetch_url(url)
    if not downloaded:
        return f"Failed to fetch {url}"
    text = trafilatura.extract(downloaded)
    if not text:
        return f"Could not extract text from {url}"
    return text[:6000]
