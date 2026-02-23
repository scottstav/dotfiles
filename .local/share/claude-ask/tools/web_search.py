"""Search the web via DuckDuckGo."""

from ddgs import DDGS

name = "web_search"
description = "Search the web for current information. Returns titles, URLs, and snippets for the top results. Use fetch_url to read full articles."
input_schema = {
    "type": "object",
    "properties": {
        "query": {
            "type": "string",
            "description": "The search query",
        },
    },
    "required": ["query"],
}


def run(input):
    query = input["query"]
    results = DDGS().text(query, max_results=5)
    if not results:
        return "No results found."
    lines = []
    for r in results:
        lines.append(f"**{r['title']}**")
        lines.append(r["href"])
        lines.append(r.get("body", ""))
        lines.append("")
    return "\n".join(lines).strip()
