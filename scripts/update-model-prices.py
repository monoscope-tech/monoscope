#!/usr/bin/env python3
"""Regenerate static/model_prices.json from LiteLLM's MIT-licensed price table.

Output: {"model": [input, output, cache_read, cache_write, context_window]}, prices in
USD per million tokens, keys normalised the way Pkg.GenAI.normaliseModel looks them up.
"""
import json, re, sys, urllib.request

SRC = "https://raw.githubusercontent.com/BerriAI/litellm/main/model_prices_and_context_window.json"
# First provider wins when two keys normalise to the same model id.
PREFERRED = ["openai", "anthropic", "gemini", "vertex_ai-language-models", "mistral", "deepseek", "xai", "groq", "cohere_chat", "bedrock_converse", "bedrock", "azure"]
MODES = {"chat", "completion", "responses", "embedding"}


def normalise(key):
    k = key.lower().rsplit("/", 1)[-1]
    k = re.sub(r"^(?:[a-z0-9-]+\.)+(?=(?:claude|llama|mistral|nova|titan|command|jamba))", "", k)
    return re.sub(r"[@:].*$", "", re.sub(r"-v\d+:.*$", "", k))


def per_million(v):
    return None if v is None else round(v * 1_000_000, 6)


def main():
    data = json.load(urllib.request.urlopen(SRC))
    rank = lambda kv: PREFERRED.index(kv[1].get("litellm_provider")) if kv[1].get("litellm_provider") in PREFERRED else len(PREFERRED)
    out = {}
    for key, v in sorted(((k, v) for k, v in data.items() if isinstance(v, dict)), key=rank):
        if v.get("mode") not in MODES or v.get("input_cost_per_token") is None:
            continue
        out.setdefault(normalise(key), [
            per_million(v["input_cost_per_token"]),
            per_million(v.get("output_cost_per_token", 0)),
            per_million(v.get("cache_read_input_token_cost")),
            per_million(v.get("cache_creation_input_token_cost")),
            v.get("max_input_tokens"),
        ])
    path = sys.argv[1] if len(sys.argv) > 1 else "static/model_prices.json"
    with open(path, "w") as f:
        json.dump(dict(sorted(out.items())), f, separators=(",", ":"))
        f.write("\n")
    print(f"{len(out)} models -> {path}")


main()
