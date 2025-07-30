#!/usr/bin/env python3
import sys
import re
from ruamel.yaml import YAML
from ruamel.yaml.comments import CommentedMap, CommentedSeq
import pathlib
import copy

def bump_scalars(node, old_tag, new_tag):
    """
    Recursively replace old_tag→new_tag in all string keys and values of a YAML node.
    """
    if isinstance(node, CommentedMap):
        for key, val in list(node.items()):
            new_key = key.replace(old_tag, new_tag) if isinstance(key, str) else key
            if isinstance(val, (CommentedMap, CommentedSeq)):
                bump_scalars(val, old_tag, new_tag)
                new_val = val
            elif isinstance(val, str):
                new_val = val.replace(old_tag, new_tag)
            else:
                new_val = val
            if new_key != key:
                del node[key]
            node[new_key] = new_val
    elif isinstance(node, CommentedSeq):
        for i, item in enumerate(node):
            if isinstance(item, (CommentedMap, CommentedSeq)):
                bump_scalars(item, old_tag, new_tag)
            elif isinstance(item, str):
                node[i] = item.replace(old_tag, new_tag)

def update_index_qmd(prev_tag, new_tag):
    """
    Update internal links in documentation/index.qmd from prev_tag to new_tag.
    Prints each changed link.
    """
    index_qmd = pathlib.Path("documentation/index.qmd")
    if not index_qmd.exists():
        sys.exit(f"❌ `{index_qmd}` not found")

    text = index_qmd.read_text()
    pattern = re.compile(rf"{re.escape(prev_tag)}/[^)\s]+\.qmd")
    matches = pattern.findall(text)
    if matches:
        new_text = text
        for old_link in matches:
            new_link = old_link.replace(f"{prev_tag}/", f"{new_tag}/")
            new_text = new_text.replace(old_link, new_link)
            print(f"✔️  Updated link in `{index_qmd}`: `{old_link}` → `{new_link}`")
        index_qmd.write_text(new_text)
    else:
        print("ℹ️  No links to update in `documentation/index.qmd`.")


def update_sidebar(prev_tag, new_tag):
    """
    Insert a new section for new_tag into _quarto.yml based on the prev_tag block.
    """
    yaml = YAML()
    yaml.preserve_quotes = True
    config_path = pathlib.Path("_quarto.yml")
    doc = yaml.load(config_path)

    # Build human titles
    p, n = prev_tag.lstrip("v"), new_tag.lstrip("v")
    prev_title = f"Version {p[0]}.{p[1:]}"
    new_title = f"Version {n[0]}.{n[1:]}"

    sidebar = doc["website"]["sidebar"]
    for entry in sidebar:
        if entry.get("id") == "Documentation":
            docs = entry
            break
    else:
        sys.exit("❌ Couldn’t find `id: Documentation` under website.sidebar")

    contents = docs["contents"]
    # Find the section to copy
    for idx, sec in enumerate(contents):
        if isinstance(sec, dict) and sec.get("section") == prev_title:
            old_idx = idx
            break
    else:
        sys.exit(f"❌ Couldn’t find a `section: {prev_title}` under Documentation")

    new_sec = copy.deepcopy(contents[old_idx])
    new_sec["section"] = new_title
    bump_scalars(new_sec, prev_tag + "/", new_tag + "/")

    contents.insert(old_idx, new_sec)
    with open(config_path, "w") as f:
        yaml.dump(doc, f)
    print(f"✔️  Inserted `{new_title}` block into `_quarto.yml`")


def main():
    if len(sys.argv) != 3:
        print("Usage: python3 update_index_and_sidebar.py <prev_version_tag> <new_version_tag>")
        sys.exit(1)
    prev_tag, new_tag = sys.argv[1], sys.argv[2]

    update_index_qmd(prev_tag, new_tag)
    update_sidebar(prev_tag, new_tag)

if __name__ == "__main__":
    main()

