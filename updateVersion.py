#!/usr/bin/env python3
import sys, re, copy
from ruamel.yaml import YAML
from ruamel.yaml.comments import CommentedMap, CommentedSeq

def bump_scalars(node, old_tag, new_tag):
    """
    Recursively walk a ruamel.yaml node (CommentedMap or CommentedSeq),
    replacing old_tag→new_tag in ALL string keys and values.
    """
    # mapping
    if isinstance(node, CommentedMap):
        for key, val in list(node.items()):
            # 1) fix the key
            new_key = key.replace(old_tag, new_tag) if isinstance(key, str) else key
            # 2) recurse or replace the value
            if isinstance(val, (CommentedMap, CommentedSeq)):
                bump_scalars(val, old_tag, new_tag)
                new_val = val
            elif isinstance(val, str):
                new_val = val.replace(old_tag, new_tag)
            else:
                new_val = val
            # 3) reassign if key changed
            if new_key != key:
                del node[key]
            node[new_key] = new_val

    # sequence
    elif isinstance(node, CommentedSeq):
        for i, item in enumerate(node):
            if isinstance(item, (CommentedMap, CommentedSeq)):
                bump_scalars(item, old_tag, new_tag)
            elif isinstance(item, str):
                node[i] = item.replace(old_tag, new_tag)

def main():
    if len(sys.argv) != 3:
        print("Usage: python3 updateVersion.py <prev_version_tag> <new_version_tag>")
        sys.exit(1)

    prev_tag, new_tag = sys.argv[1], sys.argv[2]
    # build human titles "7.1" from "v71"
    p, n = prev_tag.lstrip("v"), new_tag.lstrip("v")
    prev_title = f"Version {p[0]}.{p[1:]}"
    new_title  = f"Version {n[0]}.{n[1:]}"

    yaml = YAML()
    yaml.preserve_quotes = True
    doc = yaml.load(open("_quarto.yml"))

    # locate the Documentation block in website → sidebar
    sidebar = doc["website"]["sidebar"]
    for entry in sidebar:
        if entry.get("id") == "Documentation":
            docs = entry
            break
    else:
        sys.exit("❌ Couldn’t find `id: Documentation` under website.sidebar")

    cont = docs["contents"]

    # find the old-version section
    for idx, sec in enumerate(cont):
        if isinstance(sec, dict) and sec.get("section") == prev_title:
            old_idx = idx
            break
    else:
        sys.exit(f"❌ Couldn’t find a `section: {prev_title}` under Documentation")

    # copy, relabel, and bump every path in the copy
    new_sec = copy.deepcopy(cont[old_idx])
    new_sec["section"] = new_title
    bump_scalars(new_sec, prev_tag + "/", new_tag + "/")

    # insert the new-version block
    cont.insert(old_idx, new_sec)

    # write back
    with open("_quarto.yml", "w") as f:
        yaml.dump(doc, f)
    print(f"✔️  Inserted `{new_title}` block into `_quarto.yml`")

    # finally, regex-patch the QMD
    import pathlib
    qmd = pathlib.Path("documentation/index.qmd")
    text = qmd.read_text()
    text = re.sub(r"v\d+/", f"{new_tag}/", text)
    qmd.write_text(text)
    print(f"✔️  Updated `documentation/index.qmd` → all paths now use `{new_tag}/`")

if __name__ == "__main__":
    main()

