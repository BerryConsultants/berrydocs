#!/usr/bin/env python3
import sys
import re
import copy
from ruamel.yaml import YAML

def bump_scalars(node, old_tag, new_tag):
    if isinstance(node, dict):
        for k, v in list(node.items()):
            new_k = k.replace(old_tag, new_tag) if isinstance(k, str) else k
            if new_k is not k:
                node[new_k] = node.pop(k)
                k = new_k
            bump_scalars(v, old_tag, new_tag)
    elif isinstance(node, list):
        for i, v in enumerate(node):
            if isinstance(v, str):
                node[i] = v.replace(old_tag, new_tag)
            else:
                bump_scalars(v, old_tag, new_tag)

def main():
    if len(sys.argv) != 3:
        print("Usage: python3 updateVersion.py <prev_version_tag> <new_version_tag>")
        sys.exit(1)

    prev_tag, new_tag = sys.argv[1], sys.argv[2]
    # derive human titles "Version 7.1" etc.
    raw_prev = prev_tag.lstrip("v")
    raw_new  = new_tag.lstrip("v")
    prev_version = f"{raw_prev[0]}.{raw_prev[1:]}"
    new_version  = f"{raw_new[0]}.{raw_new[1:]}"
    prev_title   = f"Version {prev_version}"
    new_title    = f"Version {new_version}"

    yaml = YAML()
    yaml.preserve_quotes = True
    doc = yaml.load(open("_quarto.yml", "r"))

    # 1️⃣ Find the Documentation block under website → sidebar
    sidebar = doc["website"]["sidebar"]
    for entry in sidebar:
        if entry.get("id") == "Documentation":
            docs_entry = entry
            break
    else:
        sys.exit("❌ Couldn’t find `id: Documentation` under website.sidebar")

    contents = docs_entry["contents"]

    # 2️⃣ Locate the existing previous-version section
    for idx, sec in enumerate(contents):
        if not isinstance(sec, dict):
            continue
        if sec.get("section") == prev_title:
            prev_idx = idx
            break
    else:
        sys.exit(f"❌ Couldn’t find a `section: {prev_title}` under Documentation")

    old_sec = contents[prev_idx]

    # 3️⃣ Deep-copy it, bump title & paths
    new_sec = copy.deepcopy(old_sec)
    new_sec["section"] = new_title
    bump_scalars(new_sec, prev_tag + "/", new_tag + "/")

    # 4️⃣ Insert the new-version block before the old one
    contents.insert(prev_idx, new_sec)

    # 5️⃣ Write back the updated YAML
    with open("_quarto.yml", "w") as f:
        yaml.dump(doc, f)
    print(f"✔️  Inserted `{new_title}` block into `_quarto.yml`")

    # 6️⃣ Finally, regex-patch the QMD file
    import pathlib
    qmd = pathlib.Path("documentation/index.qmd")
    text = qmd.read_text()
    text = re.sub(r"v\d+/", f"{new_tag}/", text)
    qmd.write_text(text)
    print(f"✔️  Updated `documentation/index.qmd` → all paths now use `{new_tag}/`")

if __name__ == "__main__":
    main()

